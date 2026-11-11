open Common
open Helpers
open Zlog

type HttpHandler = Http.Request -> Task<Http.Response>

type ZerverOptions = {
    host: string = "0.0.0.0"
    port: int
    request_buffer_size: int = 16 * 1024
    max_request_size: int = 10_000_000
    /// How long, in ms, a connection may take to send a request head (the whole head, from
    /// when zerver starts waiting for it); <= 0 waits without a limit.
    idle_timeout: int = 5000
    /// How long, in ms, a read of a request body may wait for bytes; <= 0 waits without a limit.
    body_timeout: int = 30000
    workers: int = Parallel.workers ()
}

let private keepAlive (request: Http.Request) : bool =
    let connection =
        match List.tryFind (fun (name, _) -> name = "connection") request.headers with
        | Some (_, value) -> String.toLower value
        | None -> ""
    match request.version with
    | Http.Http1_1 -> connection <> "close"
    | Http.Http1_0 -> false

let rec private drain (body: Http.Body) (buffer: Buffer<u8>) : Task<result<unit, string>> = task {
    let! n = body.read buffer
    match n with
    | Error e -> return Error e
    | Ok n when n > 0 -> return! drain body buffer
    | Ok _ -> return Ok ()
}

let private wouldBlock = -1000

// `true` when `ms` passes with the socket still not readable. `ms` <= 0 waits without a deadline.
let private waitRead (fd: int) (ms: int) : Task<bool> =
    if ms <= 0 then
        task {
            do! Task.waitFor fd false
            return false
        }
    else
        let timer = ref 0
        let which = ref 0
        let start (k: unit -> unit) =
            timer.Value <- unsafe Native.loopTimer ms (fun () ->
                if which.Value = 0 then
                    which.Value <- 2
                    unsafe Native.loopUnwait fd false
                    k ())
            unsafe Native.loopWait fd false (fun () ->
                if which.Value = 0 then
                    which.Value <- 1
                    unsafe Native.loopUntimer timer.Value
                    k ())
        let stop () =
            unsafe Native.loopUnwait fd false
            unsafe Native.loopUntimer timer.Value
        task {
            do! Task.fromCallback start stop
            return which.Value = 2
        }

// How reading a request head ended.
type private HeadRead =
    /// The head ends at the index; the buffer is filled up to the second number.
    | HeadAt of int * int
    /// Nothing to answer: the peer closed between requests, or the idle timeout passed.
    | Done
    /// The head can't be read whole (it doesn't fit the buffer, or the peer stopped sending
    /// mid-head): answered with 400, then closed.
    | Unreadable of string
    /// The socket failed (a reset, say): nothing can be answered.
    | Gone of string

// One deadline for the whole head: a slow trickle does not reset the idle timeout.
// A timeout <= 0 waits without one.
let private fillHead (fd: int) (buf: Buffer<u8>) (timeoutMs: int) : Task<HeadRead> =
    let start = Runtime.ticks ()
    let left () =
        let elapsed = (Runtime.ticks () - start) / u64 1000000
        if elapsed >= u64 timeoutMs then 0 else int (u64 timeoutMs - elapsed)
    let rec go (filled: int) (scanned: int) =
        match RequestParser.headEnd buf scanned filled with
        | Some e -> Task.ret (HeadAt (e, filled))
        | None when filled = Buffer.length buf -> Task.ret (Unreadable "request head is larger than the buffer")
        | None ->
            let n = unsafe Native.fdRead fd buf filled (Buffer.length buf - filled)
            if n = wouldBlock then
                // waitRead takes 0 as no deadline
                let ms = if timeoutMs <= 0 then 0 else left ()
                if timeoutMs > 0 && ms <= 0 then
                    Task.ret Done
                else
                    task {
                        let! idle = waitRead fd ms
                        if idle then return Done else return! go filled filled
                    }
            else if n < 0 then
                Task.ret (Gone (unsafe Native.errorText n))
            else if n = 0 && filled = 0 then
                Task.ret Done
            else if n = 0 then
                Task.ret (Unreadable "connection closed before the request head ended")
            else
                go (filled + n) filled
    go 0 0

// A body read: waits up to `timeoutMs` for bytes (<= 0: without a limit).
let rec private readMore (fd: int) (timeoutMs: int) (buf: Buffer<u8>) : Task<result<int, string>> =
    let n = unsafe Native.fdRead fd buf 0 (Buffer.length buf)
    if n = wouldBlock then
        task {
            let! idle = waitRead fd timeoutMs
            if idle then
                return Error $"no request body bytes for {timeoutMs} ms"
            else
                return! readMore fd timeoutMs buf
        }
    else if n < 0 then
        Task.ret (Error (unsafe Native.errorText n))
    else
        Task.ret (Ok n)

let private badRequest () =
    Http.Response.empty () |> Http.Response.withCode 400 |> Http.Response.withHeader "connection" "close"

// Every request on the connection runs in this one task. The idle timeout is armed only
// while the head read is waiting, not as a race around the whole request.
let private handleRequests (config: ZerverOptions) (handler: HttpHandler) (fd: int) (buffer: Buffer<u8>) (scratch: Buffer<u8>) (noRest: Buffer<u8>) (out: ref<Buffer<u8>>) : Task<result<unit, string>> = task {
    let read = readMore fd config.body_timeout
    let mutable go = true
    let mutable failed : option<string> = None
    while go && Option.isNone failed do
        let! head = fillHead fd buffer config.idle_timeout
        match head with
        | Done -> go <- false
        | Gone e ->
            Logger.debug (event "connection gone" [ ("error", e) ])
            go <- false
        | Unreadable reason ->
            Logger.debug (event "bad request" [ ("error", reason) ])
            let! written = ResponseWriter.write fd out (badRequest ())
            match written with
            | Error e -> failed <- Some e
            | Ok () -> go <- false
        | HeadAt (e, filled) ->
            match RequestParser.parse buffer scratch noRest read e filled with
            | Error reason ->
                Logger.debug (event "bad request" [ ("error", reason) ])
                let! written = ResponseWriter.write fd out (badRequest ())
                match written with
                | Error err -> failed <- Some err
                | Ok () -> go <- false
            | Ok request ->
                let keep = keepAlive request
                let! response = handler request
                let response = if keep then response else response |> Http.Response.withHeader "connection" "close"
                let! written = ResponseWriter.write fd out response
                match written with
                | Error err -> failed <- Some err
                | Ok () when not keep -> go <- false
                | Ok () when request.body.length = 0 -> ()
                | Ok () ->
                    let! drained = drain request.body buffer
                    match drained with
                    | Error err -> failed <- Some err
                    | Ok () -> ()
    match failed with
    | Some e -> return Error e
    | None -> return Ok ()
}

let private connection (config: ZerverOptions) (handler: HttpHandler) (stream: TcpStream) : Task<unit> = task {
    use s = stream
    let (TcpStream fd) = s
    // One buffer for every response on this connection: the head is written into it in place.
    // `scratch` lowers header names; `noRest` is the empty body prefix when the read ended on the head.
    let out = ref (Buffer.create 4096 (u8 0))
    let scratch = Buffer.create 256 (u8 0)
    let noRest = Buffer.create 0 (u8 0)
    let! handled = handleRequests config handler fd (Buffer.create config.request_buffer_size (u8 0)) scratch noRest out
    match handled with
    | Error e -> Logger.warn (event "connection failed" [ ("error", e) ])
    | Ok () -> ()
}

let rec private acceptLoop (config: ZerverOptions) (handler: HttpHandler) (listener: TcpListener) (scope: Scope) : Task<unit> = task {
    let! accepted = Tcp.accept listener
    match accepted with
    | Ok stream -> Task.start scope (connection config handler stream) |> ignore
    | Error e ->
        Logger.error (event "accept failed" [ ("error", e) ])
        do! Task.sleep 100
    return! acceptLoop config handler listener scope
}

let serve (config: ZerverOptions) (handler: share<HttpHandler>) : Task<result<unit, string>> =
    match Tcp.listen config.host config.port with
    | Error e -> Task.ret (Error e)
    | Ok listener ->
        let workers = max 1 config.workers
        Logger.info (event "listening" [ ("host", config.host); ("port", string config.port); ("workers", string workers) ])
        let loops = List.init workers (fun _ -> Task.spawn (fun () -> Task.scope (acceptLoop config handler listener)))
        Task.map (fun _ -> Ok ()) (Task.all loops)

let defaultConfig : ZerverOptions = {
    port = 8080
}
