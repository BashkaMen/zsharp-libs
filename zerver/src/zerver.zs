open Common
open Helpers
open Zlog

type HttpHandler = Http.Request -> Task<Http.Response>

type ZerverOptions = {
    host: string = "0.0.0.0"
    port: int
    request_buffer_size: int = 16 * 1024
    max_request_size: int = 10_000_000
    idle_timeout: int = 5000
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
            timer.Value <- Native.loopTimer ms (fun () ->
                if which.Value = 0 then
                    which.Value <- 2
                    Native.loopUnwait fd false
                    k ())
            Native.loopWait fd false (fun () ->
                if which.Value = 0 then
                    which.Value <- 1
                    Native.loopUntimer timer.Value
                    k ())
        let stop () =
            Native.loopUnwait fd false
            Native.loopUntimer timer.Value
        task {
            do! Task.fromCallback start stop
            return which.Value = 2
        }

// One deadline for the whole head: a slow trickle does not reset the idle timeout.
let private fillHead (fd: int) (buf: Buffer<u8>) (timeoutMs: int) : Task<result<option<int * int>, string>> =
    let start = Runtime.ticks ()
    let left () =
        let elapsed = (Runtime.ticks () - start) / u64 1000000
        if elapsed >= u64 timeoutMs then 0 else int (u64 timeoutMs - elapsed)
    let rec go (filled: int) (scanned: int) =
        match RequestParser.headEnd buf scanned filled with
        | Some e -> Task.ret (Ok (Some (e, filled)))
        | None when filled = Buffer.length buf -> Task.ret (Error "request head is larger than the buffer")
        | None ->
            let n = Native.fdRead fd buf filled (Buffer.length buf - filled)
            if n = wouldBlock then
                let ms = left ()
                if ms <= 0 then
                    Task.ret (Ok None)
                else
                    task {
                        let! idle = waitRead fd ms
                        if idle then return Ok None else return! go filled filled
                    }
            else if n < 0 then
                Task.ret (Error (Native.errorText n))
            else if n = 0 && filled = 0 then
                Task.ret (Ok None)
            else if n = 0 then
                Task.ret (Error "connection closed before the request head ended")
            else
                go (filled + n) filled
    go 0 0

let rec private readMore (fd: int) (buf: Buffer<u8>) : Task<result<int, string>> =
    let n = Native.fdRead fd buf 0 (Buffer.length buf)
    if n = wouldBlock then
        task {
            do! Task.waitFor fd false
            return! readMore fd buf
        }
    else if n < 0 then
        Task.ret (Error (Native.errorText n))
    else
        Task.ret (Ok n)

let private badRequest () =
    Http.Response.empty () |> Http.Response.withCode 400 |> Http.Response.withHeader "connection" "close"

// Every request on the connection runs in this one task. The idle timeout is armed only
// while the head read is waiting, not as a race around the whole request.
let private handleRequests (config: ZerverOptions) (handler: HttpHandler) (fd: int) (buffer: Buffer<u8>) (scratch: Buffer<u8>) (noRest: Buffer<u8>) (out: ref<Buffer<u8>>) : Task<result<unit, string>> = task {
    let read = readMore fd
    let mutable go = true
    let mutable failed : option<string> = None
    while go && Option.isNone failed do
        let! head = fillHead fd buffer config.idle_timeout
        match head with
        | Error _ ->
            let! written = ResponseWriter.write fd out (badRequest ())
            match written with
            | Error e -> failed <- Some e
            | Ok () -> go <- false
        | Ok None -> go <- false
        | Ok (Some (e, filled)) ->
            match RequestParser.parse buffer scratch noRest read e filled with
            | Error _ ->
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
