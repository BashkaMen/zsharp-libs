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

let rec private handleRequests (config: ZerverOptions) (handler: HttpHandler) (buffer: Buffer<u8>) (scratch: Buffer<u8>) (noRest: Buffer<u8>) (out: ref<Buffer<u8>>) (stream: TcpStream) : Task<result<unit, string>> = task {
    let! parsed = Task.timeout config.idle_timeout (RequestParser.fromStream buffer scratch noRest (Tcp.read stream))
    match parsed with
    | None -> return Ok ()
    | Some (Ok None) -> return Ok ()
    | Some (Error _) ->
        return! ResponseWriter.write stream out (Http.Response.empty () |> Http.Response.withCode 400 |> Http.Response.withHeader "connection" "close")
    | Some (Ok (Some request)) ->
        let keep = keepAlive request
        let! response = handler request
        let response = if keep then response else response |> Http.Response.withHeader "connection" "close"
        let! written = ResponseWriter.write stream out response
        match written with
        | Error e -> return Error e
        | Ok () when not keep -> return Ok ()
        | Ok () ->
            let! drained = drain request.body buffer
            match drained with
            | Error e -> return Error e
            | Ok () -> return! handleRequests config handler buffer scratch noRest out stream
}

let private connection (config: ZerverOptions) (handler: HttpHandler) (stream: TcpStream) : Task<unit> = task {
    use s = stream
    // One buffer for every response on this connection: the head is written into it in place.
    let out = ref (Buffer.create 4096 (u8 0))
    let scratch = Buffer.create 256 (u8 0)
    let noRest = Buffer.create 0 (u8 0)
    let! handled = handleRequests config handler (Buffer.create config.request_buffer_size (u8 0)) scratch noRest out s
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
