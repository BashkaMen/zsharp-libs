open Common
open Helpers

type HttpHandler = Http.Request -> Task<Http.Response>

type ZerverOptions = {
    host: string = "0.0.0.0"
    port: int
    request_buffer_size: int = 16 * 1024
    max_request_size: int = 10_000_000
    idle_timeout: int = 5000
    handler: HttpHandler
}

let private keepAlive (request: Http.Request) : bool =
    let connection =
        match List.tryFind (fun (name, _) -> name = "connection") request.headers with
        | Some (_, value) -> String.toLower value
        | None -> ""
    match request.version with
    | Http.Http1_1 -> connection <> "close"
    | Http.Http1_0 -> false

let private bodyError (e: Http.BodyReadError) : string =
    match e with
    | Http.Unknown text -> text

let rec private drain (body: Http.Body) (buffer: Buffer<u8>) : Task<result<unit, string>> = taskResult {
    let! n = body.read buffer |> TaskResult.mapError bodyError
    if n > 0 then
        return! drain body buffer
}

let rec private handleRequests (config: ZerverOptions) (buffer: Buffer<u8>) (stream: TcpStream) : Task<result<unit, string>> = taskResult {
    let! parsed = TaskResult.ofTask (Task.timeout config.idle_timeout (RequestParser.fromStream buffer (Tcp.read stream)))
    match parsed with
    | None -> return ()
    | Some (Ok None) -> return ()
    | Some (Error _) ->
        return! ResponseWriter.write stream (Http.Response.empty |> Http.Response.withCode 400 |> Http.Response.withHeader "connection" "close")
    | Some (Ok (Some request)) ->
        let keep = keepAlive request
        let! response = TaskResult.ofTask (config.handler request)
        let response = if keep then response else response |> Http.Response.withHeader "connection" "close"
        do! ResponseWriter.write stream response
        if keep then
            do! drain request.body buffer
            return! handleRequests config buffer stream
}

let private connection (config: ZerverOptions) (stream: TcpStream) : Task<unit> = task {
    use s = stream
    let! handled = handleRequests config (Buffer.create config.request_buffer_size (u8 0)) s
    match handled with
    | Error e -> eprintln $"connection: {e}"
    | Ok () -> ()
}

let rec private acceptLoop (config: ZerverOptions) (listener: TcpListener) (scope: Scope) : Task<unit> = task {
    let! accepted = Tcp.accept listener
    match accepted with
    | Ok stream -> Task.start scope (connection config stream) |> ignore
    | Error e ->
        eprintln $"accept: {e}"
        do! Task.sleep 100
    return! acceptLoop config listener scope
}

let serve (config: ZerverOptions) : Task<result<unit, string>> = taskResult {
    let! listener = TaskResult.ofResult (Tcp.listen config.host config.port)
    return! TaskResult.ofTask (Task.scope (acceptLoop config listener))
}

let defaultConfig : ZerverOptions = {
    port = 8080;
    handler = fun req -> Task.ret Http.Response.empty
}
