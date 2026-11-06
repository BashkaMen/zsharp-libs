open Common
open Helpers

type HttpHandler = Http.Request -> Task<Http.Response>

type ZerverOptions = {
    host: string = "0.0.0.0"
    port: int
    request_buffer_size: int = 256 * 256
    max_request_size: int = 10_000_000
    max_connections: int = 100
    handler: HttpHandler
}


let private handle_request (config: ZerverOptions) (buffer: Buffer<u8>) (stream: TcpStream) : Task<result<unit, string>> = taskResult {
    let! parsed = TaskResult.ofTask (RequestParser.fromStream buffer (Tcp.read stream))
    // todo keep alive
    match parsed with
    | Error _ -> return! ResponseWriter.write stream { Http.Response.empty with code = 400 }
    | Ok request ->
        let! response = TaskResult.ofTask (config.handler request)
        return! ResponseWriter.write stream response
}

let private connection (config: ZerverOptions) (stream: TcpStream) : Task<unit> = task {
    use s = stream
    let! handled = handle_request config (Buffer.create config.request_buffer_size (u8 0)) s
    match handled with
    | Error e -> eprintln $"connection: {e}"
    | Ok () -> ()
}

let rec private worker (config: ZerverOptions) (ch: Ch<TcpStream>) : Task<unit> = task {
    let! stream = Ch.receive ch
    do! connection config stream
    return! worker config ch
}

let private offer (ch: Ch<TcpStream>) (stream: TcpStream) : Task<bool> =
    Alt.sync (Alt.choose [
        Alt.map (fun () -> true) (Ch.give ch stream)
        Alt.map (fun () -> false) (Alt.always ())
    ])

let private reject (stream: TcpStream) : Task<unit> = task {
    use s = stream
    let! written = ResponseWriter.write s { Http.Response.empty with code = 503 }
    match written with
    | Error e -> eprintln $"reject: {e}"
    | Ok () -> ()
}

let rec private acceptLoop (config: ZerverOptions) (listener: TcpListener) (ch: Ch<TcpStream>) (scope: Scope) : Task<unit> = task {
    let! accepted = Tcp.accept listener
    match accepted with
    | Ok stream ->
        let! taken = offer ch stream
        if not taken then
            Task.start scope (reject stream) |> ignore
    | Error e ->
        eprintln $"accept: {e}"
        do! Task.sleep 100
    return! acceptLoop config listener ch scope
}

let serve (config: ZerverOptions) : Task<result<unit, string>> = taskResult {
    let! listener = TaskResult.ofResult (Tcp.listen config.host config.port)
    let ch = Ch.make ()
    return! TaskResult.ofTask (Task.scope (fun scope -> task {
        for _ in 1 .. config.max_connections do
            Task.start scope (worker config ch) |> ignore
        return! acceptLoop config listener ch scope
    }))
}

let defaultConfig : ZerverOptions = {
    port = 8080;
    handler = fun req -> Task.ret Http.Response.empty
}
