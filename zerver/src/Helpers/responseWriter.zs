let private reason (code: int) =
    match code with
    | 200 -> "OK"
    | 400 -> "Bad Request"
    | 404 -> "Not Found"
    | 500 -> "Internal Server Error"
    | 503 -> "Service Unavailable"
    | _ -> "Unknown"

let private headText (response: Http.Response) =
    let headers =
        ("content-length", string response.body.length) :: response.headers
        |> List.map (fun (name, value) -> $"{name}: {value}\r\n")
        |> String.concat
    $"HTTP/1.1 {response.code} {reason response.code}\r\n{headers}\r\n"

let write (stream: TcpStream) (response: Http.Response) : Task<result<unit, string>> = task {
    let head = String.bytes (headText response)
    let size = Span.length head
    let out = Buffer.create (size + response.body.length) (u8 0)
    for i in 0 .. size - 1 do out.[i] <- head.[i]
    let filled = ref size
    let collect (chunk: Buffer<u8>) (n: int) =
        Buffer.blit chunk 0 out filled.Value n
        filled.Value <- filled.Value + n
        Task.ret (Ok ())
    let! collected = response.body.write collect
    match collected with
    | Error e -> return Error e
    | Ok () -> return! Tcp.write stream out filled.Value
}
