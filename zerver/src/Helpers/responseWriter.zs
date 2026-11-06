open TaskResult

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

let write (stream: TcpStream) (response: Http.Response) : Task<result<unit, string>> = taskResult {
    do! Tcp.writeText stream (headText response)
    return! response.body.write (Tcp.write stream)
}
