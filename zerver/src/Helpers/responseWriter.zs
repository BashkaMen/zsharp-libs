// The status line, ready to copy. Anything else is written from the code.
let private status (code: int) : string =
    match code with
    | 200 -> "HTTP/1.1 200 OK\r\n"
    | 400 -> "HTTP/1.1 400 Bad Request\r\n"
    | 404 -> "HTTP/1.1 404 Not Found\r\n"
    | 500 -> "HTTP/1.1 500 Internal Server Error\r\n"
    | 503 -> "HTTP/1.1 503 Service Unavailable\r\n"
    | _ -> ""

let private crlf = "\r\n"
let private colon = ": "
let private contentLength = "content-length: "
let private unknownStatus = " Unknown\r\n"
let private http = "HTTP/1.1 "

let private ensure (out: ref<Buffer<u8>>) (n: int) =
    if n > Buffer.length out.Value then
        out.Value <- Buffer.create n (u8 0)

let private put (buf: Buffer<u8>) (at: int) (bytes: span<u8>) : int =
    let n = Span.length bytes
    Buffer.blit bytes 0 buf at n
    at + n

// Decimal digits of a non-negative n, straight into the buffer.
let private putInt (buf: Buffer<u8>) (at: int) (n: int) : int =
    if n < 0 then
        put buf at (String.bytes (string n))
    else if n = 0 then
        buf.[at] <- u8 48 // '0'
        at + 1
    else
        let mutable digits = 0
        let mutable p = n
        while p > 0 do
            digits <- digits + 1
            p <- p / 10
        let mutable k = at + digits
        let mutable x = n
        while x > 0 do
            k <- k - 1
            buf.[k] <- u8 (48 + x % 10) // '0' + digit
            x <- x / 10
        at + digits

// How many bytes the head takes: status, content-length, and each header.
let private headSize (response: Http.Response) : int =
    let mutable n = 96 + response.body.length
    for (name, value) in response.headers do
        n <- n + String.length name + String.length value + 4
    n

// The head, then the body, in `out` (grown and kept for the next response on this connection).
let private render (out: ref<Buffer<u8>>) (response: Http.Response) : int =
    ensure out (headSize response)
    let buf = out.Value
    let known = status response.code
    let mutable at =
        if known <> "" then
            put buf 0 (String.bytes known)
        else
            put buf (putInt buf (put buf 0 (String.bytes http)) response.code) (String.bytes unknownStatus)
    at <- put buf at (String.bytes contentLength)
    at <- putInt buf at response.body.length
    at <- put buf at (String.bytes crlf)
    for (name, value) in response.headers do
        at <- put buf at (String.bytes name)
        at <- put buf at (String.bytes colon)
        at <- put buf at (String.bytes value)
        at <- put buf at (String.bytes crlf)
    put buf at (String.bytes crlf)

let write (stream: TcpStream) (out: ref<Buffer<u8>>) (response: Http.Response) : Task<result<unit, string>> = task {
    let at = render out response
    let filled = ref at
    let collect (chunk: Buffer<u8>) (n: int) =
        Buffer.blit chunk 0 out.Value filled.Value n
        filled.Value <- filled.Value + n
        Task.ret (Ok ())
    let! collected = response.body.write collect
    match collected with
    | Error e -> return Error e
    | Ok () -> return! Tcp.write stream out.Value filled.Value
}
