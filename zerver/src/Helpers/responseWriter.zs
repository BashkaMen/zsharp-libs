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

// How many bytes the response takes: status, content-length, each header, and the body.
// String.length counts UTF-8 bytes, the same bytes String.bytes writes. 96 covers the status
// line (at most 39 bytes, for a 20-digit code), content-length (at most 37) and the blank line.
let private responseSize (response: Http.Response) : int =
    let mutable n = 96 + response.body.length
    for (name, value) in response.headers do
        n <- n + String.length name + String.length value + 4
    n

/// The largest buffer a connection keeps between responses. A larger response is rendered
/// into a buffer of its own, freed once written, so a keep-alive connection that once served
/// a large response does not hold that much memory until it closes.
let keptBufferLimit = 64 * 1024

// The head, then the body, in `buf`, which is large enough.
let private render (buf: Buffer<u8>) (response: Http.Response) : int =
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

let private wouldBlock = -1000

// The bytes, in one task: a full write that the socket takes is no further task.
let private writeAll (fd: int) (buf: Buffer<u8>) (n: int) : Task<result<unit, string>> =
    let rec go (off: int) =
        let mutable at = off
        let mutable blocked = false
        let mutable err = None
        while Option.isNone err && not blocked && at < n do
            let w = Native.fdWrite fd buf at (n - at)
            if w = wouldBlock then
                blocked <- true
            else if w < 0 then
                err <- Some (Native.errorText w)
            else
                at <- at + w
        match err with
        | Some e -> Task.ret (Error e)
        | None when at >= n -> Task.ret (Ok ())
        | None ->
            let resume = at
            task {
                do! Task.waitFor fd true
                return! go resume
            }
    go 0

/// Writes the response. `out` is the connection's buffer: grown for responses up to
/// keptBufferLimit and kept for the next one; a larger response gets a buffer of its own.
let write (fd: int) (out: ref<Buffer<u8>>) (response: Http.Response) : Task<result<unit, string>> = task {
    let size = responseSize response
    let buf =
        if size > keptBufferLimit then
            Buffer.create size (u8 0)
        else
            ensure out size
            out.Value
    let at = render buf response
    let filled = ref at
    let limit = at + response.body.length
    let collect (chunk: Buffer<u8>) (n: int) =
        if filled.Value + n > limit then
            Task.ret (Error $"the body wrote more than its length of {response.body.length} bytes")
        else
            Buffer.blit chunk 0 buf filled.Value n
            filled.Value <- filled.Value + n
            Task.ret (Ok ())
    let! collected = response.body.write collect
    match collected with
    | Error e -> return Error e
    | Ok () when filled.Value <> limit ->
        return Error $"the body wrote {filled.Value - at} of its length of {response.body.length} bytes"
    | Ok () -> return! writeAll fd buf filled.Value
}
