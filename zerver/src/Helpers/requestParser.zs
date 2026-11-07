open Http

let private cr = u8 13
let private lf = u8 10
let private space = u8 32
let private colonByte = u8 58
let private question = u8 63

// Index of the blank line that ends the head, or None. `from` is how far a previous
// scan already got, so a head that arrives in several reads is not searched from 0 again.
let headEnd (buffer: Buffer<u8>) (from: int) (filled: int) : option<int> =
    let mutable i = if from > 3 then from - 3 else 0
    let mutable found = -1
    while found < 0 && i + 3 < filled do
        if buffer.[i] = cr && buffer.[i + 1] = lf && buffer.[i + 2] = cr && buffer.[i + 3] = lf then
            found <- i
        i <- i + 1
    if found < 0 then None else Some found

let private spanEq (s: span<u8>) (lit: string) : bool =
    let n = String.length lit
    let b = String.bytes lit
    let mutable ok = Span.length s = n
    let mutable i = 0
    while ok && i < n do
        ok <- s.[i] = b.[i]
        i <- i + 1
    ok

let private methodOf (s: span<u8>) : Method =
    if spanEq s "GET" then Get
    else if spanEq s "HEAD" then Head
    else if spanEq s "POST" then Post
    else if spanEq s "PUT" then Put
    else if spanEq s "DELETE" then Delete
    else if spanEq s "OPTIONS" then Options
    else Other (String.ofBytes s)

let private versionOf (s: span<u8>) : result<Version, string> =
    if spanEq s "HTTP/1.1" then Ok Http1_1
    else if spanEq s "HTTP/1.0" then Ok Http1_0
    else Error $"unsupported version: {String.ofBytes s}"

let private findByte (s: span<u8>) (b: u8) : int =
    let n = Span.length s
    let mutable i = 0
    let mutable found = -1
    while found < 0 && i < n do
        if s.[i] = b then found <- i
        i <- i + 1
    found

let private findCrlf (s: span<u8>) (from: int) : int =
    let n = Span.length s
    let mutable i = from
    let mutable found = -1
    while found < 0 && i + 1 < n do
        if s.[i] = cr && s.[i + 1] = lf then found <- i
        i <- i + 1
    found

let private isSpace (b: u8) = b = u8 32 || b = u8 9 || b = u8 10 || b = u8 13

let private edgeSpace (s: span<u8>) : bool =
    let n = Span.length s
    n > 0 && (isSpace s.[0] || isSpace s.[n - 1])

// The header name, ASCII-lowered, copied once. `scratch` is the connection's buffer.
let private lowerName (scratch: Buffer<u8>) (src: span<u8>) : string =
    let n = Span.length src
    if n = 0 then
        ""
    else if n > Buffer.length scratch then
        String.toLower (String.ofBytes src)
    else
        for i in 0 .. n - 1 do
            let c = src.[i]
            scratch.[i] <- if c >= u8 65 && c <= u8 90 then c + u8 32 else c
        let view : span<u8> = scratch
        String.ofBytes view.[0 .. n - 1]

let private parseHeaderLine (scratch: Buffer<u8>) (line: span<u8>) : result<string * string, string> =
    let colonAt = findByte line colonByte
    if colonAt <= 0 then
        Error $"malformed header: {String.ofBytes line}"
    else
        let name = line.[0 .. colonAt - 1]
        if edgeSpace name then
            Error $"whitespace around header name: {String.ofBytes line}"
        else
            let value =
                if colonAt + 1 >= Span.length line then
                    ""
                else
                    String.ofBytes (Span.trim line.[colonAt + 1 .. Span.length line - 1])
            Ok (lowerName scratch name, value)

// The request line is three space-separated tokens. Ends of the slices are exclusive.
// Three fields separated by a single space, same as splitting the line on " ".
let private requestLine (line: span<u8>) : result<span<u8> * span<u8> * span<u8>, string> =
    let n = Span.length line
    let mutable i = 0
    let mutable count = 0
    let mutable more = true
    let mutable a0 = 0
    let mutable a1 = 0
    let mutable b0 = 0
    let mutable b1 = 0
    let mutable c0 = 0
    let mutable c1 = 0
    while more && count < 4 do
        let start = i
        while i < n && line.[i] <> space do
            i <- i + 1
        count <- count + 1
        if count = 1 then
            a0 <- start
            a1 <- i
        else if count = 2 then
            b0 <- start
            b1 <- i
        else if count = 3 then
            c0 <- start
            c1 <- i
        if i < n then
            i <- i + 1
            // a trailing space is an empty field, as "a b ".split does
            if i = n then
                count <- count + 1
                more <- false
        else
            more <- false
    if count <> 3 then
        Error $"malformed request line: {String.ofBytes line}"
    else
        Ok (line.[a0 .. a1 - 1], line.[b0 .. b1 - 1], line.[c0 .. c1 - 1])

let private contentLength (headers: list<string * string>) : result<int, string> =
    let find name = List.tryFind (fun (n, _) -> n = name) headers
    match find "transfer-encoding", find "content-length" with
    | Some _, _ -> Error "transfer-encoding is not supported"
    | None, None -> Ok 0
    | None, Some (_, v) ->
        match String.parseInt v with
        | Some n when n >= 0 -> Ok n
        | _ -> Error $"invalid content-length: {v}"

let private body (read: Buffer<u8> -> Task<result<int, string>>) (rest: Buffer<u8>) (length: int) : Body =
    let delivered = ref 0
    let readBody (dest: Buffer<u8>) : Task<result<int, string>> = task {
        let left = length - delivered.Value
        let fromRest = min left (Buffer.length rest - delivered.Value)
        if left = 0 then
            return Ok 0
        else if fromRest > 0 then
            let n = min fromRest (Buffer.length dest)
            Buffer.blit rest delivered.Value dest 0 n
            delivered.Value <- delivered.Value + n
            return Ok n
        else
            let! n = read dest.[0 .. min left (Buffer.length dest) - 1]
            match n with
            | Error e -> return Error e
            | Ok 0 -> return Error "connection closed before the body ended"
            | Ok n ->
                delivered.Value <- delivered.Value + n
                return Ok n
    }
    { length = length; read = readBody }

let private headersOf (scratch: Buffer<u8>) (head: span<u8>) (from: int) : result<list<string * string>, string> =
    let mutable p = from
    let mutable acc = []
    let mutable err = None
    let n = Span.length head
    while Option.isNone err && p < n do
        let nl = findCrlf head p
        let lineEnd = if nl < 0 then n else nl
        let line = if lineEnd = p then head.[p .. p - 1] else head.[p .. lineEnd - 1]
        match parseHeaderLine scratch line with
        | Error e -> err <- Some e
        | Ok h -> acc <- h :: acc
        p <- if nl < 0 then n else nl + 2
    match err with
    | Some e -> Error e
    | None -> Ok (List.rev acc)

let private splitUrl (url: span<u8>) : string * string =
    let q = findByte url question
    if q < 0 then
        (String.ofBytes url, "")
    else
        let path = if q = 0 then "" else String.ofBytes url.[0 .. q - 1]
        let query = if q + 1 >= Span.length url then "" else String.ofBytes url.[q + 1 .. Span.length url - 1]
        (path, query)

let parse (buffer: Buffer<u8>) (scratch: Buffer<u8>) (noRest: Buffer<u8>) (read: Buffer<u8> -> Task<result<int, string>>) (e: int) (filled: int) : result<Request, string> =
    if e = 0 then
        Error "empty request head"
    else
        let bytes : span<u8> = buffer
        let head = bytes.[0 .. e - 1]
        let nl = findCrlf head 0
        let line = if nl < 0 then head else head.[0 .. nl - 1]
        if Span.length line = 0 then
            Error "empty request head"
        else
            match requestLine line with
            | Error e -> Error e
            | Ok (methodBytes, url, versionBytes) ->
                match versionOf versionBytes with
                | Error e -> Error e
                | Ok version ->
                    let headerFrom = if nl < 0 then Span.length head else nl + 2
                    match headersOf scratch head headerFrom with
                    | Error e -> Error e
                    | Ok headers ->
                        match contentLength headers with
                        | Error e -> Error e
                        | Ok length ->
                            let rest =
                                let start = e + 4
                                if start >= filled then
                                    noRest
                                else
                                    let n = filled - start
                                    let copy = Buffer.create n (u8 0)
                                    Buffer.blit buffer start copy 0 n
                                    copy
                            let (path, query) = splitUrl url
                            Ok { method = methodOf methodBytes; path = path; query = query; version = version; headers = headers; body = body read rest length }

let rec private readHead (buffer: Buffer<u8>) (read: Buffer<u8> -> Task<result<int, string>>) (filled: int) (scanned: int) : Task<result<option<int * int>, string>> = task {
    match headEnd buffer scanned filled with
    | Some e -> return Ok (Some (e, filled))
    | None when filled = Buffer.length buffer -> return Error "request head is larger than the buffer"
    | None ->
        let! n = read buffer.[filled .. Buffer.length buffer - 1]
        match n with
        | Error e -> return Error e
        | Ok 0 when filled = 0 -> return Ok None
        | Ok 0 -> return Error "connection closed before the request head ended"
        | Ok n -> return! readHead buffer read (filled + n) filled
}

let fromStream (buffer: Buffer<u8>) (scratch: Buffer<u8>) (noRest: Buffer<u8>) (read: Buffer<u8> -> Task<result<int, string>>) : Task<result<option<Request>, string>> = task {
    let! found = readHead buffer read 0 0
    match found with
    | Error e -> return Error e
    | Ok None -> return Ok None
    | Ok (Some (e, filled)) ->
        match parse buffer scratch noRest read e filled with
        | Error e -> return Error e
        | Ok request -> return Ok (Some request)
}
