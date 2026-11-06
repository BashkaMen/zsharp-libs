open Http


let private headEnd (buffer: Buffer<u8>) (filled: int) : option<int> =
    let mutable i = 0
    let mutable found = -1
    while found < 0 && i + 3 < filled do
        if buffer.[i] = u8 13 && buffer.[i + 1] = u8 10 && buffer.[i + 2] = u8 13 && buffer.[i + 3] = u8 10 then
            found <- i
        i <- i + 1
    if found < 0 then None else Some found

let rec private readHead (buffer: Buffer<u8>) (read: Buffer<u8> -> Task<result<int, string>>) (filled: int) : Task<result<option<int * int>, string>> = taskResult {
    match headEnd buffer filled with
    | Some e -> return Some (e, filled)
    | None when filled = Buffer.length buffer -> return! TaskResult.ofResult (Error "request head is larger than the buffer")
    | None ->
        let! n = read buffer.[filled .. Buffer.length buffer - 1]
        if n = 0 && filled = 0 then
            return None
        else if n = 0 then
            return! TaskResult.ofResult (Error "connection closed before the request head ended")
        else
            return! readHead buffer read (filled + n)
}

let private parseMethod (s: string) : Method =
    match s with
    | "GET" -> Get
    | "HEAD" -> Head
    | "POST" -> Post
    | "PUT" -> Put
    | "DELETE" -> Delete
    | "OPTIONS" -> Options
    | other -> Other other

let private parseVersion (s: string) : result<Version, string> =
    match s with
    | "HTTP/1.1" -> Ok Http1_1
    | "HTTP/1.0" -> Ok Http1_0
    | other -> Error $"unsupported version: {other}"

let private parseHeader (line: string) : result<string * string, string> =
    match String.indexOf ":" line with
    | None -> Error $"malformed header: {line}"
    | Some 0 -> Error $"malformed header: {line}"
    | Some i ->
        let name = String.sub line 0 i
        if String.trim name <> name then
            Error $"whitespace around header name: {line}"
        else
            Ok (String.toLower name, String.trim (String.sub line (i + 1) (String.length line - i - 1)))

let rec private parseHeaders (lines: list<string>) (acc: list<string * string>) : result<list<string * string>, string> =
    match lines with
    | [] -> Ok (List.rev acc)
    | line :: rest -> result {
        let! h = parseHeader line
        return! parseHeaders rest (h :: acc)
    }

let private parseHead (text: string) : result<Head, string> = result {
    match String.split "\r\n" text with
    | [] -> return! Error "empty request head"
    | requestLine :: headerLines ->
        match String.split " " requestLine with
        | [ m; url; v ] ->
            let! version = parseVersion v
            let! headers = parseHeaders headerLines []
            let head : Head = { method = parseMethod m; url = url; version = version; headers = headers }
            return head
        | _ -> return! Error $"malformed request line: {requestLine}"
}

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
    let readBody (dest: Buffer<u8>) : Task<result<int, BodyReadError>> = taskResult {
        let left = length - delivered.Value
        let fromRest = min left (Buffer.length rest - delivered.Value)
        if left = 0 then
            return 0
        else if fromRest > 0 then
            let n = min fromRest (Buffer.length dest)
            Buffer.blit rest delivered.Value dest 0 n
            delivered.Value <- delivered.Value + n
            return n
        else
            let! n = read dest.[0 .. min left (Buffer.length dest) - 1] |> TaskResult.mapError Unknown
            if n = 0 then
                return! TaskResult.ofResult (Error (Unknown "connection closed before the body ended"))
            else
                delivered.Value <- delivered.Value + n
                return n
    }
    { length = length; read = readBody }

let private splitUrl (url: string) : string * string =
    match String.indexOf "?" url with
    | None -> (url, "")
    | Some i -> (String.sub url 0 i, String.sub url (i + 1) (String.length url - i - 1))

let fromStream (buffer: Buffer<u8>) (read: Buffer<u8> -> Task<result<int, string>>) : Task<result<option<Request>, string>> = taskResult {
    let! found = readHead buffer read 0
    match found with
    | None -> return None
    | Some (e, filled) ->
        let rest = Buffer.ofArray (Buffer.toArray buffer.[e + 4 .. filled - 1])
        let! head = TaskResult.ofResult (parseHead (String.ofBytes buffer.[0 .. e - 1]))
        let! length = TaskResult.ofResult (contentLength head.headers)
        let (path, query) = splitUrl head.url
        let request : Request = { method = head.method; path = path; query = query; version = head.version; headers = head.headers; body = body read rest length }
        return Some request
}
