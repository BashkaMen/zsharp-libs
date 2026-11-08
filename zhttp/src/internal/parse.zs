// HTTP/1.1 messages as text. Nothing here touches a socket.
type Target = {
    scheme: string
    host: string
    port: int
    path: string
}

type Head = {
    version: string
    status: int
    reason: string
    headers: list<string * string>
}

let private noCtl (s: string) =
    not (String.contains "\r" s || String.contains "\n" s || String.contains (String.ofByte (u8 0)) s)

let private splitAuthority (rest: string) : string * string =
    let rec go (i: int) =
        if i >= String.length rest then
            (rest, "")
        else
            let b = String.byte rest i
            if b = u8 47 || b = u8 63 then
                (String.sub rest 0 i, String.sub rest i (String.length rest - i))
            else
                go (i + 1)
    go 0

let private hostPort (scheme: string) (authority: string) : result<string * int, string> =
    let def = if scheme = "https" then 443 else 80
    if String.length authority = 0 then
        Error "missing host"
    else if String.contains "@" authority then
        Error "url userinfo is not supported"
    else if String.startsWith "[" authority then
        match String.indexOf "]" authority with
        | None -> Error "malformed ipv6 host"
        | Some endAt ->
            if endAt = 1 then
                Error "missing host"
            else
                let host = String.sub authority 1 (endAt - 1)
                let after = String.sub authority (endAt + 1) (String.length authority - endAt - 1)
                if after = "" then
                    Ok (host, def)
                else if String.startsWith ":" after then
                    match String.parseInt (String.sub after 1 (String.length after - 1)) with
                    | Some p when p > 0 && p <= 65535 -> Ok (host, p)
                    | _ -> Error $"invalid port in {authority}"
                else
                    Error $"malformed host {authority}"
    else
        match String.indexOf ":" authority with
        | None -> Ok (authority, def)
        | Some c ->
            let host = String.sub authority 0 c
            let portText = String.sub authority (c + 1) (String.length authority - c - 1)
            if String.length host = 0 || String.contains ":" portText then
                Error $"malformed host {authority}"
            else
                match String.parseInt portText with
                | Some p when p > 0 && p <= 65535 -> Ok (host, p)
                | _ -> Error $"invalid port in {authority}"

/// `http://host:port/path`, or an error naming what didn't parse. The scheme is lower case.
let parseUrl (url: string) : result<Target, string> =
    if not (noCtl url) || String.contains " " url then
        Error $"unsupported url: {url}"
    else
        match String.indexOf "://" url with
        | None -> Error $"unsupported url: {url}"
        | Some i ->
            let scheme = String.toLower (String.sub url 0 i)
            if scheme <> "http" && scheme <> "https" then
                Error $"unsupported scheme: {scheme}"
            else
                let rest = String.sub url (i + 3) (String.length url - i - 3)
                let (authority, rawPath) = splitAuthority rest
                match hostPort scheme authority with
                | Error e -> Error e
                | Ok (host, port) ->
                    let path =
                        if rawPath = "" then "/"
                        else if String.startsWith "?" rawPath then "/" + rawPath
                        else rawPath
                    Ok { scheme = scheme; host = host; port = port; path = path }

let private bracketed (host: string) = if String.contains ":" host then $"[{host}]" else host

/// `scheme://host` with the port when it isn't the scheme's default, then the path.
let show (t: Target) : string =
    let host = bracketed t.host
    let standard = (t.scheme = "http" && t.port = 80) || (t.scheme = "https" && t.port = 443)
    let origin = if standard then $"{t.scheme}://{host}" else $"{t.scheme}://{host}:{t.port}"
    origin + t.path

/// The value of the `Host` header.
let hostHeader (t: Target) : string =
    let host = bracketed t.host
    let standard = (t.scheme = "http" && t.port = 80) || (t.scheme = "https" && t.port = 443)
    if standard then host else $"{host}:{t.port}"

/// Pool key: scheme, host in lower case, port.
let key (t: Target) : string = $"{t.scheme} {String.toLower t.host} {t.port}"

let private noFragment (s: string) =
    match String.indexOf "#" s with
    | None -> s
    | Some i -> String.sub s 0 i

let private pathOnly (path: string) =
    match String.indexOf "?" path with
    | None -> path
    | Some i -> String.sub path 0 i

let private directory (path: string) =
    let bare = pathOnly path
    match String.lastIndexOf "/" bare with
    | None -> "/"
    | Some i -> String.sub bare 0 (i + 1)

/// `location` relative to the request that got it, as an absolute url. Dot segments are left
/// as they are.
let resolve (base: Target) (location: string) : result<string, string> =
    let location = String.trim (noFragment location)
    if location = "" then
        Error "redirect location is empty"
    else if not (noCtl location) || String.contains " " location then
        Error $"invalid location: {location}"
    else if String.contains "://" location then
        parseUrl location |> Result.map show
    else if String.startsWith "//" location then
        parseUrl (base.scheme + ":" + location) |> Result.map show
    else if String.startsWith "/" location then
        Ok (show { base with path = location })
    else if String.startsWith "?" location then
        Ok (show { base with path = pathOnly base.path + location })
    else
        Ok (show { base with path = directory base.path + location })

let private parseHeader (line: string) : result<string * string, string> =
    match String.indexOf ":" line with
    | None -> Error $"malformed header: {line}"
    | Some 0 -> Error $"malformed header: {line}"
    | Some i ->
        let name = String.toLower (String.sub line 0 i)
        if String.trim name <> name || not (noCtl name) then
            Error $"malformed header: {line}"
        else
            let value = String.trim (String.sub line (i + 1) (String.length line - i - 1))
            if noCtl value then Ok (name, value) else Error $"malformed header: {line}"

let rec private parseHeaders (lines: list<string>) (acc: list<string * string>) : result<list<string * string>, string> =
    match lines with
    | [] -> Ok (List.rev acc)
    | "" :: _ -> Error "empty header line"
    | line :: rest ->
        match parseHeader line with
        | Error e -> Error e
        | Ok h -> parseHeaders rest (h :: acc)

/// The status line and headers, without the blank line that ended them.
let parseHead (text: string) : result<Head, string> =
    match String.split "\r\n" text with
    | [] | [ "" ] -> Error "empty response head"
    | status :: lines ->
        match String.split " " status with
        | version :: code :: rest ->
            let ver =
                match version with
                | "HTTP/1.1" -> Ok "1.1"
                | "HTTP/1.0" -> Ok "1.0"
                | other -> Error $"unsupported version: {other}"
            match ver with
            | Error e -> Error e
            | Ok ver ->
                match String.parseInt code with
                | None -> Error $"malformed status: {status}"
                | Some statusCode ->
                    match parseHeaders lines [] with
                    | Error e -> Error e
                    | Ok headers -> Ok { version = ver; status = statusCode; reason = String.join " " rest; headers = headers }
        | _ -> Error $"malformed status: {status}"

let header (name: string) (headers: list<string * string>) : option<string> =
    let name = String.toLower name
    List.tryFind (fun (k, _) -> k = name) headers |> Option.map snd

let private count (name: string) (headers: list<string * string>) =
    List.length (List.filter (fun (k, _) -> k = name) headers)

type BodyKind =
    | NoBody
    | Length of int
    | Chunked
    | UntilClose

/// How the body is framed. `HEAD`, `1xx`, `204` and `304` have none.
let bodyKind (method: string) (status: int) (headers: list<string * string>) : result<BodyKind, string> =
    if method = "HEAD" || status = 204 || status = 304 || (status >= 100 && status < 200) then
        Ok NoBody
    else if count "content-length" headers > 1 then
        Error "repeated content-length"
    else
        match header "transfer-encoding" headers, header "content-length" headers with
        | Some _, Some _ -> Error "both content-length and transfer-encoding"
        | Some te, None ->
            if String.contains "chunked" (String.toLower te) then Ok Chunked
            else Error $"unsupported transfer-encoding: {te}"
        | None, Some v ->
            match String.parseInt (String.trim v) with
            | Some n when n >= 0 -> Ok (Length n)
            | _ -> Error $"invalid content-length: {v}"
        | None, None -> Ok UntilClose

/// `true` when this response ends the connection.
let closes (version: string) (headers: list<string * string>) : bool =
    match header "connection" headers with
    | Some v ->
        let v = String.toLower v
        if String.contains "close" v then true
        else if version = "1.0" then not (String.contains "keep-alive" v)
        else false
    | None -> version = "1.0"

/// The chunk size at the start of a chunk line (`1a` or `1a;ext=1`), or an error.
let chunkSize (line: string) : result<int, string> =
    let bare =
        match String.indexOf ";" line with
        | Some i -> String.sub line 0 i
        | None -> line
    let hex = String.trim bare
    if String.length hex = 0 then
        Error "empty chunk size"
    else
        let rec go (i: int) (acc: int) : result<int, string> =
            if i >= String.length hex then
                Ok acc
            else
                let b = String.byte hex i
                let v =
                    if b >= u8 48 && b <= u8 57 then int b - 48
                    else if b >= u8 97 && b <= u8 102 then int b - 87
                    else if b >= u8 65 && b <= u8 70 then int b - 55
                    else -1
                if v < 0 then
                    Error $"invalid chunk size: {line}"
                else if acc > 576460752303423487 then
                    Error "chunk size is too large"
                else
                    go (i + 1) (acc * 16 + v)
        go 0 0

let private bad (s: string) = String.contains "\r" s || String.contains "\n" s || String.contains (String.ofByte (u8 0)) s

/// The request head, including the blank line. Header names are sent as given.
let requestHead (method: string) (path: string) (headers: list<string * string>) : result<string, string> =
    if bad method || bad path || List.exists (fun (k, v) -> bad k || bad v) headers then
        Error "a header, method or path contains a line break"
    else
        let lines = List.map (fun (k, v) -> $"{k}: {v}\r\n") headers |> String.concat
        Ok $"{method} {path} HTTP/1.1\r\n{lines}\r\n"
