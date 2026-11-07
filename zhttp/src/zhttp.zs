open Zjson
open Zlog
open Internal

/// A failed request. `Timeout` is the deadline on the client or the request. `Redirect` is a
/// redirect the client refused (too many, or a location it couldn't use).
type HttpError =
    | Dns of string
    | Connect of string
    | Tls of string
    | Timeout
    | Protocol of string
    | Redirect of string

let private describe (e: HttpError) : string =
    match e with
    | Dns s -> $"dns: {s}"
    | Connect s -> $"connect: {s}"
    | Tls s -> $"tls: {s}"
    | Timeout -> "timeout"
    | Protocol s -> $"protocol: {s}"
    | Redirect s -> $"redirect: {s}"

/// What `HttpError` prints as, for a log line.
let message (e: HttpError) : string = describe e

type Method = Get | Head | Post | Put | Patch | Delete | Options | Other of string

/// Where a request's deadline comes from.
type Limit =
    | Inherit
    | Off
    | Millis of int

type Version = Http1_0 | Http1_1

/// A request, independent of any connection. Build one with `Request.get` and the pipes.
type Request = {
    method: Method
    url: string
    headers: list<string * string>
    body: array<u8>
    timeout: Limit
}

/// A body read once, into the caller's buffer. `length` is `Some` for `content-length` and
/// `None` for a chunked body or one that runs until the connection closes. `read` gives the
/// number of bytes copied, and `0` at the end.
type Body = {
    length: option<int>
    read: Buffer<u8> -> Task<result<int, HttpError>>
}

/// A response. The fields don't change; `body.read` moves through the body. `use` closes the
/// connection when the body wasn't finished. Reading to the end returns a keep-alive connection
/// to the pool.
type Response = {
    status: int
    reason: string
    version: Version
    headers: list<string * string>
    body: Body
    close: unit -> unit
}

impl Dispose<Response> =
    let dispose (res: Response) = res.close ()

/// Defaults copied by `Client.withDefaults`. `timeoutMs` is `None` for no deadline.
/// `maxRedirects` is `0` to leave a redirect as the response.
type Defaults = {
    headers: list<string * string>
    timeoutMs: option<int>
    maxRedirects: int
    caFile: option<string>
    verifyCertificates: bool
    maxIdlePerHost: int
}

type private Idle = { key: string; fd: int; ssl: u64 }

/// A long-lived client: its pool and TLS context are shared by every task and thread that holds
/// this value. `withDefaults` returns a different client, with its own pool.
type Client = {
    config: Defaults
    pool: Mutex<list<Idle>>
    tls: Mutex<u64>
}

/// `Request -> Task<result<Response, HttpError>>`. Middleware is a function from one of these to
/// another.
type Send = Request -> Task<result<Response, HttpError>>

let private methodName (m: Method) : string =
    match m with
    | Get -> "GET"
    | Head -> "HEAD"
    | Post -> "POST"
    | Put -> "PUT"
    | Patch -> "PATCH"
    | Delete -> "DELETE"
    | Options -> "OPTIONS"
    | Other s -> s

let private idempotent (m: Method) : bool =
    match m with
    | Get | Head | Options | Put | Delete -> true
    | Post | Patch | Other _ -> false

let private setHeader (name: string) (value: string) (headers: list<string * string>) =
    let name = String.toLower name
    (name, value) :: List.filter (fun (k, _) -> k <> name) headers

let private findHeader (name: string) (headers: list<string * string>) =
    List.tryFind (fun (k, _) -> k = name) headers |> Option.map snd

let private ioError (e: string) : HttpError = if e = "timeout" then Timeout else Connect e

let private tlsError (e: string) : HttpError = if e = "timeout" then Timeout else Tls e

let private deadlineOf (ms: option<int>) : option<u64> =
    match ms with
    | None -> None
    | Some ms when ms <= 0 -> Some (Runtime.ticks ())
    | Some ms -> Some (Runtime.ticks () + u64 ms * u64 1000000)

let private effectiveTimeout (req: Request) (config: Defaults) : option<int> =
    match req.timeout with
    | Inherit -> config.timeoutMs
    | Off -> None
    | Millis ms -> Some ms

let private prepare (config: Defaults) (req: Request) : Request =
    let headers = List.fold (fun acc (k, v) -> setHeader k v acc) config.headers req.headers
    { req with headers = headers }

let private closeIdle (idle: Idle) =
    if idle.ssl <> u64 0 then Tls.free idle.ssl
    Native.fdClose idle.fd

let private takeIdle (pool: Mutex<list<Idle>>) (key: string) : option<Idle> =
    let found = ref None
    Mutex.update pool (fun xs ->
        match List.tryFind (fun (c: Idle) -> c.key = key) xs with
        | None -> xs
        | Some c ->
            found.Value <- Some c
            List.filter (fun (x: Idle) -> x.fd <> c.fd) xs) |> ignore
    found.Value

let private checkin (pool: Mutex<list<Idle>>) (maxPerHost: int) (idle: Idle) =
    let drop = ref false
    Mutex.update pool (fun xs ->
        let n = List.length (List.filter (fun (c: Idle) -> c.key = idle.key) xs)
        if n >= maxPerHost then
            drop.Value <- true
            xs
        else
            idle :: xs) |> ignore
    if drop.Value then closeIdle idle

let private contextOf (client: Client) : result<u64, HttpError> =
    let existing = Mutex.get client.tls
    if existing <> u64 0 then
        Ok existing
    else
        let ca = Option.defaultValue "" client.config.caFile
        match Tls.clientContext ca client.config.verifyCertificates with
        | Error e -> Error (Tls e)
        | Ok ctx ->
            let won = ref ctx
            Mutex.update client.tls (fun old ->
                if old = u64 0 then
                    ctx
                else
                    won.Value <- old
                    old) |> ignore
            if won.Value <> ctx then Tls.freeContext ctx
            Ok won.Value

type private Conn = {
    fd: int
    ssl: u64
    key: string
    reused: bool
    inbox: Buffer<u8>
    pos: ref<int>
}

type private Guard = { run: unit -> unit }

impl Dispose<Guard> =
    let dispose (g: Guard) = g.run ()

let private available (c: Conn) = Buffer.length c.inbox - c.pos.Value

let private compact (c: Conn) =
    if c.pos.Value > 0 && c.pos.Value = Buffer.length c.inbox then
        Buffer.truncate 0 c.inbox
        c.pos.Value <- 0
    else if c.pos.Value > 4096 then
        let n = Buffer.length c.inbox - c.pos.Value
        if n > 0 then
            Buffer.blit (c.inbox.[c.pos.Value .. Buffer.length c.inbox - 1]) 0 c.inbox 0 n
        Buffer.truncate n c.inbox
        c.pos.Value <- 0

let private fill (c: Conn) (until: option<u64>) : Task<result<int, string>> = task {
    compact c
    let scratch = Buffer.create 8192 (u8 0)
    let! n =
        if c.ssl = u64 0 then Sock.read c.fd scratch 8192 until
        else Tls.read c.ssl c.fd scratch 8192 until
    match n with
    | Error e -> return Error e
    | Ok 0 -> return Ok 0
    | Ok n ->
        for i in 0 .. n - 1 do
            Buffer.push scratch.[i] c.inbox
        return Ok n
}

/// Copy up to `want` unread bytes into `dest` (at index 0). `0` means the peer closed.
let private take (c: Conn) (dest: Buffer<u8>) (want: int) (until: option<u64>) : Task<result<int, string>> =
    let want = min want (Buffer.length dest)
    let rec go () = task {
        let have = available c
        if have > 0 then
            let n = min have want
            Buffer.blit (c.inbox.[c.pos.Value .. c.pos.Value + n - 1]) 0 dest 0 n
            c.pos.Value <- c.pos.Value + n
            return Ok n
        else
            let! more = fill c until
            match more with
            | Error e -> return Error e
            | Ok 0 -> return Ok 0
            | Ok _ -> return! go ()
    }
    if want <= 0 then Task.ret (Ok 0) else go ()

let private takeExact (c: Conn) (n: int) (until: option<u64>) : Task<result<Buffer<u8>, string>> =
    let out = Buffer.create n (u8 0)
    let rec go (off: int) = task {
        if off >= n then
            return Ok out
        else
            let! got = take c (out.[off .. n - 1]) (n - off) until
            match got with
            | Error e -> return Error e
            | Ok 0 -> return Error "connection closed before the body ended"
            | Ok k -> return! go (off + k)
    }
    if n = 0 then Task.ret (Ok out) else go 0

let private readLine (c: Conn) (until: option<u64>) : Task<result<option<string>, string>> =
    let acc = Buffer.empty
    let one = Buffer.create 1 (u8 0)
    let rec go () = task {
        if Buffer.length acc > 4096 then
            return Error "line is too long"
        else
            let! n = take c one 1 until
            match n with
            | Error e -> return Error e
            | Ok 0 ->
                if Buffer.length acc = 0 then return Ok None
                else return Error "connection closed before the line ended"
            | Ok _ ->
                let b = one.[0]
                if b = u8 10 then
                    let raw = if Buffer.length acc = 0 then "" else String.ofBytes acc
                    let line = if String.endsWith "\r" raw then String.sub raw 0 (String.length raw - 1) else raw
                    return Ok (Some line)
                else
                    Buffer.push b acc
                    return! go ()
    }
    go ()

let private findHeadEnd (buf: Buffer<u8>) (from: int) : option<int> =
    let mutable i = from
    let mutable found = -1
    let n = Buffer.length buf
    while found < 0 && i + 3 < n do
        if buf.[i] = u8 13 && buf.[i + 1] = u8 10 && buf.[i + 2] = u8 13 && buf.[i + 3] = u8 10 then
            found <- i
        i <- i + 1
    if found < 0 then None else Some found

let private readHead (c: Conn) (until: option<u64>) : Task<result<string, string>> =
    let rec go () = task {
        match findHeadEnd c.inbox c.pos.Value with
        | Some endAt ->
            let start = c.pos.Value
            let text = if endAt = start then "" else String.ofBytes (c.inbox.[start .. endAt - 1])
            c.pos.Value <- endAt + 4
            return Ok text
        | None ->
            if available c > 65536 then
                return Error "response head is larger than 65536 bytes"
            else
                let! n = fill c until
                match n with
                | Error e -> return Error e
                | Ok 0 -> return Error "connection closed before the response head ended"
                | Ok _ -> return! go ()
    }
    go ()

let private writeSpan (c: Conn) (data: span<u8>) (until: option<u64>) : Task<result<unit, string>> =
    if Span.length data = 0 then
        Task.ret (Ok ())
    else if c.ssl = u64 0 then
        let buf = Buffer.ofSpan data
        Sock.write c.fd buf (Span.length data) until
    else
        Tls.writeSpan c.ssl c.fd data until

let private writeArray (c: Conn) (bytes: array<u8>) (until: option<u64>) : Task<result<unit, string>> =
    let rec go (off: int) = task {
        let n = Array.length bytes
        if off >= n then
            return Ok ()
        else
            let m = min 16384 (n - off)
            let! w = writeSpan c (bytes.[off .. off + m - 1]) until
            match w with
            | Error e -> return Error e
            | Ok () -> return! go (off + m)
    }
    go 0

// Name lookup is inside the connect, on this thread. `Dns.lookup` would take the runtime's one
// helper thread (`Task.blocking`), which zlog's `runWith` already holds waiting for log lines,
// so the lookup would never start.
let private numericHost (host: string) =
    String.length host > 0 && List.forall (fun (c: char) -> Char.isDigit c || c = '.' || c = ':') (String.chars host)

let private connectTo (host: string) (port: int) (until: option<u64>) : Task<result<int, HttpError>> = task {
    let fd = Native.tcpConnect host port
    if fd < 0 then
        let text = Native.errorText fd
        // the runtime reports a failed lookup as EADDRNOTAVAIL
        if not (numericHost host) && text = "Cannot assign requested address" then
            return Error (Dns $"no address for {host}")
        else
            return Error (Connect $"connection to {host}:{port} failed: {text}")
    else
        let! ready = Sock.wait fd true until
        match ready with
        | Error "timeout" ->
            Native.fdClose fd
            return Error Timeout
        | Error e ->
            Native.fdClose fd
            return Error (Connect e)
        | Ok () ->
            let err = Native.socketError fd
            if err <> 0 then
                Native.fdClose fd
                return Error (Connect (Native.errorText err))
            else
                return Ok fd
}

let private openConn (client: Client) (target: Parse.Target) (fresh: bool) (until: option<u64>) : Task<result<Conn, HttpError>> =
    let key = Parse.key target
    let rec fromPool () : Task<result<Conn, HttpError>> = task {
        match takeIdle client.pool key with
        | Some idle when Sock.idle idle.fd ->
            return Ok { fd = idle.fd; ssl = idle.ssl; key = key; reused = true; inbox = Buffer.empty; pos = ref 0 }
        | Some idle ->
            closeIdle idle
            return! fromPool ()
        | None -> return! freshConn ()
    }
    and freshConn () : Task<result<Conn, HttpError>> = task {
        let! fd = connectTo target.host target.port until
        match fd with
        | Error e -> return Error e
        | Ok fd ->
            if target.scheme = "http" then
                return Ok { fd = fd; ssl = u64 0; key = key; reused = false; inbox = Buffer.empty; pos = ref 0 }
            else
                match contextOf client with
                | Error e ->
                    Native.fdClose fd
                    return Error e
                | Ok ctx ->
                    match Tls.client ctx fd target.host with
                    | Error e ->
                        Native.fdClose fd
                        return Error (Tls e)
                    | Ok ssl ->
                        let! hs = Tls.handshake ssl fd false until
                        match hs with
                        | Ok () -> return Ok { fd = fd; ssl = ssl; key = key; reused = false; inbox = Buffer.empty; pos = ref 0 }
                        | Error e ->
                            Tls.free ssl
                            Native.fdClose fd
                            return Error (tlsError e)
    }
    if fresh then freshConn () else fromPool ()

let private asIdle (c: Conn) : Idle = { key = c.key; fd = c.fd; ssl = c.ssl }

// chunked phases: 0 size line, 1 data, 2 the CRLF after the data, 3 trailers, 4 done
let private makeBody (client: Client) (conn: Conn) (method: string) (head: Parse.Head) (until: option<u64>) (userClose: bool) : result<Body * (unit -> unit), HttpError> =
    match Parse.bodyKind method head.status head.headers with
    | Error e -> Error (Protocol e)
    | Ok kind ->
        let released = ref false
        let phase = ref 0
        let left = ref (match kind with | Parse.Length n -> n | _ -> 0)
        let keep = not userClose && not (Parse.closes head.version head.headers)
        let release (reuse: bool) =
            if not released.Value then
                released.Value <- true
                if reuse && keep && available conn = 0 then
                    checkin client.pool client.config.maxIdlePerHost (asIdle conn)
                else
                    closeIdle (asIdle conn)
        let framed (e: string) =
            e = "connection closed before the body ended"
            || e = "connection closed before the line ended"
            || e = "connection closed before the chunked body ended"
            || e = "chunk did not end with CRLF"
            || e = "line is too long"
            || e = "chunk size is too large"
            || String.startsWith "invalid chunk" e
            || String.startsWith "empty chunk" e
        let fail (e: string) : HttpError =
            release false
            if e = "timeout" then Timeout else if framed e then Protocol e else Connect e
        let rec readChunked (dest: Buffer<u8>) : Task<result<int, HttpError>> = task {
            match phase.Value with
            | 4 -> return Ok 0
            | 1 ->
                if left.Value = 0 then
                    phase.Value <- 2
                    return! readChunked dest
                else
                    let! n = take conn dest (min (Buffer.length dest) left.Value) until
                    match n with
                    | Error e -> return Error (fail e)
                    | Ok 0 -> return Error (fail "connection closed before the body ended")
                    | Ok n ->
                        left.Value <- left.Value - n
                        return Ok n
            | 2 ->
                let! crlf = takeExact conn 2 until
                match crlf with
                | Error e -> return Error (fail e)
                | Ok buf when buf.[0] = u8 13 && buf.[1] = u8 10 ->
                    phase.Value <- 0
                    return! readChunked dest
                | Ok _ -> return Error (fail "chunk did not end with CRLF")
            | 3 ->
                let! line = readLine conn until
                match line with
                | Error e -> return Error (fail e)
                | Ok None -> return Error (fail "connection closed before the chunked body ended")
                | Ok (Some "") ->
                    phase.Value <- 4
                    release true
                    return Ok 0
                | Ok (Some _) -> return! readChunked dest
            | _ ->
                let! line = readLine conn until
                match line with
                | Error e -> return Error (fail e)
                | Ok None -> return Error (fail "connection closed before the chunked body ended")
                | Ok (Some line) ->
                    match Parse.chunkSize line with
                    | Error e -> return Error (fail e)
                    | Ok 0 ->
                        phase.Value <- 3
                        return! readChunked dest
                    | Ok n ->
                        left.Value <- n
                        phase.Value <- 1
                        return! readChunked dest
        }
        let read (dest: Buffer<u8>) : Task<result<int, HttpError>> = task {
            if released.Value then
                return Ok 0
            else if Buffer.length dest = 0 then
                return Error (Protocol "read buffer has no room")
            else
                match kind with
                | Parse.NoBody ->
                    release true
                    return Ok 0
                | Parse.UntilClose ->
                    let! n = take conn dest (Buffer.length dest) until
                    match n with
                    | Error e -> return Error (fail e)
                    | Ok 0 ->
                        release false
                        return Ok 0
                    | Ok n -> return Ok n
                | Parse.Length _ ->
                    if left.Value = 0 then
                        release true
                        return Ok 0
                    else
                        let! n = take conn dest (min (Buffer.length dest) left.Value) until
                        match n with
                        | Error e -> return Error (fail e)
                        | Ok 0 -> return Error (fail "connection closed before the body ended")
                        | Ok n ->
                            left.Value <- left.Value - n
                            if left.Value = 0 then release true
                            return Ok n
                | Parse.Chunked -> return! readChunked dest
        }
        let length =
            match kind with
            | Parse.Length n -> Some n
            | _ -> None
        Ok ({ length = length; read = read }, fun () -> release false)

let private versionOf (v: string) = if v = "1.0" then Http1_0 else Http1_1

let private responseOf (head: Parse.Head) (body: Body) (close: unit -> unit) : Response = {
    status = head.status
    reason = head.reason
    version = versionOf head.version
    headers = head.headers
    body = body
    close = close
}

let private isRedirect (status: int) =
    status = 301 || status = 302 || status = 303 || status = 307 || status = 308

let private nextRequest (req: Request) (current: Parse.Target) (status: int) (location: string) : result<Request, string> =
    match Parse.resolve current location with
    | Error e -> Error e
    | Ok url ->
        match Parse.parseUrl url with
        | Error e -> Error e
        | Ok target ->
            let method =
                match status with
                | 307 | 308 -> req.method
                | _ ->
                    match req.method with
                    | Get | Head -> req.method
                    | _ -> Get
            let body =
                match status with
                | 307 | 308 -> req.body
                | _ ->
                    match req.method with
                    | Get | Head -> req.body
                    | _ -> ([||] : array<u8>)
            let same =
                String.toLower target.host = String.toLower current.host && target.port = current.port && target.scheme = current.scheme
            let headers =
                let hs =
                    if same then req.headers
                    else List.filter (fun (k, _) -> k <> "authorization" && k <> "cookie" && k <> "proxy-authorization") req.headers
                if Array.length body = 0 then
                    List.filter (fun (k, _) -> k <> "content-type" && k <> "content-length" && k <> "transfer-encoding") hs
                else
                    hs
            Ok { method = method; url = url; headers = headers; body = body; timeout = req.timeout }

let private requestBytes (req: Request) (target: Parse.Target) : result<span<u8> * array<u8> * bool, HttpError> =
    let method = methodName req.method
    if String.contains " " method || String.length method = 0 then
        Error (Protocol $"invalid method: {method}")
    else
        let payload = if method = "HEAD" then ([||] : array<u8>) else req.body
        let userClose =
            match findHeader "connection" req.headers with
            | Some v -> String.contains "close" (String.toLower v)
            | None -> false
        let ua = Option.defaultValue "zhttp/0.1" (findHeader "user-agent" req.headers)
        let enc = Option.defaultValue "identity" (findHeader "accept-encoding" req.headers)
        let hasLength = Array.length payload > 0 || (method <> "GET" && method <> "HEAD")
        let rest =
            List.filter (fun (k, _) ->
                k <> "host" && k <> "user-agent" && k <> "accept-encoding" && k <> "connection" && k <> "content-length" && k <> "transfer-encoding") req.headers
        let headers =
            List.concat [
                [ ("host", Parse.hostHeader target); ("user-agent", ua); ("accept-encoding", enc); ("connection", if userClose then "close" else "keep-alive") ]
                (if hasLength then [ ("content-length", string (Array.length payload)) ] else [])
                rest
            ]
        match Parse.requestHead method target.path headers with
        | Error e -> Error (Protocol e)
        | Ok text -> Ok (String.bytes text, payload, userClose)

let private readResponse (client: Client) (conn: Conn) (method: string) (until: option<u64>) (userClose: bool) : Task<result<Response, HttpError>> =
    let rec heads () = task {
        let! text = readHead conn until
        match text with
        | Error e ->
            let mapped =
                if e = "timeout" then Timeout
                else if String.contains "connection closed before the response head" e || String.contains "response head is larger" e then Protocol e
                else Connect e
            return Error mapped
        | Ok text ->
            match Parse.parseHead text with
            | Error e -> return Error (Protocol e)
            | Ok head when head.status = 101 -> return Error (Protocol "switching protocols is not supported")
            | Ok head when head.status >= 100 && head.status < 200 -> return! heads ()
            | Ok head ->
                match makeBody client conn method head until userClose with
                | Error e -> return Error e
                | Ok (body, close) -> return Ok (responseOf head body close)
    }
    heads ()

let private drainResponse (res: Response) : Task<result<unit, HttpError>> =
    let buf = Buffer.create 8192 (u8 0)
    let rec go () = task {
        let! n = res.body.read buf
        match n with
        | Error e -> return Error e
        | Ok 0 -> return Ok ()
        | Ok _ -> return! go ()
    }
    go ()

let private staleHead (e: HttpError) =
    match e with
    | Protocol s -> String.contains "connection closed before the response head" s
    | Connect _ -> true
    | _ -> false

let rec private exchange (client: Client) (req: Request) (redirectsLeft: int) (fresh: bool) (until: option<u64>) : Task<result<Response, HttpError>> = task {
    match Parse.parseUrl req.url with
    | Error e -> return Error (Protocol e)
    | Ok target ->
        let! opened = openConn client target fresh until
        match opened with
        | Error e -> return Error e
        | Ok conn ->
            let settled = ref false
            use guard = { run = fun () -> if not settled.Value then closeIdle (asIdle conn) }
            match requestBytes req target with
            | Error e -> return Error e
            | Ok (head, payload, userClose) ->
                let retryWrite = conn.reused && not fresh && idempotent req.method
                let! wrote = writeSpan conn head until
                match wrote with
                | Error e when retryWrite && e <> "timeout" ->
                    settled.Value <- true
                    closeIdle (asIdle conn)
                    return! exchange client req redirectsLeft true until
                | Error e -> return Error (ioError e)
                | Ok () ->
                    let! sent = writeArray conn payload until
                    match sent with
                    | Error e when retryWrite && e <> "timeout" ->
                        settled.Value <- true
                        closeIdle (asIdle conn)
                        return! exchange client req redirectsLeft true until
                    | Error e -> return Error (ioError e)
                    | Ok () ->
                        let! response = readResponse client conn (methodName req.method) until userClose
                        match response with
                        | Error e when conn.reused && not fresh && staleHead e && idempotent req.method ->
                            settled.Value <- true
                            closeIdle (asIdle conn)
                            return! exchange client req redirectsLeft true until
                        | Error e -> return Error e
                        | Ok response ->
                            settled.Value <- true
                            let follow = isRedirect response.status && client.config.maxRedirects > 0 && redirectsLeft > 0
                            let stop = isRedirect response.status && client.config.maxRedirects > 0 && redirectsLeft = 0
                            if follow then
                                match Parse.header "location" response.headers with
                                | None -> return Ok response
                                | Some location ->
                                    let! drained = drainResponse response
                                    match drained with
                                    | Error e -> return Error e
                                    | Ok () ->
                                        match nextRequest req target response.status location with
                                        | Error e -> return Error (Redirect e)
                                        | Ok next -> return! exchange client next (redirectsLeft - 1) false until
                            else if stop then
                                response.close ()
                                return Error (Redirect $"stopped after {client.config.maxRedirects} redirects at {req.url}")
                            else
                                return Ok response
}

let private maxBuffered = 10_000_000

let private readToEnd (body: Body) : Task<result<array<u8>, HttpError>> = task {
    match body.length with
    | Some n when n > maxBuffered -> return Error (Protocol $"response body is larger than {maxBuffered} bytes")
    | Some n ->
        let out = Buffer.create n (u8 0)
        let rec go (off: int) = task {
            if off >= n then
                return Ok (Buffer.toArray out)
            else
                let! got = body.read (out.[off .. n - 1])
                match got with
                | Error e -> return Error e
                | Ok 0 -> return Error (Protocol "connection closed before the body ended")
                | Ok k -> return! go (off + k)
        }
        if n = 0 then return Ok (Buffer.toArray out) else return! go 0
    | None ->
        let out = Buffer.empty
        let buf = Buffer.create 8192 (u8 0)
        let rec go () = task {
            if Buffer.length out > maxBuffered then
                return Error (Protocol $"response body is larger than {maxBuffered} bytes")
            else
                let! n = body.read buf
                match n with
                | Error e -> return Error e
                | Ok 0 -> return Ok (Buffer.toArray out)
                | Ok n ->
                    for i in 0 .. n - 1 do
                        Buffer.push buf.[i] out
                    return! go ()
        }
        return! go ()
}

module Request =
    let private make (method: Method) (url: string) : Request = {
        method = method
        url = url
        headers = []
        body = ([||] : array<u8>)
        timeout = Inherit
    }

    /// `GET url`.
    let get (url: string) : Request = make Get url
    /// `HEAD url`.
    let head (url: string) : Request = make Head url
    /// `POST url`, with an empty body until `Request.body`, `text` or `json`.
    let post (url: string) : Request = make Post url
    /// `PUT url`.
    let put (url: string) : Request = make Put url
    /// `PATCH url`.
    let patch (url: string) : Request = make Patch url
    /// `DELETE url`.
    let delete (url: string) : Request = make Delete url
    /// `OPTIONS url`.
    let options (url: string) : Request = make Options url
    /// A method that isn't one of the named ones, sent as `name`.
    let custom (name: string) (url: string) : Request = make (Other name) url

    /// Sets a header, replacing one with the same name. The name is stored in lower case.
    let header (name: string) (value: string) (req: Request) : Request = { req with headers = setHeader name value req.headers }

    /// The body bytes, with no content type of their own.
    let body (bytes: array<u8>) (req: Request) : Request = { req with body = bytes }

    /// The bytes of a span or buffer, copied.
    let bytes (data: span<u8>) (req: Request) : Request = body (Array.ofSpan data) req

    /// `text` as the body, with `content-type: text/plain; charset=utf-8`.
    let text (text: string) (req: Request) : Request =
        req |> header "content-type" "text/plain; charset=utf-8" |> body (Array.ofSpan (String.bytes text))

    /// `value` as JSON (`Serialize`), with `content-type: application/json`.
    let json (value: 'a) (req: Request) : Request =
        let data = toBytes (JsonWriter.create 256) value
        req |> header "content-type" "application/json" |> body (Array.ofSpan data)

    /// This request's deadline, in milliseconds, instead of the client's.
    let timeout (ms: int) (req: Request) : Request = { req with timeout = Millis ms }

    /// No deadline on this request, whatever the client says.
    let noTimeout (req: Request) : Request = { req with timeout = Off }

module Response =
    /// The first header of this name, or `None`. The name is compared in either case.
    let header (name: string) (res: Response) : option<string> = findHeader (String.toLower name) res.headers

    /// Reads the body to its end and returns the bytes. A body over 10 MB is an error; stream
    /// `body.read` for a larger one.
    let bytes (res: Response) : Task<result<array<u8>, HttpError>> = readToEnd res.body

    /// The body as text.
    let text (res: Response) : Task<result<string, HttpError>> = task {
        let! bytes = readToEnd res.body
        match bytes with
        | Error e -> return Error e
        | Ok bytes -> return Ok (String.ofBytes bytes)
    }

    /// The body as `'a` (`Deserialize`).
    let json (res: Response) : Task<result<'a, HttpError>> = task {
        let! bytes = readToEnd res.body
        match bytes with
        | Error e -> return Error e
        | Ok bytes ->
            match (fromBytes bytes : result<'a, string>) with
            | Ok value -> return Ok value
            | Error e -> return Error (Protocol $"json: {e}")
    }

    /// Reads the body and drops it, so a keep-alive connection can be used again.
    let drain (res: Response) : Task<result<unit, HttpError>> = drainResponse res

module Client =
    let private empty : Defaults = {
        headers = []
        timeoutMs = Some 30000
        maxRedirects = 10
        caFile = None
        verifyCertificates = true
        maxIdlePerHost = 8
    }

    /// A client with a 30 second deadline, up to 10 redirects, certificate checks on, and up to
    /// 8 idle connections per host.
    let create () : Client = {
        config = empty
        pool = Mutex.make ([] : list<Idle>)
        tls = Mutex.make (u64 0)
    }

    /// A new client with `change` applied to these defaults, and a new empty pool.
    let withDefaults (change: Defaults -> Defaults) (client: Client) : Client = {
        config = change client.config
        pool = Mutex.make ([] : list<Idle>)
        tls = Mutex.make (u64 0)
    }

    /// `withDefaults` setting the deadline, in milliseconds.
    let withTimeout (ms: int) (client: Client) : Client = withDefaults (fun d -> { d with timeoutMs = Some ms }) client

    /// `withDefaults` setting how many redirects to follow. `0` returns the redirect itself.
    let withMaxRedirects (n: int) (client: Client) : Client = withDefaults (fun d -> { d with maxRedirects = n }) client

    /// `withDefaults` trusting `path`, a PEM file, as well as the system CAs.
    let withCaFile (path: string) (client: Client) : Client = withDefaults (fun d -> { d with caFile = Some path }) client

    /// `withDefaults` with certificate checks off. The connection is still encrypted.
    let withVerification (on: bool) (client: Client) : Client = withDefaults (fun d -> { d with verifyCertificates = on }) client

    /// `withDefaults` adding a header sent on every request that doesn't set the same name.
    let withHeader (name: string) (value: string) (client: Client) : Client =
        withDefaults (fun d -> { d with headers = setHeader name value d.headers }) client

    /// Sends `req`. The client's headers and deadline fill in what the request doesn't set.
    /// A stale keep-alive connection is tried once more.
    let send (client: Client) (req: Request) : Task<result<Response, HttpError>> =
        let req = prepare client.config req
        exchange client req client.config.maxRedirects false (deadlineOf (effectiveTimeout req client.config))

    /// `send` as a `Send`, for middleware: `Client.sender client |> Middleware.log`.
    let sender (client: Client) : Send = fun req -> send client req

module Middleware =
    let private retryable (e: HttpError) =
        match e with
        | Dns _ | Connect _ | Timeout -> true
        | Tls _ | Protocol _ | Redirect _ -> false

    /// Calls `next` again, up to `times` more, when an idempotent method fails with `Dns`,
    /// `Connect` or `Timeout`. `GET`, `HEAD`, `OPTIONS`, `PUT` and `DELETE` are idempotent.
    let retry (times: int) (next: Send) : Send =
        let rec go (left: int) (req: Request) : Task<result<Response, HttpError>> = task {
            let! result = next req
            match result with
            | Error e when left > 0 && idempotent req.method && retryable e ->
                do! Task.sleep (20 * (times - left + 1))
                return! go (left - 1) req
            | other -> return other
        }
        go times

    /// Logs the method, url, status or error, and how long `next` took, through zlog.
    let log (next: Send) : Send =
        fun (req: Request) -> task {
            let sw = Stopwatch.startNew ()
            let! result = next req
            let ms = string sw.ElapsedMilliseconds
            let fields = [ ("method", methodName req.method); ("url", req.url); ("ms", ms) ]
            match result with
            | Ok res ->
                Logger.info (event "http" (("status", string res.status) :: fields))
                return Ok res
            | Error e ->
                Logger.warn (event "http" (("error", describe e) :: fields))
                return Error e
        }

module Http =
    /// The process-wide client `get` and `post` use. It keeps its own pool.
    let defaultClient : Client = Client.create ()

    /// `Client.send` on `defaultClient`.
    let send (req: Request) : Task<result<Response, HttpError>> = Client.send defaultClient req

    /// `GET url` on `defaultClient`.
    let get (url: string) : Task<result<Response, HttpError>> = send (Request.get url)

    /// `POST url` with `body` on `defaultClient`.
    let post (url: string) (body: array<u8>) : Task<result<Response, HttpError>> = send (Request.post url |> Request.body body)
