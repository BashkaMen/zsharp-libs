// A small HTTP/1.1 server for zhttp's tests, plain and (with a certificate) HTTPS.
open Internal

type private Link = { fd: int; ssl: u64 }

type private Req = {
    method: string
    target: string
    headers: list<string * string>
    body: array<u8>
}

let private pull (link: Link) (buf: Buffer<u8>) : Task<result<int, string>> =
    if link.ssl = u64 0 then Sock.read link.fd buf (Buffer.length buf) None
    else Tls.read link.ssl link.fd buf (Buffer.length buf) None

let private push (link: Link) (data: span<u8>) : Task<result<unit, string>> =
    if Span.length data = 0 then
        Task.ret (Ok ())
    else if link.ssl = u64 0 then
        let buf = Buffer.ofSpan data
        Sock.write link.fd buf (Span.length data) None
    else
        Tls.writeSpan link.ssl link.fd data None

let private findEnd (buf: Buffer<u8>) (from: int) : option<int> =
    let mutable i = from
    let mutable found = -1
    let n = Buffer.length buf
    while found < 0 && i + 3 < n do
        if buf.[i] = u8 13 && buf.[i + 1] = u8 10 && buf.[i + 2] = u8 13 && buf.[i + 3] = u8 10 then
            found <- i
        i <- i + 1
    if found < 0 then None else Some found

let private header (name: string) (headers: list<string * string>) =
    List.tryFind (fun (k, _) -> k = name) headers |> Option.map snd

let private parseHeaders (lines: list<string>) : result<list<string * string>, string> =
    let rec go (lines: list<string>) (acc: list<string * string>) =
        match lines with
        | [] -> Ok (List.rev acc)
        | line :: rest ->
            match String.indexOf ":" line with
            | None -> Error $"malformed header: {line}"
            | Some i ->
                let name = String.toLower (String.sub line 0 i)
                let value = String.trim (String.sub line (i + 1) (String.length line - i - 1))
                go rest ((name, value) :: acc)
    go lines []

let private parseHead (text: string) : result<string * string * list<string * string>, string> =
    match String.split "\r\n" text with
    | request :: lines ->
        match String.split " " request with
        | [ method; target; _ ] ->
            match parseHeaders lines with
            | Ok headers -> Ok (method, target, headers)
            | Error e -> Error e
        | _ -> Error $"malformed request: {request}"
    | _ -> Error "empty request"

let private fill (link: Link) (inbox: Buffer<u8>) : Task<result<int, string>> = task {
    let scratch = Buffer.create 4096 (u8 0)
    let! n = pull link scratch
    match n with
    | Error e -> return Error e
    | Ok 0 -> return Ok 0
    | Ok n ->
        for i in 0 .. n - 1 do
            Buffer.push scratch.[i] inbox
        return Ok n
}

let private take (inbox: Buffer<u8>) (pos: ref<int>) (dest: Buffer<u8>) (want: int) (link: Link) : Task<result<int, string>> =
    let rec go () = task {
        let have = Buffer.length inbox - pos.Value
        if have > 0 then
            let n = min have want
            Buffer.blit (inbox.[pos.Value .. pos.Value + n - 1]) 0 dest 0 n
            pos.Value <- pos.Value + n
            return Ok n
        else
            let! more = fill link inbox
            match more with
            | Error e -> return Error e
            | Ok 0 -> return Ok 0
            | Ok _ -> return! go ()
    }
    if want <= 0 then Task.ret (Ok 0) else go ()

let private readReq (link: Link) (inbox: Buffer<u8>) (pos: ref<int>) : Task<result<option<Req>, string>> =
    let rec head () = task {
        match findEnd inbox pos.Value with
        | Some endAt ->
            let start = pos.Value
            let text = if endAt = start then "" else String.ofBytes (inbox.[start .. endAt - 1])
            pos.Value <- endAt + 4
            match parseHead text with
            | Error e -> return Error e
            | Ok (method, target, headers) ->
                let n =
                    match header "content-length" headers with
                    | None -> 0
                    | Some v -> Option.defaultValue (-1) (String.parseInt (String.trim v))
                if n < 0 then
                    return Error "bad content-length"
                else if n = 0 then
                    return Ok (Some { method = method; target = target; headers = headers; body = ([||] : array<u8>) })
                else
                    let body = Buffer.create n (u8 0)
                    let rec rest (off: int) = task {
                        if off >= n then
                            return Ok (Some { method = method; target = target; headers = headers; body = Buffer.toArray body })
                        else
                            let! got = take inbox pos (body.[off .. n - 1]) (n - off) link
                            match got with
                            | Error e -> return Error e
                            | Ok 0 -> return Error "body closed early"
                            | Ok k -> return! rest (off + k)
                    }
                    return! rest 0
        | None ->
            if Buffer.length inbox - pos.Value > 65536 then
                return Error "request head is too large"
            else
                let! n = fill link inbox
                match n with
                | Error e -> return Error e
                | Ok 0 ->
                    if Buffer.length inbox = pos.Value then return Ok None else return Error "closed during the head"
                | Ok _ -> return! head ()
    }
    head ()

let private pathOf (target: string) =
    match String.indexOf "?" target with
    | None -> target
    | Some i -> String.sub target 0 i

let private send (link: Link) (status: int) (reason: string) (extra: string) (body: span<u8>) (close: bool) : Task<result<bool, string>> = task {
    let conn = if close then "close" else "keep-alive"
    let head = $"HTTP/1.1 {status} {reason}\r\ncontent-length: {Span.length body}\r\nconnection: {conn}\r\n{extra}\r\n"
    let! wrote = push link (String.bytes head)
    match wrote with
    | Error e -> return Error e
    | Ok () ->
        let! bodyWrote = push link body
        match bodyWrote with
        | Error e -> return Error e
        | Ok () -> return Ok (not close)
}

let private answer (link: Link) (n: int) (req: Req) : Task<result<bool, string>> = task {
    match pathOf req.target with
    | "/chunked" ->
        let raw = "HTTP/1.1 200 OK\r\ntransfer-encoding: chunked\r\nconnection: keep-alive\r\n\r\n5\r\nhello\r\n6\r\n world\r\n0\r\nX-Trail: 1\r\n\r\n"
        let! wrote = push link (String.bytes raw)
        match wrote with
        | Error e -> return Error e
        | Ok () -> return Ok true
    | "/bad" ->
        let! _ = push link (String.bytes "not http\r\n\r\n")
        return Ok false
    | "/slow" ->
        do! Task.sleep 5000
        return! send link 200 "OK" "" (String.bytes "slow") false
    | "/close" -> return! send link 200 "OK" "" (String.bytes "closed") true
    | "/count" -> return! send link 200 "OK" "" (String.bytes (string n)) false
    | "/big" -> return! send link 200 "OK" "" (String.bytes (String.repeat 100000 "a")) false
    | "/header" ->
        let value = Option.defaultValue "" (header "x-test" req.headers)
        return! send link 200 "OK" "" (String.bytes value) false
    | "/redirect" -> return! send link 302 "Found" "location: /echo\r\n" (String.bytes "") false
    | "/hop" -> return! send link 302 "Found" "location: /hop2\r\n" (String.bytes "") false
    | "/hop2" -> return! send link 302 "Found" "location: /echo\r\n" (String.bytes "") false
    | path when String.startsWith "/status/" path ->
        let code = Option.defaultValue 500 (String.parseInt (String.sub path 8 (String.length path - 8)))
        return! send link code "Status" "" (String.bytes "status") false
    | _ ->
        let payload = if Array.length req.body = 0 then req.target else String.ofBytes req.body
        let ct = Option.defaultValue "text/plain" (header "content-type" req.headers)
        return! send link 200 "OK" $"content-type: {ct}\r\n" (String.bytes payload) false
}

let private talk (link: Link) : Task<unit> = task {
    let inbox = Buffer.empty
    let pos = ref 0
    let seen = ref 0
    let rec go () = task {
        let! req = readReq link inbox pos
        match req with
        | Error _ | Ok None -> ()
        | Ok (Some req) ->
            seen.Value <- seen.Value + 1
            let! step = answer link seen.Value req
            match step with
            | Ok true -> return! go ()
            | _ -> ()
    }
    do! go ()
    return ()
}

let private serve (fd: int) (ctx: u64) : Task<unit> = task {
    if ctx = u64 0 then
        do! talk { fd = fd; ssl = u64 0 }
        unsafe Native.fdClose fd
    else
        match Tls.server ctx fd with
        | Error _ -> unsafe Native.fdClose fd
        | Ok ssl ->
            let! hs = Tls.handshake ssl fd true None
            match hs with
            | Error _ ->
                Tls.free ssl
                unsafe Native.fdClose fd
            | Ok () ->
                do! talk { fd = fd; ssl = ssl }
                Tls.free ssl
                unsafe Native.fdClose fd
}

let rec private acceptLoop (scope: Scope) (listener: TcpListener) (ctx: u64) : Task<unit> = task {
    let! accepted = Tcp.accept listener
    match accepted with
    | Error _ -> ()
    | Ok stream ->
        let (TcpStream fd) = stream
        Task.start scope (serve fd ctx) |> ignore
        return! acceptLoop scope listener ctx
}

/// Runs `body` with the port of a server on 127.0.0.1. `cert` is a PEM certificate and key for HTTPS.
let run (cert: option<string * string>) (body: int -> Task<unit>) : Task<unit> = task {
    match Tcp.listen "127.0.0.1" 0 with
    | Error e -> return panic $"listen: {e}"
    | Ok listener ->
        let (TcpListener fd) = listener
        let port = unsafe Native.socketPort fd
        if port <= 0 then
            panic $"no port: {port}"
        let ctx =
            match cert with
            | None -> u64 0
            | Some (file, key) ->
                match Tls.serverContext file key with
                | Ok ctx -> ctx
                | Error e -> panic $"server tls: {e}"
        do! Task.scope (fun scope -> task {
            Task.start scope (acceptLoop scope listener ctx) |> ignore
            do! body port
            Task.cancel scope
            return ()
        })
        unsafe Native.fdClose fd
        if ctx <> u64 0 then Tls.freeContext ctx
        return ()
}
