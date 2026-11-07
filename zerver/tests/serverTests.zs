open Testing
open Zerver
open Support

let rec private readBody (body: Http.Body) (buf: Buffer<u8>) (chunks: list<string>) : Task<result<string, string>> = task {
    let! n = body.read buf
    match n with
    | Error e -> return Error e
    | Ok 0 -> return Ok (String.concat (List.rev chunks))
    | Ok n -> return! readBody body buf (textOf buf n :: chunks)
}

let private methodName (m: Http.Method) =
    match m with
    | Http.Get -> "GET"
    | Http.Head -> "HEAD"
    | Http.Post -> "POST"
    | Http.Put -> "PUT"
    | Http.Delete -> "DELETE"
    | Http.Options -> "OPTIONS"
    | Http.Other s -> s

let private reply (code: int) (text: string) =
    Http.Response.empty () |> Http.Response.withCode code |> Http.Response.withBody (Http.ResponseBody.fromText text)

// The routes the tests use.
let handler (req: Http.Request) : Task<Http.Response> = task {
    match req.path with
    | "/echo" ->
        let! body = readBody req.body (Buffer.create 16384 (u8 0)) []
        match body with
        | Ok text -> return reply 200 text
        | Error e -> return reply 500 e
    | "/info" ->
        let version = if req.version = Http.Http1_1 then "1.1" else "1.0"
        let headers = req.headers |> List.map (fun (n, v) -> $"{n}={v}") |> String.join ";"
        return reply 200 $"{methodName req.method}|{req.path}|{req.query}|{version}|{headers}"
    | "/ignore-body" -> return reply 200 "ignored"
    | "/read-two" ->
        let buf = Buffer.create 2 (u8 0)
        let! n = req.body.read buf
        match n with
        | Ok n -> return reply 200 (textOf buf n)
        | Error e -> return reply 500 e
    | "/big" -> return reply 200 (repeat 20000 "0123456789")
    | "/missing" -> return reply 404 "nope" |> Http.Response.withHeader "x-why" "missing"
    | "/slow" ->
        do! Task.sleep 400
        return reply 200 "slow"
    | _ -> return reply 200 "root"
}

let private config (port: int) : ZerverOptions = { port = port; host = "127.0.0.1"; workers = 1; idle_timeout = 300 }

let private t (name: string) body = testWithTimeout 15000 name body

// One connection: `raw` sent, then everything until zerver closes, as replies.
let private exchangeWith (cfg: ZerverOptions) (raw: string) : Task<result<list<Reply>, string>> =
    withServer cfg handler (fun () -> task {
        let! c = connect cfg.port
        match c with
        | Error e -> return Error e
        | Ok s ->
            use s = s
            let! sent = send s raw
            match sent with
            | Error e -> return Error e
            | Ok () ->
                let! back = readToEnd 5000 s
                return back |> Result.bind replies
    })

let private exchange (port: int) (raw: string) = exchangeWith (config port) raw

let private verify (got: result<'a, string>) (ok: 'a -> bool) : result<unit, string> =
    match got with
    | Ok x when ok x -> Ok ()
    | Ok x -> Error $"unexpected: {show x}"
    | Error e -> Error e

let private one (status: string) (body: string) (rs: list<Reply>) =
    match rs with
    | [ r ] -> r.status = status && r.body = body
    | _ -> false

let private closes (rs: list<Reply>) =
    List.forall (fun r -> header "connection" r = Some "close") rs

let private isBadRequest (rs: list<Reply>) =
    match rs with
    | [ r ] -> r.status = "HTTP/1.1 400 Bad Request" && header "connection" r = Some "close" && r.body = ""
    | _ -> false

let private expectExchange (port: int) (raw: string) (ok: list<Reply> -> bool) = task {
    let! r = exchange port raw
    return verify r ok
}

let basicTests = testList "requests" [
    t "GET is answered" (fun () ->
        expectExchange 39301 "GET / HTTP/1.1\r\nconnection: close\r\n\r\n" (one "HTTP/1.1 200 OK" "root"))
    t "the handler sees method, path, query, version and headers" (fun () ->
        expectExchange 39302 "PATCH /info?a=1&b HTTP/1.1\r\nHost: x\r\nX-Id: 7\r\nconnection: close\r\n\r\n" (one "HTTP/1.1 200 OK" "PATCH|/info|a=1&b|1.1|host=x;x-id=7;connection=close"))
    t "POST echoes the body" (fun () ->
        expectExchange 39303 "POST /echo HTTP/1.1\r\ncontent-length: 5\r\nconnection: close\r\n\r\nhello" (one "HTTP/1.1 200 OK" "hello"))
    t "a 404 with a handler's header" (fun () ->
        expectExchange 39304 "GET /missing HTTP/1.1\r\nconnection: close\r\n\r\n" (fun rs ->
            one "HTTP/1.1 404 Not Found" "nope" rs && header "x-why" (List.head rs) = Some "missing"))
    t "a response larger than the starting buffer" (fun () ->
        expectExchange 39305 "GET /big HTTP/1.1\r\nconnection: close\r\n\r\n" (one "HTTP/1.1 200 OK" (repeat 20000 "0123456789")))
    t "a 1 MB body is echoed whole" (fun () ->
        let body = repeat 65536 "0123456789abcdef"
        expectExchange 39306 $"POST /echo HTTP/1.1\r\ncontent-length: {String.length body}\r\nconnection: close\r\n\r\n{body}" (one "HTTP/1.1 200 OK" body))
    t "the content-length matches the body" (fun () ->
        expectExchange 39307 "POST /echo HTTP/1.1\r\ncontent-length: 3\r\nconnection: close\r\n\r\nabc" (fun rs ->
            List.map (header "content-length") rs = [ Some "3" ]))
]

let closeTests = testList "connection close" [
    t "connection: close is answered with connection: close, then closed" (fun () ->
        expectExchange 39311 "GET / HTTP/1.1\r\nconnection: close\r\n\r\n" closes)
    t "connection: CLOSE in any case closes" (fun () ->
        expectExchange 39312 "GET / HTTP/1.1\r\nConnection: CLOSE\r\n\r\n" (fun rs -> List.length rs = 1 && closes rs))
    t "HTTP/1.0 closes after one response" (fun () ->
        expectExchange 39313 "GET / HTTP/1.0\r\n\r\nGET / HTTP/1.0\r\n\r\n" (fun rs -> List.length rs = 1 && closes rs))
    t "HTTP/1.0 with keep-alive still closes (unsupported)" (fun () ->
        expectExchange 39314 "GET / HTTP/1.0\r\nconnection: keep-alive\r\n\r\n" (fun rs -> List.length rs = 1 && closes rs))
    t "requests after connection: close are not answered" (fun () ->
        withServer (config 39315) handler (fun () -> task {
            let! c = connect 39315
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                let! _ = send s "GET / HTTP/1.1\r\nconnection: close\r\n\r\n"
                let! first = readToEnd 3000 s
                let! _ = send s "GET / HTTP/1.1\r\n\r\n"
                return verify (first |> Result.bind replies) (fun rs -> List.length rs = 1)
        }))
    t "a client that connects and closes leaves zerver serving" (fun () ->
        withServer (config 39316) handler (fun () -> task {
            let! first = connect 39316
            match first with
            | Error e -> return Error e
            | Ok s ->
                Native.fdClose (let (TcpStream fd) = s in fd)
                let! c = connect 39316
                match c with
                | Error e -> return Error e
                | Ok s ->
                    use s = s
                    let! _ = send s "GET / HTTP/1.1\r\nconnection: close\r\n\r\n"
                    let! back = readToEnd 3000 s
                    return verify (back |> Result.bind replies) (one "HTTP/1.1 200 OK" "root")
        }))
]

let badRequestTests = testList "bad requests" [
    t "a malformed request line is 400, then closed" (fun () ->
        expectExchange 39321 "NOT A VALID REQUEST LINE\r\n\r\n" isBadRequest)
    t "an unsupported version is 400" (fun () ->
        expectExchange 39322 "GET / HTTP/2.0\r\n\r\n" isBadRequest)
    t "a malformed header is 400" (fun () ->
        expectExchange 39323 "GET / HTTP/1.1\r\nno colon here\r\n\r\n" isBadRequest)
    t "whitespace before a header's colon is 400" (fun () ->
        expectExchange 39324 "GET / HTTP/1.1\r\nhost : x\r\n\r\n" isBadRequest)
    t "an invalid content-length is 400" (fun () ->
        expectExchange 39325 "POST /echo HTTP/1.1\r\ncontent-length: lots\r\n\r\n" isBadRequest)
    t "transfer-encoding is 400" (fun () ->
        expectExchange 39326 "POST /echo HTTP/1.1\r\ntransfer-encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n" isBadRequest)
    t "an empty request head is 400" (fun () ->
        expectExchange 39327 "\r\n\r\n" isBadRequest)
    // zerver closes with the rest of the head unread, so the kernel sends a reset after the
    // 400; read only up to the end of the 400 (RFC 9112 9.6 asks for a lingering close)
    t "a head larger than the request buffer is 400" (fun () ->
        withServer { config 39328 with request_buffer_size = 256 } handler (fun () -> task {
            let! c = connect 39328
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                let! _ = send s $"GET / HTTP/1.1\r\nx-big: {repeat 300 "a"}\r\n\r\n"
                let! back = readUntil 3000 (String.contains "\r\n\r\n") s
                return verify (back |> Result.bind replies) isBadRequest
        }))
    t "a head that just fits the request buffer is served" (fun () -> task {
        let cfg = { config 39329 with request_buffer_size = 256 }
        let fixedPart = "GET / HTTP/1.1\r\nconnection: close\r\nx: \r\n\r\n"
        let! r = exchangeWith cfg $"GET / HTTP/1.1\r\nconnection: close\r\nx: {repeat (256 - String.length fixedPart) "a"}\r\n\r\n"
        return verify r (one "HTTP/1.1 200 OK" "root")
    })
    t "a bad request after a good one on the same connection" (fun () ->
        withServer (config 39330) handler (fun () -> task {
            let! c = connect 39330
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                let! _ = send s "GET / HTTP/1.1\r\n\r\n"
                let! first = readUntil 3000 (fun acc -> String.contains "root" acc) s
                let! _ = send s "BAD\r\n\r\n"
                let! rest = readToEnd 3000 s
                let all = Result.bind (fun a -> rest |> Result.map (fun b -> a + b)) first
                return verify (all |> Result.bind replies) (fun rs ->
                    match rs with
                    | [ a; b ] -> a.body = "root" && isBadRequest [ b ]
                    | _ -> false)
        }))
    t "a client gone mid-head leaves zerver serving" (fun () ->
        withServer (config 39331) handler (fun () -> task {
            let! first = connect 39331
            match first with
            | Error e -> return Error e
            | Ok s ->
                let! _ = send s "GET / HTT"
                Native.fdClose (let (TcpStream fd) = s in fd)
                let! c = connect 39331
                match c with
                | Error e -> return Error e
                | Ok s ->
                    use s = s
                    let! _ = send s "GET / HTTP/1.1\r\nconnection: close\r\n\r\n"
                    let! back = readToEnd 3000 s
                    return verify (back |> Result.bind replies) (one "HTTP/1.1 200 OK" "root")
        }))
    t "a client gone mid-body leaves zerver serving" (fun () ->
        withServer (config 39332) handler (fun () -> task {
            let! first = connect 39332
            match first with
            | Error e -> return Error e
            | Ok s ->
                let! _ = send s "POST /echo HTTP/1.1\r\ncontent-length: 100\r\n\r\nonly a part"
                do! Task.sleep 50
                Native.fdClose (let (TcpStream fd) = s in fd)
                let! c = connect 39332
                match c with
                | Error e -> return Error e
                | Ok s ->
                    use s = s
                    let! _ = send s "GET / HTTP/1.1\r\nconnection: close\r\n\r\n"
                    let! back = readToEnd 3000 s
                    return verify (back |> Result.bind replies) (one "HTTP/1.1 200 OK" "root")
        }))
]

// Sends each request on one connection, waiting for each answer before the next.
let private sequential (port: int) (requests: list<string>) : Task<result<list<Reply>, string>> =
    withServer (config port) handler (fun () -> task {
        let! c = connect port
        match c with
        | Error e -> return Error e
        | Ok s ->
            use s = s
            let acc = ref ""
            let mutable failed = None
            let mutable n = 0
            for r in requests do
                if Option.isNone failed then
                    n <- n + 1
                    let want = n
                    let! _ = send s r
                    let! back = readUntil 3000 (fun got -> match replies (acc.Value + got) with | Ok rs -> List.length rs >= want | Error _ -> false) s
                    match back with
                    | Ok got -> acc.Value <- acc.Value + got
                    | Error e -> failed <- Some e
            match failed with
            | Some e -> return Error $"{e}; so far {acc.Value}"
            | None -> return replies acc.Value
    })

let keepAliveTests = testList "keep-alive" [
    t "several requests on one connection" (fun () -> task {
        let! r = sequential 39341 [ "GET / HTTP/1.1\r\n\r\n"; "POST /echo HTTP/1.1\r\ncontent-length: 3\r\n\r\nabc"; "GET /info?x HTTP/1.1\r\n\r\n" ]
        return verify r (fun rs -> List.map (fun r -> r.body) rs = [ "root"; "abc"; "GET|/info|x|1.1|" ])
    })
    t "a kept connection gets no connection: close" (fun () -> task {
        let! r = sequential 39342 [ "GET / HTTP/1.1\r\n\r\n" ]
        return verify r (fun rs -> List.forall (fun r -> header "connection" r = None) rs)
    })
    t "a hundred requests on one connection" (fun () -> task {
        let requests = List.init 100 (fun i -> $"POST /echo HTTP/1.1\r\ncontent-length: {String.length (string i)}\r\n\r\n{i}")
        let! r = sequential 39343 requests
        return verify r (fun rs -> List.map (fun r -> r.body) rs = List.init 100 string)
    })
    t "a large response, then a small one, on one connection" (fun () -> task {
        let! r = sequential 39344 [ "GET /big HTTP/1.1\r\n\r\n"; "GET /missing HTTP/1.1\r\n\r\n" ]
        return verify r (fun rs ->
            match rs with
            | [ a; b ] -> String.length a.body = 200000 && b.status = "HTTP/1.1 404 Not Found" && b.body = "nope"
            | _ -> false)
    })
    t "an unread body is drained before the next request" (fun () -> task {
        let! r = sequential 39345 [ "POST /ignore-body HTTP/1.1\r\ncontent-length: 10\r\n\r\nGET /x HTT"; "GET /info HTTP/1.1\r\n\r\n" ]
        return verify r (fun rs -> List.map (fun r -> r.body) rs = [ "ignored"; "GET|/info||1.1|" ])
    })
    t "a partly read body is drained before the next request" (fun () -> task {
        let! r = sequential 39346 [ "POST /read-two HTTP/1.1\r\ncontent-length: 6\r\n\r\nabcdef"; "GET /info HTTP/1.1\r\n\r\n" ]
        return verify r (fun rs -> List.map (fun r -> r.body) rs = [ "ab"; "GET|/info||1.1|" ])
    })
    t "an unread large body is drained" (fun () -> task {
        let body = repeat 50000 "z"
        let! r = sequential 39347 [ $"POST /ignore-body HTTP/1.1\r\ncontent-length: 50000\r\n\r\n{body}"; "GET /info HTTP/1.1\r\n\r\n" ]
        return verify r (fun rs -> List.map (fun r -> r.body) rs = [ "ignored"; "GET|/info||1.1|" ])
    })
    t "a body that arrives after the idle window is still drained" (fun () ->
        withServer (config 39348) handler (fun () -> task {
            let! c = connect 39348
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                let! _ = send s "POST /ignore-body HTTP/1.1\r\ncontent-length: 5\r\n\r\n"
                let! first = readUntil 3000 (String.contains "ignored") s
                // longer than idle_timeout: zerver is draining, not waiting for a head
                do! Task.sleep 600
                let! _ = send s "abcdeGET /info HTTP/1.1\r\nconnection: close\r\n\r\n"
                let! rest = readToEnd 3000 s
                let all = Result.bind (fun a -> rest |> Result.map (fun b -> a + b)) first
                return verify (all |> Result.bind replies) (fun rs -> List.map (fun r -> r.body) rs = [ "ignored"; "GET|/info||1.1|connection=close" ])
        }))
    t "a slow handler is not cut off by the idle timeout" (fun () ->
        expectExchange 39349 "GET /slow HTTP/1.1\r\nconnection: close\r\n\r\n" (one "HTTP/1.1 200 OK" "slow"))
    // Pipelining (RFC 9112 9.3): bytes of the next request that came in the same read as this
    // one are dropped today, so the second request is never answered.
    ptest "two requests in one write are both answered (pipelining)" (fun () ->
        expectExchange 39350 "GET / HTTP/1.1\r\n\r\nGET /info HTTP/1.1\r\nconnection: close\r\n\r\n" (fun rs ->
            List.map (fun r -> r.body) rs = [ "root"; "GET|/info||1.1|connection=close" ]))
    // RFC 9110 9.3.2: a response to HEAD has no body; writing one desynchronizes keep-alive.
    ptest "a HEAD response has no body" (fun () ->
        expectExchange 39351 "HEAD / HTTP/1.1\r\nconnection: close\r\n\r\n" (fun rs ->
            match rs with
            | [ r ] -> r.body = ""
            | _ -> false))
]

let private elapsedSince (start: int) = millis () - start

let timeoutTests = testList "idle timeout" [
    t "a connection that sends nothing is closed after the idle timeout" (fun () ->
        withServer (config 39361) handler (fun () -> task {
            let! c = connect 39361
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                let start = millis ()
                let! back = readToEnd 5000 s
                let took = elapsedSince start
                return
                    if back <> Ok "" then Error $"got {back}"
                    else if took < 250 || took > 1500 then Error $"closed after {took} ms, idle_timeout is 300"
                    else Ok ()
        }))
    t "a kept connection is closed after the idle timeout once answered" (fun () ->
        withServer (config 39362) handler (fun () -> task {
            let! c = connect 39362
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                let! _ = send s "GET / HTTP/1.1\r\n\r\n"
                let start = millis ()
                let! back = readToEnd 5000 s
                let took = elapsedSince start
                return
                    match back |> Result.bind replies with
                    | Ok [ r ] when r.body = "root" && took >= 250 && took <= 1500 -> Ok ()
                    | other -> Error $"got {show other} after {took} ms"
        }))
    t "a head that trickles in is cut at one deadline, not reset by each byte" (fun () ->
        withServer (config 39363) handler (fun () -> task {
            let! c = connect 39363
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                let start = millis ()
                // a byte every 100 ms: each one is inside the idle timeout, the whole is not
                let trickle = task {
                    let mutable i = 0
                    let mutable ok = true
                    while ok && i < 20 do
                        let! w = send s "G"
                        ok <- Result.isOk w
                        do! Task.sleep 100
                        i <- i + 1
                    return ()
                }
                let! (_, back) = Task.both trickle (readToEnd 5000 s)
                let took = elapsedSince start
                // closed with nothing sent back: a plain close, or a reset when a byte was unread
                let silent =
                    match back with
                    | Ok "" -> true
                    | Error e -> String.contains "reset" e && String.contains "after 0 reads" e
                    | Ok _ -> false
                return
                    if not silent then Error $"got {back}"
                    else if took > 1500 then Error $"closed after {took} ms"
                    else Ok ()
        }))
    t "a head in several pieces within the timeout is served" (fun () ->
        withServer (config 39364) handler (fun () -> task {
            let! c = connect 39364
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                for piece in [ "GET /in"; "fo HTTP/1."; "1\r\nconnection: cl"; "ose\r"; "\n\r"; "\n" ] do
                    let! _ = send s piece
                    do! Task.sleep 20
                let! back = readToEnd 3000 s
                return verify (back |> Result.bind replies) (one "HTTP/1.1 200 OK" "GET|/info||1.1|connection=close")
        }))
    t "a head one byte at a time is served" (fun () ->
        withServer (config 39365) handler (fun () -> task {
            let! c = connect 39365
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                for ch in String.chars "GET / HTTP/1.1\r\nconnection: close\r\n\r\n" do
                    let! _ = send s (string ch)
                    do! Task.sleep 2
                let! back = readToEnd 3000 s
                return verify (back |> Result.bind replies) (one "HTTP/1.1 200 OK" "root")
        }))
    t "a body sent separately from its head is read" (fun () ->
        withServer (config 39366) handler (fun () -> task {
            let! c = connect 39366
            match c with
            | Error e -> return Error e
            | Ok s ->
                use s = s
                let! _ = send s "POST /echo HTTP/1.1\r\ncontent-length: 11\r\nconnection: close\r\n\r\n"
                do! Task.sleep 50
                let! _ = send s "hello "
                do! Task.sleep 50
                let! _ = send s "world"
                let! back = readToEnd 3000 s
                return verify (back |> Result.bind replies) (one "HTTP/1.1 200 OK" "hello world")
        }))
]

let concurrencyTests = testList "concurrency" [
    t "fifty connections at once are all answered" (fun () ->
        withServer (config 39371) handler (fun () -> task {
            let client (i: int) = task {
                let! c = connect 39371
                match c with
                | Error e -> return Error e
                | Ok s ->
                    use s = s
                    let! _ = send s $"POST /echo HTTP/1.1\r\ncontent-length: {String.length (string i)}\r\nconnection: close\r\n\r\n{i}"
                    let! back = readToEnd 5000 s
                    return back |> Result.bind replies |> Result.map (fun rs -> List.map (fun r -> r.body) rs)
            }
            let! all = Task.all (List.init 50 client)
            return
                if all = List.init 50 (fun i -> Ok [ string i ]) then Ok ()
                else Error $"got {show all}"
        }))
    t "a slow handler on one connection does not hold up another" (fun () ->
        withServer (config 39372) handler (fun () -> task {
            let get (path: string) = task {
                let! c = connect 39372
                match c with
                | Error e -> return Error e
                | Ok s ->
                    use s = s
                    let! _ = send s $"GET {path} HTTP/1.1\r\nconnection: close\r\n\r\n"
                    let! back = readToEnd 5000 s
                    return back |> Result.map (fun _ -> millis ())
            }
            let! (slow, fast) = Task.both (get "/slow") (get "/")
            return
                match slow, fast with
                | Ok s, Ok f when f < s -> Ok ()
                | other -> Error $"{show other}"
        }))
]

[<Tests>]
let tests = testList "server" [ basicTests; closeTests; badRequestTests; keepAliveTests; timeoutTests; concurrencyTests ]
