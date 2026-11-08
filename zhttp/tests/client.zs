open Testing
open Zhttp
open Zjson
open Zlog
open Serve

[<Derive(Serialize, Deserialize)>]
type Hello = { hello: string }

let private url (port: int) (path: string) = $"http://127.0.0.1:{port}{path}"

let private client () = Client.create () |> Client.withTimeout 5000 |> Client.withMaxRedirects 0

let private call (c: Client) (req: Request) : Task<Response> = task {
    let! result = Client.send c req
    match result with
    | Ok res -> return res
    | Error e -> return panic (message e)
}

let private textOf (res: Response) : Task<string> = task {
    let! text = Response.text res
    match text with
    | Ok s -> return s
    | Error e -> return panic (message e)
}

let private drain (res: Response) : Task<unit> = task {
    let! result = Response.drain res
    match result with
    | Ok () -> ()
    | Error e -> panic (message e)
}

let private show (result: result<Response, HttpError>) : string =
    match result with
    | Ok res -> $"Ok {res.status}"
    | Error e -> $"Error {message e}"

let private certs (san: string) : string * string =
    match Directory.createTemp "zhttp" with
    | Error e -> panic e
    | Ok dir ->
        let key = dir + "/key.pem"
        let cert = dir + "/cert.pem"
        let args = [
            "req"; "-x509"; "-newkey"; "ec"; "-pkeyopt"; "ec_paramgen_curve:prime256v1"
            "-keyout"; key; "-out"; cert; "-days"; "1"; "-nodes"; "-subj"; "/CN=localhost"
            "-addext"; $"subjectAltName={san}"
        ]
        match Process.run "openssl" args with
        | Ok ran when ran.code = 0 -> (cert, key)
        | Ok ran -> panic $"openssl exit {ran.code} {ran.output}"
        | Error e -> panic e

let private fake (status: int) : Response = {
    status = status
    reason = "OK"
    version = Http1_1
    headers = []
    body = { length = Some 0; read = fun _ -> Task.ret (Ok 0) }
    close = fun () -> ()
}

[<Tests>]
let tests = testList "zhttp" [
    test "rejects a url that is not http" (fun () -> task {
        let configured =
            Client.create ()
            |> Client.withDefaults (fun d -> { d with timeoutMs = Some 1000; maxRedirects = 2; maxIdlePerHost = 4 })
        let! result = Client.send configured (Request.get "notaurl")
        match result with
        | Error (Protocol _) -> ()
        | other -> panic $"expected a protocol error, got {show other}"
    })
    test "names a dns failure" (fun () -> task {
        let! result = Client.send (client ()) (Request.get "http://no-such-host.invalid/")
        match result with
        | Error (Dns _) -> ()
        | other -> panic $"expected a dns error, got {show other}"
    })
    test "names a refused connection" (fun () -> task {
        let! result = Client.send (client ()) (Request.get "http://127.0.0.1:1/")
        match result with
        | Error (Connect _) -> ()
        | other -> panic $"expected a connect error, got {show other}"
    })
    test "echoes, keeps the connection, and follows redirects" (fun () -> Serve.run None (fun port -> task {
        let c = client ()
        let! res = call c (Request.get (url port "/echo?x=1"))
        let! body = textOf res
        if body <> "/echo?x=1" then panic body
        let posted = Request.post (url port "/echo") |> Request.text "hi" |> Request.header "x-test" "yes"
        let! res = call c posted
        let! body = textOf res
        if body <> "hi" then panic body
        if Response.header "content-type" res <> Some "text/plain; charset=utf-8" then panic $"type {Response.header "content-type" res}"
        let! res = call c (Request.get (url port "/header") |> Request.header "x-test" "yes")
        let! body = textOf res
        if body <> "yes" then panic body
        let! res = call c (Request.get (url port "/chunked"))
        let! body = textOf res
        if body <> "hello world" then panic $"chunked {body}"
        let! res = call c (Request.get (url port "/count"))
        let! body = textOf res
        if body <> "5" then panic $"after chunked the connection should still be open, count {body}"
        let! res = call c (Request.get (url port "/close"))
        let! body = textOf res
        if body <> "closed" then panic body
        let! res = call c (Request.get (url port "/count"))
        let! body = textOf res
        if body <> "1" then panic $"a closed connection was reused, count {body}"
        let followed = Client.create () |> Client.withTimeout 5000
        let! res = call followed (Request.get (url port "/hop"))
        let! body = textOf res
        if res.status <> 200 || body <> "/echo" then panic $"redirect {res.status} {body}"
        let stopped = Client.create () |> Client.withTimeout 5000 |> Client.withMaxRedirects 0
        let! res = call stopped (Request.get (url port "/redirect"))
        if res.status <> 302 then panic $"redirect not returned: {res.status}"
        let! _ = textOf res
        let limited = Client.create () |> Client.withTimeout 5000 |> Client.withMaxRedirects 1
        let! result = Client.send limited (Request.get (url port "/hop"))
        match result with
        | Error (Redirect _) -> ()
        | other -> panic $"expected a redirect error, got {show other}"
    }))
    test "reads a body in chunks and parses json" (fun () -> Serve.run None (fun port -> task {
        let c = client ()
        let! res = call c (Request.get (url port "/big"))
        let buf = Buffer.create 1000 (u8 0)
        let total = ref 0
        let rec go () = task {
            let! n = res.body.read buf
            match n with
            | Error e -> return panic (message e)
            | Ok 0 -> return ()
            | Ok n ->
                total.Value <- total.Value + n
                if buf.[0] <> u8 97 then panic "expected 'a'"
                return! go ()
        }
        do! go ()
        if total.Value <> 100000 then panic $"read {total.Value}"
        let! quick = Http.get (url port "/echo")
        match quick with
        | Error e -> panic (message e)
        | Ok res ->
            let! quickText = textOf res
            if quickText <> "/echo" then panic $"Http.get {quickText}"
        let req = Request.post (url port "/echo") |> Request.json { hello = "world" }
        let! res = call c req
        let! parsed = (Response.json res : Task<result<Hello, HttpError>>)
        match parsed with
        | Ok hello when hello.hello = "world" -> ()
        | Ok hello -> panic hello.hello
        | Error e -> panic (message e)
    }))
    test "times out and rejects a broken response" (fun () -> Serve.run None (fun port -> task {
        let c = Client.create () |> Client.withTimeout 400 |> Client.withMaxRedirects 0
        let! result = Client.send c (Request.get (url port "/slow"))
        match result with
        | Error Timeout -> ()
        | other -> panic $"expected a timeout, got {show other}"
        let! result = Client.send (client ()) (Request.get (url port "/bad"))
        match result with
        | Error (Protocol _) -> ()
        | other -> panic $"expected a protocol error, got {show other}"
    }))
    test "is safe to call from several tasks" (fun () -> Serve.run None (fun port -> task {
        let c = client ()
        let target = url port "/echo"
        let one () = task {
            let! result = Client.send c (Request.get target)
            match result with
            | Ok res -> return! Response.text res
            | Error e -> return Error e
        }
        let! all = Task.all (List.init 4 (fun _ -> Task.spawn (fun () -> one ())))
        for item in all do
            match item with
            | Ok "/echo" -> ()
            | other -> panic $"spawn {other}"
    }))
    test "logs a request while the client is in use" (fun () -> Serve.run None (fun port -> task {
        match Directory.createTemp "zhttp" with
        | Error e -> panic e
        | Ok dir ->
            let path = dir + "/log.txt"
            match Sinks.File.create Format.text path with
            | Error e -> panic e
            | Ok file ->
                let send = client () |> Client.sender |> Middleware.log
                let! result =
                    LoggerConfig.empty
                    |> LoggerConfig.withSink file
                    |> LoggerConfig.runWith (fun () -> send (Request.get (url port "/echo")))
                match result with
                | Error e -> panic (message e)
                | Ok res ->
                    let! body = textOf res
                    if body <> "/echo" then panic body
                match File.readAllText path with
                | Error e -> panic e
                | Ok text ->
                    if not (String.contains "status=200" text) then panic $"log was {text}"
    }))
    test "retries an idempotent failure and logs it" (fun () -> task {
        let tries = ref 0
        let flaky (req: Request) = task {
            tries.Value <- tries.Value + 1
            if tries.Value < 3 then return Error (Connect "reset") else return Ok (fake 204)
        }
        let! result = (flaky |> Middleware.retry 2) (Request.get "http://example.test/")
        match result with
        | Ok res when res.status = 204 && tries.Value = 3 -> ()
        | other -> panic $"retry {tries.Value} {show other}"
        let posts = ref 0
        let once (req: Request) = task {
            posts.Value <- posts.Value + 1
            return Error (Connect "reset")
        }
        let! _ = (once |> Middleware.retry 4) (Request.post "http://example.test/" |> Request.text "x")
        if posts.Value <> 1 then panic $"post was retried {posts.Value} times"
        match Directory.createTemp "zhttp" with
        | Error e -> panic e
        | Ok dir ->
            let path = dir + "/log.txt"
            match Sinks.File.create Format.text path with
            | Error e -> panic e
            | Ok file ->
                let send = (fun (req: Request) -> Task.ret (Ok (fake 204))) |> Middleware.log
                let! _ = LoggerConfig.empty |> LoggerConfig.withSink file |> LoggerConfig.runWith (fun () -> send (Request.get "http://example.test/logged"))
                match File.readAllText path with
                | Error e -> panic e
                | Ok text ->
                    if not (String.contains "status=204" text && String.contains "http://example.test/logged" text) then
                        panic $"log was {text}"
    })
    test "checks the server certificate" (fun () -> task {
        let (cert, key) = certs "DNS:localhost,IP:127.0.0.1"
        do! Serve.run (Some (cert, key)) (fun port -> task {
            let target = $"https://127.0.0.1:{port}/echo"
            let plain = client ()
            let! denied = Client.send plain (Request.get target)
            match denied with
            | Error (Tls _) -> ()
            | other -> panic $"expected a tls error, got {show other}"
            let trusted = Client.create () |> Client.withCaFile cert |> Client.withTimeout 5000 |> Client.withMaxRedirects 0
            let! res = call trusted (Request.post target |> Request.text "secret")
            let! body = textOf res
            if body <> "secret" then panic body
            let openClient = Client.create () |> Client.withVerification false |> Client.withTimeout 5000 |> Client.withMaxRedirects 0
            let! res = call openClient (Request.get target)
            if res.status <> 200 then panic (string res.status)
            do! drain res
            let pooled = Client.create () |> Client.withCaFile cert |> Client.withTimeout 5000 |> Client.withMaxRedirects 0
            let! first = call pooled (Request.get $"https://127.0.0.1:{port}/count")
            let! a = textOf first
            let! second = call pooled (Request.get $"https://127.0.0.1:{port}/chunked")
            let! b = textOf second
            let! third = call pooled (Request.get $"https://127.0.0.1:{port}/count")
            let! c = textOf third
            if a <> "1" || b <> "hello world" || c <> "3" then panic $"https {a} {b} {c}"
        })
    })
    test "rejects a certificate for a different host" (fun () -> task {
        let (cert, key) = certs "DNS:localhost"
        do! Serve.run (Some (cert, key)) (fun port -> task {
            let trusted = Client.create () |> Client.withCaFile cert |> Client.withTimeout 5000
            let! result = Client.send trusted (Request.get $"https://127.0.0.1:{port}/echo")
            match result with
            | Error (Tls _) -> ()
            | other -> panic $"expected a hostname failure, got {show other}"
        })
    })
    test "gets a public https page" (fun () -> task {
        let c = Client.create () |> Client.withTimeout 20000
        let! result = Client.send c (Request.get "https://example.com/")
        match result with
        | Error (Dns _) | Error (Connect _) | Error Timeout -> ()
        | Error e -> panic (message e)
        | Ok res ->
            let! text = Response.text res
            match text with
            | Ok body when res.status = 200 && String.contains "Example" body -> ()
            | Ok _ -> panic $"status {res.status}"
            | Error e -> panic (message e)
    })
]
