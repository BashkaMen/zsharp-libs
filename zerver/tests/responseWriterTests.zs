open Testing
open Zerver
open Support

let private bytesOf (s: string) : Buffer<u8> = Buffer.ofSpan (String.bytes s)

// A loopback connection: zerver's end (its fd) and the client's end, which `body` closes.
let private pair (port: int) (body: int -> TcpStream -> Task<result<'a, string>>) : Task<result<'a, string>> = task {
    match Tcp.listen "127.0.0.1" port with
    | Error e -> return Error e
    | Ok listener ->
        use l = listener
        let! client = connect port
        let! accepted = Tcp.accept l
        match client, accepted with
        | Ok c, Ok a ->
            let (TcpStream fd) = a
            return! body fd c
        | Error e, _ -> return Error e
        | _, Error e -> return Error e
}

// The responses written one after another with one `out` buffer, as zerver does on a
// connection, and what the client read back.
let private writeAll (port: int) (out: ref<Buffer<u8>>) (responses: list<Http.Response>) : Task<result<string, string>> =
    pair port (fun fd client -> task {
        // the client reads while zerver writes: a response larger than the socket buffer
        // only goes through when someone reads it
        let writer = task {
            let mutable failed = None
            for response in responses do
                if Option.isNone failed then
                    let! w = Helpers.ResponseWriter.write fd out response
                    match w with
                    | Error e -> failed <- Some e
                    | Ok () -> ()
            unsafe Native.fdClose fd
            return failed
        }
        let! (failed, read) = Task.both writer (readToEnd 8000 client)
        unsafe Native.fdClose (let (TcpStream c) = client in c)
        match failed with
        | Some e -> return Error e
        | None -> return read
    })

let private wire (port: int) (responses: list<Http.Response>) = writeAll port (ref (Buffer.create 4096 (u8 0))) responses

let private text (code: int) (body: string) =
    Http.Response.empty () |> Http.Response.withCode code |> Http.Response.withBody (Http.ResponseBody.fromText body)

let private status (port: int) (code: int) : Task<result<string, string>> = task {
    let! raw = wire port [ Http.Response.empty () |> Http.Response.withCode code ]
    return raw |> Result.map (fun r -> String.sub r 0 (Option.defaultValue 0 (String.indexOf "\r\n" r)))
}

let private expectWire (port: int) (responses: list<Http.Response>) (expected: string) = task {
    let! raw = wire port responses
    return
        match raw with
        | Ok r when r = expected -> Ok ()
        | Ok r -> Error $"wrote {show r}, expected {show expected}"
        | Error e -> Error e
}

let private expectStatus (port: int) (code: int) (line: string) = task {
    let! s = status port code
    return if s = Ok line then Ok () else Error $"status line {s}, expected {line}"
}

let private t (name: string) body = testWithTimeout 10000 name body

let statusTests = testList "status line" [
    t "200" (fun () -> expectStatus 39201 200 "HTTP/1.1 200 OK")
    t "400" (fun () -> expectStatus 39202 400 "HTTP/1.1 400 Bad Request")
    t "404" (fun () -> expectStatus 39203 404 "HTTP/1.1 404 Not Found")
    t "500" (fun () -> expectStatus 39204 500 "HTTP/1.1 500 Internal Server Error")
    t "503" (fun () -> expectStatus 39205 503 "HTTP/1.1 503 Service Unavailable")
    t "a code without a reason is Unknown" (fun () -> expectStatus 39206 418 "HTTP/1.1 418 Unknown")
    t "201 is written with its digits" (fun () -> expectStatus 39207 201 "HTTP/1.1 201 Unknown")
    t "a one-digit code" (fun () -> expectStatus 39208 7 "HTTP/1.1 7 Unknown")
    t "code 0" (fun () -> expectStatus 39209 0 "HTTP/1.1 0 Unknown")
    t "a long code" (fun () -> expectStatus 39210 1234567 "HTTP/1.1 1234567 Unknown")
    t "a negative code" (fun () -> expectStatus 39211 -5 "HTTP/1.1 -5 Unknown")
]

let wireTests = testList "wire" [
    t "an empty response" (fun () ->
        expectWire 39221 [ Http.Response.empty () ] "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n")
    t "a text body" (fun () ->
        expectWire 39222 [ text 200 "hello" ] "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello")
    t "headers follow content-length, latest withHeader first" (fun () ->
        let r = text 404 "x" |> Http.Response.withHeader "a" "1" |> Http.Response.withHeader "b" "2"
        expectWire 39223 [ r ] "HTTP/1.1 404 Not Found\r\ncontent-length: 1\r\nb: 2\r\na: 1\r\n\r\nx")
    t "a JSON body sets content-type" (fun () ->
        let r = Http.Response.empty () |> Http.Response.withJsonOf 200 [ 1; 2; 3 ]
        expectWire 39224 [ r ] "HTTP/1.1 200 OK\r\ncontent-length: 7\r\ncontent-type: application/json\r\n\r\n[1,2,3]")
    t "UTF-8 header values and bodies are written as bytes" (fun () ->
        let r = text 200 "привіт ✓" |> Http.Response.withHeader "x-name" "Дмитро"
        expectWire 39225 [ r ] "HTTP/1.1 200 OK\r\ncontent-length: 16\r\nx-name: Дмитро\r\n\r\nпривіт ✓")
    t "a body written in several sends is joined" (fun () ->
        let chunks = [ "ab"; "cde"; "f" ]
        let body : Http.ResponseBody = {
            length = 6
            write = fun send -> task {
                let mutable r = Ok ()
                for c in chunks do
                    if r = Ok () then
                        let b = bytesOf c
                        let! w = send b (Buffer.length b)
                        r <- w
                return r
            } }
        expectWire 39226 [ Http.Response.empty () |> Http.Response.withBody body ] "HTTP/1.1 200 OK\r\ncontent-length: 6\r\n\r\nabcdef")
    t "a send of fewer bytes than the chunk holds writes only those" (fun () ->
        let body : Http.ResponseBody = { length = 2; write = fun send -> send (bytesOf "okNOT") 2 }
        expectWire 39227 [ Http.Response.empty () |> Http.Response.withBody body ] "HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok")
    t "a body's error is the write's error" (fun () -> task {
        let body : Http.ResponseBody = { length = 3; write = fun _ -> Task.ret (Error "no body") }
        let! r = wire 39228 [ Http.Response.empty () |> Http.Response.withBody body ]
        return if r = Error "no body" then Ok () else Error $"got {r}"
    })
    t "a body larger than the starting buffer is written whole" (fun () -> task {
        let big = repeat 100000 "0123456789"
        let! r = wire 39229 [ text 200 big ]
        return
            match r |> Result.bind replies with
            | Ok [ reply ] when reply.body = big && header "content-length" reply = Some "1000000" -> Ok ()
            | other -> Error $"got {String.length (show other)} bytes of something else"
    })
    t "many headers larger than the starting buffer" (fun () -> task {
        let r = List.fold (fun r i -> r |> Http.Response.withHeader $"x-h{i}" (repeat 50 "v")) (Http.Response.empty ()) [ 1 .. 200 ]
        let! raw = wire 39230 [ r ]
        return
            match raw |> Result.bind replies with
            | Ok [ reply ] when List.length reply.headers = 201 -> Ok ()
            | other -> Error $"got {show other}"
    })
    t "a reused buffer: a big response, then a small one, has none of the big one" (fun () -> task {
        let out = ref (Buffer.create 4096 (u8 0))
        let big = repeat 10000 "B"
        let! raw = writeAll 39231 out [ text 200 big; text 404 "s"; Http.Response.empty () ]
        return
            match raw |> Result.bind replies with
            | Ok [ a; b; c ] when a.body = big && b.status = "HTTP/1.1 404 Not Found" && b.body = "s" && c.body = "" && c.headers = [ ("content-length", "0") ] -> Ok ()
            | other -> Error $"got {show other}"
    })
    t "the buffer grows to fit and is kept" (fun () -> task {
        let out = ref (Buffer.create 16 (u8 0))
        let! raw = writeAll 39232 out [ text 200 (repeat 5000 "x") ]
        return
            if Result.isOk raw && Buffer.length out.Value >= 5000 then Ok ()
            else Error $"buffer {Buffer.length out.Value}, {Result.isOk raw}"
    })
    t "a response bigger than the socket buffer is written whole (EAGAIN path)" (fun () -> task {
        // 8 MB is more than a loopback socket takes at once, so writeAll waits for it to drain
        let big = repeat (8 * 1024 * 1024 / 16) "0123456789abcdef"
        let! r = wire 39233 [ text 200 big ]
        return
            match r |> Result.bind replies with
            | Ok [ reply ] when reply.body = big -> Ok ()
            | Ok rs -> Error $"{List.length rs} replies"
            | Error e -> Error (String.sub e 0 (min 200 (String.length e)))
    })
    // String.length counts bytes; were it characters, 600 bytes of Cyrillic counted as 300
    // would overrun the 96 bytes of slack
    t "a long non-ASCII header value is written whole" (fun () ->
        let value = repeat 300 "я"
        let r = text 200 "ok" |> Http.Response.withHeader "x-ім'я" value
        expectWire 39235 [ r ] $"HTTP/1.1 200 OK\r\ncontent-length: 2\r\nx-ім'я: {value}\r\n\r\nok")
    t "many non-ASCII headers with a non-ASCII body" (fun () -> task {
        let r = List.fold (fun r i -> r |> Http.Response.withHeader $"x-{i}" (repeat 40 "✓я")) (text 200 (repeat 100 "ї")) [ 1 .. 50 ]
        let! raw = wire 39236 [ r ]
        return
            match raw |> Result.bind replies with
            | Ok [ reply ] when List.length reply.headers = 51 && reply.body = repeat 100 "ї" && header "x-7" reply = Some (repeat 40 "✓я") -> Ok ()
            | other -> Error $"got {show other}"
    })
    t "a response over the kept limit gets a buffer of its own" (fun () -> task {
        let out = ref (Buffer.create 4096 (u8 0))
        let big = repeat (Helpers.ResponseWriter.keptBufferLimit * 3) "L"
        let! raw = writeAll 39237 out [ text 200 big; text 200 "small" ]
        return
            match raw |> Result.bind replies with
            | Ok [ a; b ] when a.body = big && b.body = "small" && Buffer.length out.Value = 4096 -> Ok ()
            | Ok rs -> Error $"{List.length rs} replies, kept buffer {Buffer.length out.Value}"
            | Error e -> Error (String.sub e 0 (min 200 (String.length e)))
    })
    t "the kept buffer never grows past the limit" (fun () -> task {
        let out = ref (Buffer.create 16 (u8 0))
        let limit = Helpers.ResponseWriter.keptBufferLimit
        let sizes = [ 100; limit / 2; limit - 200; limit; limit + 1; 10 * limit; 50 ]
        let! raw = writeAll 39238 out (List.map (fun n -> text 200 (repeat n "k")) sizes)
        return
            match raw |> Result.bind replies with
            | Ok rs when List.map (fun r -> String.length r.body) rs = sizes && Buffer.length out.Value <= limit -> Ok ()
            | Ok rs -> Error $"bodies {List.map (fun r -> String.length r.body) rs}, kept buffer {Buffer.length out.Value}"
            | Error e -> Error (String.sub e 0 (min 200 (String.length e)))
    })
    t "a response just under the limit is rendered in the kept buffer" (fun () -> task {
        let out = ref (Buffer.create 16 (u8 0))
        let! raw = writeAll 39239 out [ text 200 (repeat 60000 "k") ]
        return
            if Result.isOk raw && Buffer.length out.Value > 60000 && Buffer.length out.Value <= Helpers.ResponseWriter.keptBufferLimit then Ok ()
            else Error $"kept buffer {Buffer.length out.Value}"
    })
    t "a body that writes more than its length is an error, not a panic" (fun () -> task {
        let body : Http.ResponseBody = { length = 3; write = fun send -> send (bytesOf "too long") 8 }
        let! r = wire 39240 [ Http.Response.empty () |> Http.Response.withBody body ]
        return
            match r with
            | Error e when String.contains "more than its length" e -> Ok ()
            | other -> Error $"got {other}"
    })
    t "a body that writes less than its length is an error, not a short response" (fun () -> task {
        let body : Http.ResponseBody = { length = 10; write = fun send -> send (bytesOf "short") 5 }
        let! r = wire 39241 [ Http.Response.empty () |> Http.Response.withBody body ]
        return
            match r with
            | Error e when String.contains "wrote 5 of its length of 10" e -> Ok ()
            | other -> Error $"got {other}"
    })
    t "a write to a closed socket is an error, not a panic" (fun () ->
        pair 39234 (fun fd client -> task {
            unsafe Native.fdClose (let (TcpStream c) = client in c) |> ignore
            let out = ref (Buffer.create 4096 (u8 0))
            let big = text 200 (repeat (4 * 1024 * 1024) "x")
            // the first write may land in the socket buffer before the reset is seen
            let! a = Helpers.ResponseWriter.write fd out big
            let! b = Helpers.ResponseWriter.write fd out big
            unsafe Native.fdClose fd
            return if Result.isError a || Result.isError b then Ok () else Error "both writes succeeded"
        }))
]

[<Tests>]
let tests = testList "responseWriter" [ statusTests; wireTests ]
