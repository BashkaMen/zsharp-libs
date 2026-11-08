open Testing
open Zerver


type Read = Buffer<u8> -> Task<result<int, string>>

let private bytesOf (s: string) : Buffer<u8> = Buffer.ofSpan (String.bytes s)

let private textOf (b: Buffer<u8>) (n: int) : string =
    let view : span<u8> = b
    if n = 0 then "" else String.ofBytes view.[0 .. n - 1]

// A socket that hands out the chunks one per read (cut to the reader's buffer), then 0: closed.
// `asked` records each read's buffer size.
let private socket (chunks: list<string>) (asked: ref<list<int>>) : Read =
    let left = ref (List.map bytesOf chunks)
    fun (dest: Buffer<u8>) ->
        asked.Value <- asked.Value @ [ Buffer.length dest ]
        match left.Value with
        | [] -> Task.ret (Ok 0)
        | chunk :: rest ->
            let n = min (Buffer.length chunk) (Buffer.length dest)
            Buffer.blit chunk 0 dest 0 n
            left.Value <- if n < Buffer.length chunk then chunk.[n .. Buffer.length chunk - 1] :: rest else rest
            Task.ret (Ok n)

let private noAsk () = ref []

// A socket that must not be read.
let private untouched : Read = fun _ -> Task.ret (Error "the socket was read")

let private failing (e: string) : Read = fun _ -> Task.ret (Error e)

let private bufferSize = 16 * 1024

// What fillHead would have: the bytes in a request buffer, the head's end, and what was read.
let private parseWith (scratch: Buffer<u8>) (read: Read) (raw: string) : result<Http.Request, string> =
    let buffer = Buffer.create bufferSize (u8 0)
    let src = String.bytes raw
    let filled = Span.length src
    Buffer.blit src 0 buffer 0 filled
    match Helpers.RequestParser.headEnd buffer 0 filled with
    | None -> Error "test: no blank line in the input"
    | Some e -> Helpers.RequestParser.parse buffer scratch (Buffer.create 0 (u8 0)) read e filled

let private parseRead (read: Read) (raw: string) = parseWith (Buffer.create 256 (u8 0)) read raw

let private parse (raw: string) = parseRead untouched raw

let private ok (r: result<Http.Request, string>) : Http.Request =
    match r with
    | Ok request -> request
    | Error e -> panic $"expected a request, got Error {e}"

let private isError (r: result<Http.Request, string>) : bool =
    match r with
    | Ok _ -> false
    | Error _ -> true

let private errorOf (r: result<Http.Request, string>) : string =
    match r with
    | Ok _ -> panic "expected an error, got a request"
    | Error e -> e

// The body read to its end with a buffer of `chunk` bytes: the text, or the first error.
let private readBody (chunk: int) (body: Http.Body) : Task<result<string, string>> =
    let dest = Buffer.create chunk (u8 0)
    let rec go (acc: string) = task {
        let! n = body.read dest
        match n with
        | Error e -> return Error e
        | Ok 0 -> return Ok acc
        | Ok n -> return! go (acc + textOf dest n)
    }
    go ""

let private bodyOf (chunk: int) (request: Http.Request) : result<string, string> =
    Task.run (readBody chunk request.body)

let private get (raw: string) : Http.Request = ok (parse raw)

let private headEnd (s: string) =
    let b = bytesOf s
    Helpers.RequestParser.headEnd b 0 (Buffer.length b)

let headEndTests = testList "headEnd" [
    test "finds the blank line after the request line" (fun () ->
        assert (headEnd "GET / HTTP/1.1\r\n\r\n" = Some 14))
    test "finds it after headers" (fun () ->
        assert (headEnd "GET / HTTP/1.1\r\nhost: x\r\n\r\n" = Some 23))
    test "is None without a blank line" (fun () ->
        assert (headEnd "GET / HTTP/1.1\r\nhost: x\r\n" = None))
    test "is None for an empty buffer" (fun () ->
        assert (headEnd "" = None))
    test "is None for fewer than four bytes" (fun () ->
        assert (headEnd "\r\n\r" = None))
    test "is 0 for a buffer that starts with the blank line" (fun () ->
        assert (headEnd "\r\n\r\nGET" = Some 0))
    test "finds the first blank line, not one in the body" (fun () ->
        assert (headEnd "POST / HTTP/1.1\r\n\r\nbody\r\n\r\n" = Some 15))
    test "does not take bare LFs as a blank line" (fun () ->
        assert (headEnd "GET / HTTP/1.1\n\n" = None))
    test "does not take CR CR LF LF as a blank line" (fun () ->
        assert (headEnd "GET / HTTP/1.1\r\r\n\n" = None))
    test "only looks below filled" (fun () ->
        let b = bytesOf "GET / HTTP/1.1\r\n\r\n"
        assert (Helpers.RequestParser.headEnd b 0 17 = None)
        assert (Helpers.RequestParser.headEnd b 0 18 = Some 14))
    test "a resumed scan finds a blank line that straddles the previous read" (fun () ->
        let b = bytesOf "GET / HTTP/1.1\r\n\r\n"
        // the previous read ended at each byte of the terminator, and found nothing
        for from in 14 .. 17 do
            assert (Helpers.RequestParser.headEnd b 0 from = None)
            assert (Helpers.RequestParser.headEnd b from 18 = Some 14))
    test "a resumed scan from 0 to 3 starts at 0" (fun () ->
        let b = bytesOf "\r\n\r\n"
        for from in 0 .. 3 do
            assert (Helpers.RequestParser.headEnd b from 4 = Some 0))
    test "a resumed scan at every split of a long head matches a full scan" (fun () ->
        let raw = "POST /a/b?c=d HTTP/1.1\r\nhost: example.com\r\ncontent-length: 3\r\naccept: */*\r\n\r\nabc"
        let b = bytesOf raw
        let n = Buffer.length b
        let full = Helpers.RequestParser.headEnd b 0 n
        assert (full = Some 73)
        for from in 0 .. n do
            if Helpers.RequestParser.headEnd b 0 from = None then
                assert (Helpers.RequestParser.headEnd b from n = full))
]

let methodTests = testList "method" [
    test "GET" (fun () -> assert ((get "GET / HTTP/1.1\r\n\r\n").method = Http.Get))
    test "HEAD" (fun () -> assert ((get "HEAD / HTTP/1.1\r\n\r\n").method = Http.Head))
    test "POST" (fun () -> assert ((get "POST / HTTP/1.1\r\n\r\n").method = Http.Post))
    test "PUT" (fun () -> assert ((get "PUT / HTTP/1.1\r\n\r\n").method = Http.Put))
    test "DELETE" (fun () -> assert ((get "DELETE / HTTP/1.1\r\n\r\n").method = Http.Delete))
    test "OPTIONS" (fun () -> assert ((get "OPTIONS * HTTP/1.1\r\n\r\n").method = Http.Options))
    test "anything else is Other, verbatim" (fun () ->
        assert ((get "PATCH / HTTP/1.1\r\n\r\n").method = Http.Other "PATCH")
        assert ((get "CONNECT / HTTP/1.1\r\n\r\n").method = Http.Other "CONNECT"))
    test "methods are case-sensitive" (fun () ->
        assert ((get "get / HTTP/1.1\r\n\r\n").method = Http.Other "get")
        assert ((get "Post / HTTP/1.1\r\n\r\n").method = Http.Other "Post"))
    test "a prefix of a known method is Other" (fun () ->
        assert ((get "GE / HTTP/1.1\r\n\r\n").method = Http.Other "GE")
        assert ((get "GETS / HTTP/1.1\r\n\r\n").method = Http.Other "GETS"))
]

let versionTests = testList "version" [
    test "HTTP/1.1" (fun () -> assert ((get "GET / HTTP/1.1\r\n\r\n").version = Http.Http1_1))
    test "HTTP/1.0" (fun () -> assert ((get "GET / HTTP/1.0\r\n\r\n").version = Http.Http1_0))
    test "HTTP/2.0 is unsupported" (fun () ->
        assert (errorOf (parse "GET / HTTP/2.0\r\n\r\n") = "unsupported version: HTTP/2.0"))
    test "HTTP/0.9 is unsupported" (fun () -> assert (isError (parse "GET / HTTP/0.9\r\n\r\n")))
    test "the version is case-sensitive" (fun () -> assert (isError (parse "GET / http/1.1\r\n\r\n")))
    test "a version with extra bytes is unsupported" (fun () ->
        assert (isError (parse "GET / HTTP/1.10\r\n\r\n"))
        assert (isError (parse "GET / HTTP/1.1x\r\n\r\n")))
    test "a missing version is a malformed request line" (fun () ->
        assert (errorOf (parse "GET /\r\n\r\n") = "malformed request line: GET /"))
]

let requestLineTests = testList "request line" [
    test "an empty head is an error" (fun () ->
        assert (errorOf (parse "\r\n\r\n") = "empty request head"))
    test "a head that starts with an empty line is an error" (fun () ->
        assert (isError (parse "\r\nGET / HTTP/1.1\r\n\r\n")))
    test "one token is malformed" (fun () ->
        assert (errorOf (parse "GET\r\n\r\n") = "malformed request line: GET"))
    test "four tokens are malformed" (fun () ->
        assert (errorOf (parse "GET / HTTP/1.1 x\r\n\r\n") = "malformed request line: GET / HTTP/1.1 x"))
    test "two spaces between tokens are malformed" (fun () ->
        assert (isError (parse "GET  / HTTP/1.1\r\n\r\n"))
        assert (isError (parse "GET /  HTTP/1.1\r\n\r\n")))
    test "a trailing space is malformed" (fun () ->
        assert (isError (parse "GET / HTTP/1.1 \r\n\r\n")))
    test "a space after the target with no version is an error" (fun () ->
        assert (isError (parse "GET / \r\n\r\n")))
    test "a tab is not a separator" (fun () ->
        assert (isError (parse "GET\t/\tHTTP/1.1\r\n\r\n")))
    test "the NOT A VALID REQUEST LINE of the e2e test is an error" (fun () ->
        assert (isError (parse "NOT A VALID REQUEST LINE\r\n\r\n")))
    test "the target is kept byte for byte" (fun () ->
        let r = get "GET /a%20b/%D1%8F;x=1 HTTP/1.1\r\n\r\n"
        assert (r.path = "/a%20b/%D1%8F;x=1"))
    test "an absolute-form target is the path" (fun () ->
        assert ((get "GET http://example.com/x HTTP/1.1\r\n\r\n").path = "http://example.com/x"))
    test "the asterisk-form target is the path" (fun () ->
        assert ((get "OPTIONS * HTTP/1.1\r\n\r\n").path = "*"))
    test "UTF-8 in the target is kept" (fun () ->
        assert ((get "GET /привет HTTP/1.1\r\n\r\n").path = "/привет"))
]

let private target (url: string) : string * string =
    let r : Http.Request = get $"GET {url} HTTP/1.1\r\n\r\n"
    (r.path, r.query)

let urlTests = testList "url" [
    test "no query" (fun () -> assert (target "/users" = ("/users", "")))
    test "the root" (fun () -> assert (target "/" = ("/", "")))
    test "a query" (fun () -> assert (target "/users?id=1&x=2" = ("/users", "id=1&x=2")))
    test "an empty query" (fun () -> assert (target "/users?" = ("/users", "")))
    test "only a query" (fun () -> assert (target "?a=1" = ("", "a=1")))
    test "only a question mark" (fun () -> assert (target "?" = ("", "")))
    test "the query starts at the first question mark" (fun () -> assert (target "/a?b?c" = ("/a", "b?c")))
    test "a fragment is part of the query" (fun () -> assert (target "/a?b=1#f" = ("/a", "b=1#f")))
    test "a fragment without a query is part of the path" (fun () -> assert (target "/a#f" = ("/a#f", "")))
]

let private headers (raw: string) = (get raw).headers

let headerTests = testList "headers" [
    test "none" (fun () -> assert (headers "GET / HTTP/1.1\r\n\r\n" = []))
    test "one" (fun () -> assert (headers "GET / HTTP/1.1\r\nhost: x\r\n\r\n" = [ ("host", "x") ]))
    test "in order" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nb: 2\r\na: 1\r\nc: 3\r\n\r\n" = [ ("b", "2"); ("a", "1"); ("c", "3") ]))
    test "repeated names are all kept" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nx: 1\r\nX: 2\r\n\r\n" = [ ("x", "1"); ("x", "2") ]))
    test "names are lowered" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nContent-Type: text/plain\r\nX-REQUEST-ID: Ab\r\n\r\n" = [ ("content-type", "text/plain"); ("x-request-id", "Ab") ]))
    test "values keep their case" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nx: MiXeD\r\n\r\n" = [ ("x", "MiXeD") ]))
    test "only ASCII letters are lowered in a name" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nA-Z_09[@`{: v\r\n\r\n" = [ ("a-z_09[@`{", "v") ]))
    test "values are trimmed of spaces and tabs" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nx: \t a b \t \r\n\r\n" = [ ("x", "a b") ]))
    test "a value with no space after the colon" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nx:v\r\n\r\n" = [ ("x", "v") ]))
    test "an empty value" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nx:\r\n\r\n" = [ ("x", "") ]))
    test "a value of only spaces is empty" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nx:   \r\n\r\n" = [ ("x", "") ]))
    test "the name ends at the first colon" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nhost: localhost:8080\r\n\r\n" = [ ("host", "localhost:8080") ]))
    test "UTF-8 in a value is kept" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nx-name: Дмитро ✓\r\n\r\n" = [ ("x-name", "Дмитро ✓") ]))
    test "a line without a colon is an error" (fun () ->
        assert (errorOf (parse "GET / HTTP/1.1\r\nnocolon\r\n\r\n") = "malformed header: nocolon"))
    test "an empty name is an error" (fun () ->
        assert (errorOf (parse "GET / HTTP/1.1\r\n: v\r\n\r\n") = "malformed header: : v"))
    test "a space before the colon is an error" (fun () ->
        assert (errorOf (parse "GET / HTTP/1.1\r\nhost : x\r\n\r\n") = "whitespace around header name: host : x"))
    test "a tab before the colon is an error" (fun () ->
        assert (isError (parse "GET / HTTP/1.1\r\nhost\t: x\r\n\r\n")))
    test "a line folded onto the previous one (leading space) is an error" (fun () ->
        assert (isError (parse "GET / HTTP/1.1\r\nx: a\r\n  b\r\n\r\n"))
        assert (isError (parse "GET / HTTP/1.1\r\n host: x\r\n\r\n")))
    test "a bad header after good ones is an error" (fun () ->
        assert (isError (parse "GET / HTTP/1.1\r\na: 1\r\nb: 2\r\nbad\r\n\r\n")))
    test "a name as long as the scratch buffer is lowered" (fun () ->
        let name = String.repeat 256 "N"
        assert (headers $"GET / HTTP/1.1\r\n{name}: v\r\n\r\n" = [ (String.repeat 256 "n", "v") ]))
    test "a name longer than the scratch buffer is lowered too" (fun () ->
        let name = String.repeat 1000 "Ab"
        assert (headers $"GET / HTTP/1.1\r\n{name}: v\r\n\r\n" = [ (String.repeat 1000 "ab", "v") ]))
    test "a name of one byte" (fun () ->
        assert (headers "GET / HTTP/1.1\r\nX: v\r\n\r\n" = [ ("x", "v") ]))
    test "a reused scratch buffer does not leak an earlier, longer name" (fun () ->
        let scratch = Buffer.create 256 (u8 0)
        let first = ok (parseWith scratch untouched "GET / HTTP/1.1\r\nVery-Long-Name: 1\r\n\r\n")
        let second = ok (parseWith scratch untouched "GET / HTTP/1.1\r\nAb: 2\r\n\r\n")
        assert (first.headers = [ ("very-long-name", "1") ])
        assert (second.headers = [ ("ab", "2") ]))
    test "names already returned are not changed by the next parse" (fun () ->
        let scratch = Buffer.create 256 (u8 0)
        let first = ok (parseWith scratch untouched "GET / HTTP/1.1\r\nAaaa: 1\r\n\r\n")
        ok (parseWith scratch untouched "GET / HTTP/1.1\r\nZzzz: 2\r\n\r\n") |> ignore
        assert (first.headers = [ ("aaaa", "1") ]))
    test "a hundred headers" (fun () ->
        let lines = List.init 100 (fun i -> $"h{i}: {i}\r\n") |> String.concat
        let hs = headers $"GET / HTTP/1.1\r\n{lines}\r\n"
        assert (List.length hs = 100)
        assert (List.head hs = ("h0", "0"))
        assert (List.last hs = ("h99", "99")))
]

let contentLengthTests = testList "content-length" [
    test "no content-length is an empty body" (fun () ->
        assert ((get "POST / HTTP/1.1\r\n\r\n").body.length = 0))
    test "content-length 0" (fun () ->
        assert ((get "POST / HTTP/1.1\r\ncontent-length: 0\r\n\r\n").body.length = 0))
    test "the name is case-insensitive" (fun () ->
        assert ((ok (parseRead (socket [ "abc" ] (noAsk ())) "POST / HTTP/1.1\r\nContent-Length: 3\r\n\r\n")).body.length = 3))
    test "the value may have spaces around it" (fun () ->
        assert ((ok (parseRead (socket [ "abc" ] (noAsk ())) "POST / HTTP/1.1\r\ncontent-length:   3  \r\n\r\n")).body.length = 3))
    test "a negative length is an error" (fun () ->
        assert (errorOf (parse "POST / HTTP/1.1\r\ncontent-length: -1\r\n\r\n") = "invalid content-length: -1"))
    test "a non-numeric length is an error" (fun () ->
        assert (errorOf (parse "POST / HTTP/1.1\r\ncontent-length: abc\r\n\r\n") = "invalid content-length: abc"))
    test "an empty length is an error" (fun () ->
        assert (isError (parse "POST / HTTP/1.1\r\ncontent-length:\r\n\r\n")))
    test "a length with trailing bytes is an error" (fun () ->
        assert (isError (parse "POST / HTTP/1.1\r\ncontent-length: 5x\r\n\r\n"))
        assert (isError (parse "POST / HTTP/1.1\r\ncontent-length: 5 5\r\n\r\n")))
    test "a fractional length is an error" (fun () ->
        assert (isError (parse "POST / HTTP/1.1\r\ncontent-length: 1.5\r\n\r\n")))
    test "a length past the int range is an error" (fun () ->
        assert (isError (parse "POST / HTTP/1.1\r\ncontent-length: 99999999999999999999\r\n\r\n")))
    test "transfer-encoding is unsupported" (fun () ->
        assert (errorOf (parse "POST / HTTP/1.1\r\ntransfer-encoding: chunked\r\n\r\n") = "transfer-encoding is not supported"))
    test "transfer-encoding wins over content-length" (fun () ->
        assert (isError (parse "POST / HTTP/1.1\r\ncontent-length: 3\r\nTransfer-Encoding: chunked\r\n\r\nabc")))
    // RFC 9110 8.6: Content-Length = 1*DIGIT; anything else must be rejected, or a proxy in front
    // and this server can disagree on where the body ends.
    ptest "a plus sign is an error (RFC 9110: 1*DIGIT)" (fun () ->
        assert (isError (parse "POST / HTTP/1.1\r\ncontent-length: +3\r\n\r\nabc")))
    ptest "differing repeated content-lengths are an error (RFC 9112 6.3)" (fun () ->
        assert (isError (parse "POST / HTTP/1.1\r\ncontent-length: 3\r\ncontent-length: 5\r\n\r\nabc")))
]

let bodyTests = testList "body" [
    test "an empty body reads 0 without the socket" (fun () ->
        let r = get "GET / HTTP/1.1\r\n\r\n"
        assert (bodyOf 16 r = Ok ""))
    test "a body read with the head comes from the buffer" (fun () ->
        let r = get "POST / HTTP/1.1\r\ncontent-length: 5\r\n\r\nhello"
        assert (bodyOf 16 r = Ok "hello"))
    test "a body read with the head, in pieces smaller than it" (fun () ->
        let asked = noAsk ()
        let r = ok (parseRead (socket [] asked) "POST / HTTP/1.1\r\ncontent-length: 10\r\n\r\n0123456789")
        assert (bodyOf 3 r = Ok "0123456789")
        assert (asked.Value = []))
    test "a body that arrives after the head comes from the socket" (fun () ->
        let r = ok (parseRead (socket [ "hel"; "lo" ] (noAsk ())) "POST / HTTP/1.1\r\ncontent-length: 5\r\n\r\n")
        assert (bodyOf 16 r = Ok "hello"))
    test "part of the body with the head, the rest from the socket" (fun () ->
        let r = ok (parseRead (socket [ "llo"; " world" ] (noAsk ())) "POST / HTTP/1.1\r\ncontent-length: 11\r\n\r\nhe")
        assert (bodyOf 4 r = Ok "hello world"))
    test "the socket is never asked for more than what is left of the body" (fun () ->
        let asked = noAsk ()
        let r = ok (parseRead (socket [ "abc"; "defgh"; "NEXT REQUEST" ] asked) "POST / HTTP/1.1\r\ncontent-length: 10\r\n\r\nxy")
        assert (bodyOf 64 r = Ok "xyabcdefgh")
        // 8 left after the buffer's 2; 5 after "abc"; none asked once the body is complete
        assert (asked.Value = [ 8; 5 ]))
    test "a pipelined request after the body is not part of it" (fun () ->
        let r = get "POST / HTTP/1.1\r\ncontent-length: 3\r\n\r\nabcGET / HTTP/1.1\r\n\r\n"
        assert (bodyOf 64 r = Ok "abc"))
    test "after the body, reads give 0" (fun () ->
        let r : Http.Request = get "POST / HTTP/1.1\r\ncontent-length: 2\r\n\r\nok"
        let dest = Buffer.create 8 (u8 0)
        assert (Task.run (r.body.read dest) = Ok 2)
        assert (Task.run (r.body.read dest) = Ok 0)
        assert (Task.run (r.body.read dest) = Ok 0))
    test "a socket closed before the body ended is an error" (fun () ->
        let r = ok (parseRead (socket [ "ab" ] (noAsk ())) "POST / HTTP/1.1\r\ncontent-length: 5\r\n\r\n")
        assert (bodyOf 16 r = Error "connection closed before the body ended"))
    test "a socket error is the body's error" (fun () ->
        let r = ok (parseRead (failing "reset") "POST / HTTP/1.1\r\ncontent-length: 5\r\n\r\n")
        assert (bodyOf 16 r = Error "reset"))
    test "the body read with the head is a copy: reusing the buffer does not change it" (fun () ->
        let buffer = Buffer.create 64 (u8 0)
        let raw = String.bytes "POST / HTTP/1.1\r\ncontent-length: 4\r\n\r\nbody"
        let filled = Span.length raw
        Buffer.blit raw 0 buffer 0 filled
        let e = Option.get (Helpers.RequestParser.headEnd buffer 0 filled)
        let r = ok (Helpers.RequestParser.parse buffer (Buffer.create 256 (u8 0)) (Buffer.create 0 (u8 0)) untouched e filled)
        // zerver drains into the same buffer, and reads the next head into it
        for i in 0 .. 63 do buffer.[i] <- u8 88
        assert (bodyOf 16 r = Ok "body"))
    test "the body length is the content-length, not what came with the head" (fun () ->
        let r = ok (parseRead (socket [ "cd" ] (noAsk ())) "POST / HTTP/1.1\r\ncontent-length: 4\r\n\r\nab")
        assert (r.body.length = 4)
        assert (bodyOf 1 r = Ok "abcd"))
    test "binary bytes in the body are kept" (fun () ->
        let raw = String.bytes "POST / HTTP/1.1\r\ncontent-length: 256\r\n\r\n"
        let buffer = Buffer.create 1024 (u8 0)
        let headLen = Span.length raw
        Buffer.blit raw 0 buffer 0 headLen
        for i in 0 .. 255 do buffer.[headLen + i] <- u8 i
        let filled = headLen + 256
        let e = Option.get (Helpers.RequestParser.headEnd buffer 0 filled)
        let r = ok (Helpers.RequestParser.parse buffer (Buffer.create 256 (u8 0)) (Buffer.create 0 (u8 0)) untouched e filled)
        let dest = Buffer.create 256 (u8 0)
        assert (Task.run (r.body.read dest) = Ok 256)
        for i in 0 .. 255 do assert (dest.[i] = u8 i))
]

// Generated requests: tokens from a safe alphabet, so each one is a valid request.
let private tokenChars = String.chars "abcxyzABCXYZ019-_."
let private token = Gen.listOfLength 1 (Gen.elements tokenChars) |> Gen.bind (fun first -> Gen.list (Gen.elements tokenChars) |> Gen.map (fun rest -> String.ofChars (first @ rest)))
let private valueChars = String.chars "abc XYZ019-_.:;/=,\"!"

type Generated = { method: string; path: string; query: string; headers: list<string * string>; body: string }

let private generated : Gen<Generated> =
    Gen.elements [ "GET"; "POST"; "PUT"; "DELETE"; "PATCH" ] |> Gen.bind (fun m ->
    token |> Gen.bind (fun path ->
    Gen.list (Gen.elements tokenChars) |> Gen.bind (fun query ->
    Gen.list (Gen.listOfLength 2 token |> Gen.bind (fun nv ->
        Gen.list (Gen.elements valueChars) |> Gen.map (fun v -> (List.head nv, String.trim (String.ofChars v))))) |> Gen.bind (fun hs ->
    Gen.list (Gen.elements tokenChars) |> Gen.map (fun body ->
        { method = m; path = "/" + path; query = String.ofChars query; headers = List.filter (fun (n, _) -> String.toLower n <> "content-length" && String.toLower n <> "transfer-encoding") hs; body = String.ofChars body })))))

let private render (g: Generated) : string =
    let q = if g.query = "" then "" else "?" + g.query
    let hs = g.headers |> List.map (fun (n, v) -> $"{n}: {v}\r\n") |> String.concat
    $"{g.method} {g.path}{q} HTTP/1.1\r\n{hs}content-length: {String.length g.body}\r\n\r\n{g.body}"

let private methodName (m: Http.Method) =
    match m with
    | Http.Get -> "GET"
    | Http.Head -> "HEAD"
    | Http.Post -> "POST"
    | Http.Put -> "PUT"
    | Http.Delete -> "DELETE"
    | Http.Options -> "OPTIONS"
    | Http.Other s -> s

let private roundTrips (g: Generated) : bool =
    match parse (render g) with
    | Error _ -> false
    | Ok r ->
        methodName r.method = g.method
        && r.path = g.path
        && r.query = g.query
        && r.version = Http.Http1_1
        && r.headers = List.map (fun (n, v) -> (String.toLower n, v)) g.headers @ [ ("content-length", string (String.length g.body)) ]
        && bodyOf 7 r = Ok g.body

// Any bytes at all: parse gives Ok or Error, and never panics (a bad slice, say).
let private neverPanics (bytes: array<u8>) : bool =
    let buffer = Buffer.create (Array.length bytes + 4) (u8 0)
    for i in 0 .. Array.length bytes - 1 do buffer.[i] <- bytes.[i]
    let n = Array.length bytes
    // the blank line, so there is always a head to parse
    for (i, b) in [ (0, 13); (1, 10); (2, 13); (3, 10) ] do buffer.[n + i] <- u8 b
    let filled = n + 4
    match Helpers.RequestParser.headEnd buffer 0 filled with
    | None -> false
    | Some e ->
        match Helpers.RequestParser.parse buffer (Buffer.create 8 (u8 0)) (Buffer.create 0 (u8 0)) untouched e filled with
        | Ok _ -> true
        | Error _ -> true

// Bytes biased to what a request is made of, so the parser's branches are reached.
let private requestish : Gen<array<u8>> =
    Gen.list (Gen.frequency [
        (6, Gen.elements (String.chars "GET /a?b HTTP/1.01:x") |> Gen.map (fun c -> u8 (int c)))
        (2, Gen.elements [ u8 13; u8 10; u8 32; u8 9; u8 58 ])
        (1, Gen.u8 ()) ])
    |> Gen.map Array.ofList

[<Derive(Arbitrary)>]
type Bytes = Bytes of list<u8>

let propertyTests = testList "properties" [
    testPropertyWith { runs = 500 } "a generated request parses back to what was generated" (fun (seed: u64) ->
        Gen.sample seed 1 generated |> List.forall roundTrips)
    testPropertyWith { runs = 2000 } "arbitrary bytes never panic the parser" (fun (Bytes bytes) ->
        neverPanics (Array.ofList bytes))
    testPropertyWith { runs = 2000 } "request-like bytes never panic the parser" (fun (seed: u64) ->
        Gen.sample seed 1 requestish |> List.forall neverPanics)
    testPropertyWith { runs = 500 } "a resumed head scan finds what a full scan does" (fun (seed: u64) ->
        Gen.sample seed 1 requestish |> List.forall (fun bytes ->
            let b = Buffer.ofSpan bytes
            let n = Buffer.length b
            let full = Helpers.RequestParser.headEnd b 0 n
            List.forall (fun from -> Helpers.RequestParser.headEnd b 0 from <> None || Helpers.RequestParser.headEnd b from n = full) [ 0 .. n ]))
]

[<Tests>]
let tests = testList "requestParser" [
    headEndTests
    methodTests
    versionTests
    requestLineTests
    urlTests
    headerTests
    contentLengthTests
    bodyTests
    propertyTests
]
