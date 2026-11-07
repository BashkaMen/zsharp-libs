open Zjson

type Method = Get | Head | Post | Put | Delete | Options | Other of string
type Version = Http1_0 | Http1_1
type Head = {
    method: Method;
    headers: list<string * string>;
    version: Version;
    url: string
}

type Body = {
    length: int
    read: Buffer<u8> -> Task<result<int, string>>
}

type Request = {
    method: Method
    path: string
    query: string
    version: Version
    headers: list<string * string>
    body: Body
}

type Send = Buffer<u8> -> int -> Task<result<unit, string>>

type ResponseBody = {
    length: int
    write: Send -> Task<result<unit, string>>
}

type Response = {
    code: int
    headers: list<string * string>
    body: ResponseBody
}

module Body =
    let empty : Body = { length = 0; read = fun buffer -> Task.ret (Ok 0) }

module ResponseBody =
    let empty () : ResponseBody = { length = 0; write = fun send -> Task.ret (Ok ()) }

    let fromText (txt: string) : ResponseBody =
        let bytes = Buffer.ofSpan (String.bytes txt)
        { length = Buffer.length bytes; write = fun send -> send bytes (Buffer.length bytes) }

    let fromJson (value: 'a) : ResponseBody =
        let w = JsonWriter.create 4096
        serialize w value
        { length = w.len.Value; write = fun send -> send w.buf.Value w.len.Value }

module Response =
    let empty () : Response = { code = 200; headers = []; body = ResponseBody.empty () }
    let withCode code res = { res with code = code }
    let withBody body res = { res with body = body }
    let withHeader key value res = { res with headers = (key, value) :: res.headers }
    let withJson code json res =
        { res with
            code = code
            headers = ("content-type", "application/json") :: res.headers
            body = ResponseBody.fromJson json }
    let withJsonOf code (value: 'a) res = withJson code value res
