// Helpers for the socket tests: a loopback client, and zerver run beside it.
open Zerver

let textOf (b: Buffer<u8>) (n: int) : string =
    let view : span<u8> = b
    if n = 0 then "" else String.ofBytes view.[0 .. n - 1]

let millis () : int = int (Runtime.ticks () / u64 1000000)

/// `s` repeated `n` times, in linear time (String.repeat appends one at a time).
let repeat (n: int) (s: string) : string = String.concat (List.replicate n s)

// Chunks are kept in a list and joined once, so a large response is not copied per read.
let rec private readLoop (s: TcpStream) (buf: Buffer<u8>) (chunks: list<string>) (enough: string -> bool) : Task<result<string, string>> = task {
    let acc = String.concat (List.rev chunks)
    if enough acc then
        return Ok acc
    else
        let! n = Tcp.read s buf
        match n with
        | Error e -> return Error $"{e} after {String.length acc} bytes"
        | Ok 0 -> return Ok acc
        | Ok n -> return! readLoop s buf (textOf buf n :: chunks) enough
}

let rec private readAll (s: TcpStream) (buf: Buffer<u8>) (chunks: list<string>) : Task<result<string, string>> = task {
    let! n = Tcp.read s buf
    match n with
    | Error e -> return Error $"{e} after {List.length chunks} reads"
    | Ok 0 -> return Ok (String.concat (List.rev chunks))
    | Ok n -> return! readAll s buf (textOf buf n :: chunks)
}

/// Everything the peer sends until it closes, or `Error` when that takes longer than `ms`.
let readToEnd (ms: int) (s: TcpStream) : Task<result<string, string>> = task {
    let! r = Task.timeout ms (readAll s (Buffer.create 65536 (u8 0)) [])
    match r with
    | Some r -> return r
    | None -> return Error $"the peer did not close within {ms} ms"
}

/// What the peer sends until `enough` holds of it (or it closes), or `Error` after `ms`.
let readUntil (ms: int) (enough: string -> bool) (s: TcpStream) : Task<result<string, string>> = task {
    let! r = Task.timeout ms (readLoop s (Buffer.create 65536 (u8 0)) [] enough)
    match r with
    | Some r -> return r
    | None -> return Error $"no complete answer within {ms} ms"
}

let send (s: TcpStream) (text: string) : Task<result<unit, string>> = Tcp.writeText s text

let connect (port: int) : Task<result<TcpStream, string>> = Tcp.connect "127.0.0.1" port

/// How many times `sub` occurs in `s`.
let count (sub: string) (s: string) : int =
    let rec go (from: int) (n: int) =
        let i = String.indexOfFrom s sub from
        if i < 0 then n else go (i + String.length sub) (n + 1)
    go 0 0

/// Runs `client` against zerver on `port` with `handler`; zerver stops when the client is done.
let withServer (config: ZerverOptions) (handler: share<HttpHandler>) (client: unit -> Task<result<'a, string>>) : Task<result<'a, string>> =
    let server = serve config handler |> Task.map (fun r ->
        match r with
        | Ok () -> Error "zerver stopped before the client was done"
        | Error e -> Error $"zerver: {e}")
    Zlog.LoggerConfig.empty |> Zlog.LoggerConfig.runWith (fun () -> Task.race server (client ()))

/// A response read back from the wire.
type Reply = { status: string; headers: list<string * string>; body: string }

let header (name: string) (r: Reply) : option<string> =
    List.tryFind (fun (n, _) -> n = name) r.headers |> Option.map snd

/// The responses in what a connection sent, each body cut at its content-length; `Error` for
/// bytes that are not a whole response.
let replies (raw: string) : result<list<Reply>, string> =
    let rec go (at: int) (acc: list<Reply>) =
        if at >= String.length raw then
            Ok (List.rev acc)
        else
            let headEnd = String.indexOfFrom raw "\r\n\r\n" at
            if headEnd < 0 then
                Error $"no end of head in {String.sub raw at (String.length raw - at)}"
            else
                match String.split "\r\n" (String.sub raw at (headEnd - at)) with
                | [] -> Error "empty head"
                | status :: lines ->
                    let headers =
                        lines |> List.map (fun line ->
                            match String.indexOf ": " line with
                            | Some i -> (String.toLower (String.sub line 0 i), String.sub line (i + 2) (String.length line - i - 2))
                            | None -> (line, ""))
                    let length =
                        match List.tryFind (fun (n, _) -> n = "content-length") headers with
                        | Some (_, v) -> Option.defaultValue -1 (String.parseInt v)
                        | None -> -1
                    let bodyAt = headEnd + 4
                    if length < 0 then
                        Error $"no content-length in {status}"
                    else if bodyAt + length > String.length raw then
                        Error $"body cut short in {status}"
                    else
                        go (bodyAt + length) ({ status = status; headers = headers; body = String.sub raw bodyAt length } :: acc)
    go 0 []
