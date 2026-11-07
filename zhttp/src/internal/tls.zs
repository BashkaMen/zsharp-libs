// HTTPS through the C wrappers in c/tls.c. `until` is the same deadline `Sock.wait` uses.
open Sock

extern zs_tls_client_ctx : cstr -> i32 -> u64
extern zs_tls_server_ctx : cstr -> cstr -> u64
extern zs_tls_ctx_free : u64 -> unit
extern zs_tls_client : u64 -> i32 -> cstr -> u64
extern zs_tls_server : u64 -> i32 -> u64
extern zs_tls_free : u64 -> unit
extern zs_tls_handshake : u64 -> i32 -> i32
extern zs_tls_read : u64 -> span<u8> -> i32
extern zs_tls_write : u64 -> span<u8> -> i32
extern zs_tls_last_error : unit -> cstr

let private err () = String.ofCStr (zs_tls_last_error ())

let clientContext (caFile: string) (verify: bool) : result<u64, string> =
    let ctx = zs_tls_client_ctx caFile (if verify then i32 1 else i32 0)
    if ctx = u64 0 then Error (err ()) else Ok ctx

let serverContext (certFile: string) (keyFile: string) : result<u64, string> =
    let ctx = zs_tls_server_ctx certFile keyFile
    if ctx = u64 0 then Error (err ()) else Ok ctx

let freeContext (ctx: u64) = if ctx <> u64 0 then zs_tls_ctx_free ctx

let client (ctx: u64) (fd: int) (host: string) : result<u64, string> =
    let ssl = zs_tls_client ctx (i32 fd) host
    if ssl = u64 0 then Error (err ()) else Ok ssl

let server (ctx: u64) (fd: int) : result<u64, string> =
    let ssl = zs_tls_server ctx (i32 fd)
    if ssl = u64 0 then Error (err ()) else Ok ssl

let free (ssl: u64) = if ssl <> u64 0 then zs_tls_free ssl

let rec handshake (ssl: u64) (fd: int) (serverSide: bool) (until: option<u64>) : Task<result<unit, string>> = task {
    let code = zs_tls_handshake ssl (if serverSide then i32 1 else i32 0)
    if code = i32 1 then
        return Ok ()
    else if code = i32 2 || code = i32 3 then
        let! ready = wait fd (code = i32 3) until
        match ready with
        | Error e -> return Error e
        | Ok () -> return! handshake ssl fd serverSide until
    else
        return Error (err ())
}

let rec read (ssl: u64) (fd: int) (buf: Buffer<u8>) (n: int) (until: option<u64>) : Task<result<int, string>> = task {
    let take = min n (Buffer.length buf)
    if take <= 0 then
        return Ok 0
    else
        let code = zs_tls_read ssl (buf.[0 .. take - 1])
        if code > i32 0 then
            return Ok (int code)
        else if code = i32 0 then
            return Ok 0
        else if code = i32 -2 || code = i32 -3 then
            let! ready = wait fd (code = i32 -3) until
            match ready with
            | Error e -> return Error e
            | Ok () -> return! read ssl fd buf n until
        else
            return Error (err ())
}

let rec writeSpan (ssl: u64) (fd: int) (data: span<u8>) (until: option<u64>) : Task<result<unit, string>> = task {
    let n = Span.length data
    if n = 0 then
        return Ok ()
    else
        let code = zs_tls_write ssl data
        if code > i32 0 then
            let wrote = int code
            if wrote >= n then
                return Ok ()
            else
                return! writeSpan ssl fd (data.[wrote .. n - 1]) until
        else if code = i32 -2 || code = i32 -3 then
            let! ready = wait fd (code = i32 -3) until
            match ready with
            | Error e -> return Error e
            | Ok () -> return! writeSpan ssl fd data until
        else
            return Error (err ())
}
