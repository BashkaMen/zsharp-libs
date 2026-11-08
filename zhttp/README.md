# zhttp

HTTP client for Z#: immutable requests, a client that keeps connections open, and `Task<result<Response, HttpError>>`.

## Usage

```fsharp
open Zhttp
open Zlog

[<Derive(Serialize, Deserialize)>]
type Hello = { hello: string }

let fetch (url: string) : Task<result<Hello, HttpError>> = task {
    let send =
        Client.create ()
        |> Client.withDefaults (fun d -> { d with timeoutMs = Some 5000; maxRedirects = 4 })
        |> Client.sender
        |> Middleware.retry 2
        |> Middleware.log
    let req =
        Request.get url
        |> Request.header "accept" "application/json"
        |> Request.timeout 2000
    let! res = send req
    match res with
    | Error e -> return Error e
    | Ok res -> return! Response.json res
}

let main () =
    let run =
        LoggerConfig.empty
        |> LoggerConfig.withSink (Sinks.Console.create Format.text)
        |> LoggerConfig.runWith (fun () -> fetch "https://example.com/")
    match Task.run run with
    | Error e ->
        eprintln $"get: {message e}"
        1
    | Ok hello ->
        println hello.hello
        0
```

`Request.get url |> Request.header k v |> Request.timeout ms |> Request.json value` builds a value. Nothing is sent until `Client.send`. `Response.text`, `Response.bytes` and `Response.json` read the body; `body.read` streams it into a `Buffer`.

| | |
|---|---|
| `Client.create` | a pool, a 30 second deadline, up to 10 redirects, certificate checks on |
| `Client.withDefaults` | a new client; the function edits the defaults record. The new client has its own pool |
| `Client.send client req` | `Task<result<Response, HttpError>>` |
| `Client.sender` | that function as a `Send`, so middleware can wrap it |
| `Middleware.retry n` | `Send -> Send`, again up to `n` times on `Dns`, `Connect` and `Timeout`, for `GET`, `HEAD`, `OPTIONS`, `PUT` and `DELETE` |
| `Middleware.log` | the method, url, status or error, and milliseconds, through zlog |
| `Http.get url` | `GET` on one process-wide client, for a quick call |

`HttpError` is `Dns`, `Connect`, `Tls`, `Timeout`, `Protocol` or `Redirect`.

A client is safe to share with other tasks and threads. Its idle connections and its TLS context sit behind a mutex. `withTimeout`, `withMaxRedirects`, `withCaFile`, `withVerification` and `withHeader` are `withDefaults` for one field.

`Request.text` and `Request.json` set the content type. `Request.body` and `Request.bytes` set the bytes and leave the content type alone. A request's `timeout` replaces the client's; `Request.noTimeout` clears it.

Redirects follow `301`, `302`, `303`, `307` and `308`, up to `maxRedirects` (`0` returns the redirect). `303`, and `301` or `302` of a method other than `GET` or `HEAD`, are sent again as `GET` without the body. `307` and `308` keep the method and the body. A redirect to another host drops `authorization`, `cookie` and `proxy-authorization`.

HTTPS uses OpenSSL. Verification is on unless `withVerification false`: the system CA store, plus `withCaFile` (a PEM file), SNI, and a check that the certificate names the host. The minimum version is TLS 1.2. The link needs OpenSSL found by `pkg-config openssl`: `brew install pkgconf openssl` on macOS, `apt install pkg-config libssl-dev` on Debian.

## What a call does

HTTP/1.1. The request is `content-length` when it has a body. The response may be `content-length` or chunked; trailers are read and dropped. A body with neither, on a response that has one, is read until the server closes.

Idle connections are kept, up to 8 per host, and taken again for the same scheme, host and port. A reused connection that fails before a response is tried once more when the method is idempotent. `Connection: close`, from the server or from the request, is not kept.

The socket is non-blocking. A read or write that would block, including `SSL_ERROR_WANT_READ` and `SSL_ERROR_WANT_WRITE`, waits with `Task.waitFor`. The deadline covers the connect, the handshake and every later read.

`use` on a response closes the connection when the body was not finished. Reading to the end puts a keep-alive connection back in the pool.

## Sharing the types with zerver

`Request` and `Response` are plain values: no socket and no TLS handle in the record. The body is the same shape as zerver's, a length and a `Buffer<u8> -> Task<result<int, _>>`. zerver is unchanged. A later package could hold the method, the headers and that body, and both the client and the server could use it. They are not the same types today: a server request has a path and a body it reads, and a client request has a URL and a body it already holds.

## Limitations

- HTTP/1.1 only. No HTTP/2, HTTP/3, proxy, cookies, or multipart helper.
- A request body is buffered and sent with `content-length`. A response body streams. `Response.text`, `bytes` and `json` refuse a body over 10 MB; `body.read` does not.
- No `gzip` or other content encoding. The client sends `accept-encoding: identity` unless the request sets one.
- Redirect targets are not normalized: a `..` segment is left as it was. Only `http` and `https` are followed.
- No client certificates, and no verify callback of your own. `withCaFile` adds a PEM file to the system store; it does not replace it.
- The pool and the `SSL_CTX` live until the process exits. There is no finalizer. `withDefaults` does not keep the previous client's pool.
- The name is looked up on the calling thread, inside the connect. `Dns.lookup` needs the runtime's one helper thread, and zlog's `runWith` keeps that thread waiting for log lines, so a lookup there would never start. A slow resolver stalls other tasks on that event loop. The first address the resolver returns is the one that is used.
- zs 0.7.0 cannot put a C pointer, or a `share<F>` field, in a value that crosses threads. The pool stores an `SSL` as an integer. Middleware stays a function, not a field of the client.
- Read a response on the task that received it. `body.read` waits on that task's event loop.
- A stale pooled connection is retried once for an idempotent method. A `POST` that fails on a reused connection is returned as `Connect`; the server may already have seen it.
- Response trailers are discarded. One request at a time on a connection.

## Tests and the example

The tests speak HTTP to a small server in-process, HTTPS to that server with a certificate this process generates, and once to `https://example.com/`. A machine with no network still passes that last test when the failure is DNS, connect or a timeout.

```sh
cd zhttp && zs test
cd zhttp/examples/get && zs run
cd zhttp/examples/get && zs run -- http://127.0.0.1:8080/echo
```

`zs` 0.7.0, with LLVM 23 or later first on `PATH`. The example logs through zlog and prints the status and the body.
