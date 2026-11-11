<div align="center">

# zsharp-libs

**An HTTP server and client, typed endpoints, JSON, logging, configuration and a Postgres client for [Z#](https://github.com/ZigoteProject/zsharp), written from scratch.**

![zs](https://img.shields.io/badge/zs-0.7.1-7c3aed)
![packages](https://img.shields.io/badge/packages-8-2563eb)
![platform](https://img.shields.io/badge/tested%20on-macOS%20arm64-111827)

[zerver](zerver/README.md) · [zhttp](zhttp/README.md) · [zapi](zapi/README.md) · [zjson](zjson/README.md) · [zlog](zlog/README.md) · [taskResult](taskResult/README.md) · [zconfig](zconfig/README.md) · [zpg](zpg/README.md)

</div>

---

## Quick look

A JSON endpoint on every core, with each request logged:

```fsharp
open Zerver
open Zjson
open Zlog

[<Derive(Serialize, Deserialize)>]
type Hello = { hello: string }

let handler (req: Http.Request) = task {
    Logger.info (event "request" [ ("path", req.path) ])
    match req.path with
    | "/hello" -> return Http.Response.empty () |> Http.Response.withJsonOf 200 { hello = "world" }
    | _ -> return Http.Response.empty () |> Http.Response.withCode 404
}

let main () =
    let app =
        LoggerConfig.empty
        |> LoggerConfig.withSink (Sinks.Console.create Format.text)
        |> LoggerConfig.runWith (fun () -> Zerver.serve Zerver.defaultConfig handler)
    match Task.run app with
    | Error e ->
        eprintln $"app: {e}"
        1
    | Ok () -> 0
```

```text
$ curl localhost:8080/hello
{"hello":"world"}

2026-11-07T15:49:18.173188Z INFO listening host=0.0.0.0 port=8080 workers=18
2026-11-07T15:49:18.204421Z INFO request path=/hello
```

## Packages

| | Package | What it gives you |
|---|---|---|
| 🌐 | **[zerver](zerver/README.md)** | HTTP/1.1 server with keep-alive and an accept loop on every core (`workers`, the core count by default). Handlers are `Http.Request -> Task<Http.Response>`. |
| 📡 | **[zhttp](zhttp/README.md)** | HTTP/1.1 client. Immutable `Request` and `Response`, a pooled `Client`, `Task<result<Response, HttpError>>`, middleware as `Send -> Send`, and HTTPS through OpenSSL. |
| 🧭 | **[zapi](zapi/README.md)** | Typed endpoints on top of zerver: a request record bound from path, query, headers and body by a derive, a `Task<result<Reply<_>, _>>` handler, method, path and middleware declared next to it. |
| 🧩 | **[zjson](zjson/README.md)** | JSON through `Serialize` / `Deserialize` derives, written straight to bytes and read straight from bytes, with no `Json` tree in between. Errors name where a value didn't fit (`at [1].id`). |
| 📝 | **[zlog](zlog/README.md)** | Structured logging: `Logger.info`, `warn`, `error` from any task on any thread, context with `Logger.scope`, console and file sinks. |
| 🔗 | **[taskResult](taskResult/README.md)** | A `taskResult { }` builder for `Task<result<_, _>>`: `let!` stops at the first `Error`. |
| ⚙️ | **[zconfig](zconfig/README.md)** | Typed configuration: values, environment variables (`Section__Key`) and JSON files merged by `ConfigReader`, read into a record with a `FromConfig` derive. |
| 🐘 | **[zpg](zpg/README.md)** | Postgres through libpq: a pool, parameterized queries, rows into records, transactions; timestamps, dates, intervals, jsonb through zjson, bytea and arrays. |

## Performance

<table>
<tr><th>zerver, requests/s</th><th>zjson vs std <code>Json</code></th></tr>
<tr><td>

| route | zerver | Bun | Node | .NET | Oxpecker |
|---|---|---|---|---|---|
| `/json` 18 B | 154k | **224k** | 178k | 130k | 132k |
| `/object` 1.4 KB | 155k | **204k** | 156k | 129k | 130k |
| `/list` 141 KB | **40.6k** | 15.8k | 12.0k | 38.6k | 38.7k |
| peak RSS, `/list` | **18 MB** | 28 MB | 139 MB | 280 MB | 300 MB |

</td><td>

| case | speedup |
|---|---|
| write, 1000 users | **3.8×** |
| write, 100 posts | **4.5×** |
| read, 1000 users | **1.8×** |
| read, 100 posts | **1.4×** |

</td></tr>
</table>

`wrk -t4 -c64 -d10s`, Apple M5 Pro (18 cores), server and `wrk` on one machine. Methods and every
number: [zerver](zerver/README.md#benchmarks), [zjson](zjson/README.md#benchmarks).

## Getting started

**1. Toolchain.** `zs` 0.7.1, with LLVM 23 or later first on `PATH` (tasks need it; Apple's clang is too old):

```sh
export PATH=/opt/homebrew/opt/llvm/bin:$PATH
zs doctor
```

zhttp links OpenSSL through `pkg-config`: `brew install pkgconf openssl` on macOS, `apt install pkg-config libssl-dev` on Debian.

**2. Dependencies.** Clone this repository next to your project and add the packages by path:

```toml
[dependencies]
zerver     = { path = "../zsharp-libs/zerver" }
zhttp      = { path = "../zsharp-libs/zhttp" }
zapi       = { path = "../zsharp-libs/zapi" }
zjson      = { path = "../zsharp-libs/zjson" }
zlog       = { path = "../zsharp-libs/zlog" }
taskResult = { path = "../zsharp-libs/taskResult" }
zconfig    = { path = "../zsharp-libs/zconfig" }
zpg        = { path = "../zsharp-libs/zpg" }
```

Each package's root module is its name: `open Zerver`, `open Zhttp`, `open Zapi`, `open Zjson`, `open Zlog`, `open TaskResult`, `open Zconfig`, `open Zpg`.

> A git dependency (`{ git = "…" }`) reads the `zs.toml` at a repository's root, so it can't reach
> these folders yet: use a path.

**3. Run something.**

```sh
cd zlog/examples/basic && zs run            # logging to the console and a file
cd zerver/examples/echo && zs run           # an echo server on :8080
cd zhttp/examples/get && zs run             # GET https://example.com/
cd zapi/examples/users && zs run            # typed endpoints on :8080
cd zjson && zs test                         # zjson's tests
cd zerver && zs test                        # zerver's tests: parser, response writer, server over loopback
cd zhttp && zs test                         # zhttp's tests
cd taskResult && zs test                    # taskResult's tests
cd zconfig/examples/basic && zs run         # config from json, values and env
cd zpg && zs test                           # zpg's tests, against a local Postgres
```

## Repository

```text
zerver/       HTTP server         examples/echo, examples/bench (with Bun, Node, .NET and Oxpecker), tests/, tests/e2e
zhttp/        HTTP client         examples/get, tests/
zapi/         typed endpoints     examples/users
zjson/        JSON                tests/ (read, write, bench)
zlog/         logging             examples/basic
taskResult/   Task<result> CE     tests/
zconfig/      configuration       examples/basic, tests/
zpg/          Postgres client     examples/basic, tests/ (a running Postgres)
```

zerver's end-to-end tests run against the echo example:

```sh
cd zerver/examples/echo && zs build --release && ./build/echo &
cd zerver && npm run test:e2e
```

## Known issues

- zs 0.7.0 crashes on a record with a `share<F>` field. A fix is proposed upstream in
  [ZigoteProject/zsharp#1](https://github.com/ZigoteProject/zsharp/pull/1); until it lands, the
  libraries keep `share` functions in parameters and tuples.
- Measured on macOS arm64 only.
