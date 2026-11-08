# Z# libraries

A set of libraries for Z#, the language in `../zsharp`, written from scratch. Each library is
its own Z# package in a top-level folder.

| Package | What |
|---|---|
| `zerver/` | HTTP server |
| `zhttp/` | HTTP client |
| `zlog/` | Logging |
| `zjson/` | JSON serialization |
| `zapi/` | Typed routing on top of zerver, in the spirit of minimal APIs |
| `taskResult/` | `Task<result<_, _>>` helpers |
| `zconfig/` | Typed configuration from values, env variables and JSON files |

## Working mode

The owner writes the libraries themselves. Agents give targeted help: answer the specific
question, explain a Z# feature, review a piece of code, suggest a small snippet. Do not write
whole modules or features unless explicitly asked.

## Layout

Each package:

- `zs.toml` — the manifest; `main` is the library's root module, seen by dependents under the
  package name (`zerver` → `Zerver`). Other files under `src/` are modules named by their path
  (`src/Helpers/requestParser.zs` → `Zerver.Helpers.RequestParser` for a dependent).
- `examples/<name>/` — a runnable program with its own `zs.toml`, depending on the library by
  path (`zerver = { path = "../.." }`).
- `tests/` — `[<Tests>]` files that `zs test` runs, and tests outside Z#, e.g. `zerver/tests/e2e/`
  (Node, `npm run test:e2e` from `zerver/`, against a server running on `127.0.0.1:8080` or
  `ZERVER_URL`). A test file is a module named by its file, so it must not share a name with a
  module of the package (`tests/requestParser.zs` would shadow `RequestParser`).

## Commands

Run from a package or example folder:

```sh
zs run            # build and run a debug build (an example)
zs test           # run the [<Tests>] lists
zs build --release
```

`zs` calls the first `clang` on PATH, and code with tasks needs LLVM 23 or later: Apple's clang
is too old, so Homebrew's `llvm` (`/opt/homebrew/opt/llvm/bin`) must come first. `zs doctor`
checks this.

## Z# references

- `../zsharp/README.md` — the language at a glance, differences from F#.
- `../zsharp/docs/guide/` — the guide; `17-from-fsharp.md` for F# habits.
- `../zsharp/examples/echo/` — a TCP server and client, the closest starting point.
- `../zsharp/lib/std/` — the standard library sources: check here before assuming an API exists.

Z# looks like F# but is not F#: no null, no exceptions (`result` or the `Throw` effect),
interpolation instead of `printfn`, integer overflow panics. Verify syntax and library calls
against the sources above rather than F# memory.

## Conventions

- Everything in code is English: identifiers, comments, commit messages.
- Commit only when asked.
