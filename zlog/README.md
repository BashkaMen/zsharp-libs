# zlog

Structured logging for Z#: a message with key-value fields, from any task on any thread, written
to sinks by one task beside the program.

## Usage

```fsharp
open Zlog

let work () : Task<int> = task {
    Logger.info "started"
    Logger.info (event "request" [ ("path", "/echo"); ("ms", "12") ])
    let log = Logger.scope [ ("request_id", "1") ]
    log.warn "slow request"
    return 0
}

let main () =
    match Sinks.File.create Format.text "app.log" with
    | Error e ->
        eprintln $"app: {e}"
        1
    | Ok file ->
        LoggerConfig.empty
        |> LoggerConfig.withMinLevel Info
        |> LoggerConfig.withSink (Sinks.Console.create Format.text)
        |> LoggerConfig.withSink file
        |> LoggerConfig.runWith work
        |> Task.run
```

```text
2026-11-07T15:15:53.150115Z INFO started
2026-11-07T15:15:53.150176Z INFO request path=/echo ms=12
2026-11-07T15:15:53.150202Z WARN slow request request_id=1
```

- **Levels:** `Logger.debug`, `info`, `warn` and `error` (`Fail`, printed as `ERROR`). `LoggerConfig.empty` starts at `Info`; a call below the minimum level builds nothing.
- **What can be logged:** a `string`, an `Event` made with `event message fields`, or any type with an `impl Loggable` that turns it into a message and fields:

  ```fsharp
  type SlowRequest = { path: string; ms: int }

  impl Loggable<SlowRequest> =
      let toEvent (e: SlowRequest) = ("slow request", [ ("path", e.path); ("ms", string e.ms) ])

  Logger.warn { path = "/echo"; ms = 120 }
  ```

- **Context:** `Logger.scope fields` gives a `Logger` value whose `debug`, `info`, `warn` and `error` add those fields to every entry; `log.scope more` adds more. It holds only the fields, so it can be passed anywhere, other threads included.
- **Delivery:** a call checks the level, builds the entry and sends it to one process-wide `Channel`. `runWith` turns logging on at the config's level, runs a task that takes each entry and writes and flushes it to every sink, and at `body`'s end turns logging off and writes what's left before returning.

## Sinks

| Sink | |
|---|---|
| `Sinks.Console.create format` | `Warn` and `Fail` to stderr, the rest to stdout |
| `Sinks.File.create format path` | appends to a file; a `result`, since the file may not open |

A sink is a record of two functions, so a new one is `{ write = fun entry -> …; flush = fun () -> … }`.
`Format.text` is the only format: time, level, message, then `key=value` fields.

## Limitations

- One logger per process. Outside `runWith` logging is off and a call does nothing.
- Each entry crosses to a helper thread (`Task.blocking`) to be taken from the channel, and a full channel (4096 entries) makes the caller wait: fine for events and errors, to be measured before logging every request.
- The console sink writes with `println`, which C buffers when stdout isn't a terminal, so entries still in that buffer are lost when the process is killed.

## Example

```sh
cd examples/basic
zs run
```
