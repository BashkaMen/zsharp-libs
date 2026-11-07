open Zlog

type SlowRequest = { path: string; ms: int }

impl Loggable<SlowRequest> =
    let toEvent (e: SlowRequest) = ("slow request", [ ("path", e.path); ("ms", string e.ms) ])

let handle (path: string) : Task<unit> = task {
    let log = Logger.scope [ ("request_id", "1") ]
    log.info "request started"
    do! Task.sleep 10
    log.warn { path = path; ms = 120 }
}

let work () : Task<int> = task {
    Logger.debug "hidden below the minimum level"
    Logger.info (event "started" [ ("pid", "42") ])
    do! handle "/echo"
    Logger.error (event "connection failed" [ ("error", "reset by peer") ])
    return 0
}

let main () =
    match Sinks.File.create Format.text "basic.log" with
    | Error e ->
        eprintln $"basic: {e}"
        1
    | Ok file ->
        LoggerConfig.empty
        |> LoggerConfig.withMinLevel Zlog.Info
        |> LoggerConfig.withSink (Sinks.Console.create Format.text)
        |> LoggerConfig.withSink file
        |> LoggerConfig.runWith work
        |> Task.run
