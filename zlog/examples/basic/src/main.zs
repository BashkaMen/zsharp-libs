open Zlog

type SlowRequest = { path: string; ms: int }

impl Loggable<SlowRequest> =
    let toEvent (e: SlowRequest) = ("slow request", [ ("path", e.path); ("ms", string e.ms) ])

let handle (log: Logger) (path: string) : Task<unit> = task {
    let log = log.scope [ ("request_id", "1") ]
    log.info "request started"
    do! Task.sleep 10
    log.warn { path = path; ms = 120 }
}

let work (log: Logger) : Task<int> = task {
    log.debug "hidden below the minimum level"
    log.info (event "started" [ ("pid", "42") ])
    do! handle log "/echo"
    log.error (event "connection failed" [ ("error", "reset by peer") ])
    return 0
}

let main () =
    match Sinks.File.create Format.text "basic.log" with
    | Error e ->
        eprintln $"basic: {e}"
        1
    | Ok file -> Task.run (run Types.Info [ Sinks.Console.create Format.text; file ] work)
