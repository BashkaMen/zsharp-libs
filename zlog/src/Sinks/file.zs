open Types

let private report (written: result<unit, string>) : unit =
    match written with
    | Ok () -> ()
    | Error e -> eprintln $"logger: {e}"

let create (format: Entry -> string) (path: string) : result<Sink, string> =
    match File.openAppend path with
    | Error e -> Error e
    | Ok writer ->
        let write entry = report (Writer.writeText writer (format entry + "\n"))
        let flush () = report (Writer.flush writer)
        let sink : Sink = { write = write; flush = flush }
        Ok sink
