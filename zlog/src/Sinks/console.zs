open Zlog

let create (format: Entry -> string) : Sink =
    let write (entry: Entry) =
        if entry.level >= Warn then
            eprintln (format entry)
        else
            println (format entry)
    { write = write; flush = fun () -> () }
