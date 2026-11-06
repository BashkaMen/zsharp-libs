type Level = Debug | Info | Warn | Fail

type Entry = {
    time: DateTime
    level: Level
    message: string
    fields: list<string * string>
}

type Sink = {
    write: Entry -> unit
    flush: unit -> unit
}
