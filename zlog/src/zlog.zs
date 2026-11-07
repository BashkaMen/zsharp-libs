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

trait Loggable<'a> =
    abstract toEvent : 'a -> string * list<string * string>

type Event = { message: string; fields: list<string * string> }

let event (message: string) (fields: list<string * string>) : Event = { message = message; fields = fields }

impl Loggable<string> =
    let toEvent (message: string) = (message, [])

impl Loggable<Event> =
    let toEvent (event: Event) = (event.message, event.fields)

let private entry (context: list<string * string>) (level: Level) (x: 'a) : Entry =
    let (message, fields) = toEvent x
    { time = DateTime.utcNow (); level = level; message = message; fields = List.append context fields }

type Logger =
    { minLevel: Level
      context: list<string * string>
      write: Entry -> unit
      flush: unit -> unit }
    member l.scope(fields: list<string * string>) : Logger = { l with context = List.append l.context fields }
    member l.debug(x: 'a) = if Debug >= l.minLevel then l.write (entry l.context Debug x)
    member l.info(x: 'a) = if Info >= l.minLevel then l.write (entry l.context Info x)
    member l.warn(x: 'a) = if Warn >= l.minLevel then l.write (entry l.context Warn x)
    member l.error(x: 'a) = if Fail >= l.minLevel then l.write (entry l.context Fail x)

type LoggerConfig = { minLevel: Level; sinks: list<Sink> }
