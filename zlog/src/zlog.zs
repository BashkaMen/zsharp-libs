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

type private Message = Write of Entry | Flush | Stop

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

let private flushAll (sinks: list<Sink>) = List.iter (fun sink -> sink.flush ()) sinks

let rec private pump (inbox: Mailbox<Message>) (sinks: list<Sink>) : Task<unit> = task {
    let! ready = Alt.sync (Alt.choose [
        Alt.map Some (Mailbox.take inbox)
        Alt.map (fun () -> None) (Alt.always ())
    ])
    let! message =
        match ready with
        | Some message -> Task.ret message
        | None ->
            flushAll sinks
            Alt.sync (Mailbox.take inbox)
    match message with
    | Write entry ->
        List.iter (fun sink -> sink.write entry) sinks
        return! pump inbox sinks
    | Flush ->
        flushAll sinks
        return! pump inbox sinks
    | Stop -> flushAll sinks
}

type LoggerConfig = { minLevel: Level; sinks: list<Sink> }

let empty : LoggerConfig = { minLevel = Info; sinks = [] }

let withMinLevel (minLevel: Level) (config: LoggerConfig) : LoggerConfig = { config with minLevel = minLevel }

let withSink (sink: Sink) (config: LoggerConfig) : LoggerConfig = { config with sinks = List.append config.sinks [ sink ] }

let runWith (body: Logger -> Task<'a>) (config: LoggerConfig) : Task<'a> =
    Task.scope (fun scope -> task {
        let inbox = Mailbox.make ()
        let pumped = Task.start scope (pump inbox config.sinks)
        let logger : Logger =
            { minLevel = config.minLevel
              context = []
              write = fun e -> Mailbox.post inbox (Write e)
              flush = fun () -> Mailbox.post inbox Flush }
        let! result = body logger
        Mailbox.post inbox Stop
        do! Task.wait pumped
        return result
    })
