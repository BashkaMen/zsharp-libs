open Types

type private Message = Write of Entry | Stop

trait Loggable<'a> =
    abstract toEvent : 'a -> string * list<string * string>

type Event = { message: string; fields: list<string * string> }

let event (message: string) (fields: list<string * string>) : Event = { message = message; fields = fields }

impl Loggable<string> =
    let toEvent (message: string) = (message, [])

impl Loggable<Event> =
    let toEvent (event: Event) = (event.message, event.fields)

type Logger =
    { log: Level -> string -> list<string * string> -> unit
      scope: list<string * string> -> Logger }
    member l.debug(x: 'a) = let (message, fields) = toEvent x in l.log Debug message fields
    member l.info(x: 'a) = let (message, fields) = toEvent x in l.log Info message fields
    member l.warn(x: 'a) = let (message, fields) = toEvent x in l.log Warn message fields
    member l.error(x: 'a) = let (message, fields) = toEvent x in l.log Fail message fields

let rec private make (minLevel: Level) (inbox: Mailbox<Message>) (context: list<string * string>) : Logger =
    let log level message fields =
        if level >= minLevel then
            let entry : Entry = { time = DateTime.utcNow (); level = level; message = message; fields = List.append context fields }
            Mailbox.post inbox (Write entry)
    let scope fields = make minLevel inbox (List.append context fields)
    { log = log; scope = scope }

let rec private pump (inbox: Mailbox<Message>) (sinks: list<Sink>) : Task<unit> = task {
    let! ready = Alt.sync (Alt.choose [
        Alt.map Some (Mailbox.take inbox)
        Alt.map (fun () -> None) (Alt.always ())
    ])
    let! message =
        match ready with
        | Some message -> Task.ret message
        | None ->
            List.iter (fun sink -> sink.flush ()) sinks
            Alt.sync (Mailbox.take inbox)
    match message with
    | Write entry ->
        List.iter (fun sink -> sink.write entry) sinks
        return! pump inbox sinks
    | Stop -> List.iter (fun sink -> sink.flush ()) sinks
}

let run (minLevel: Level) (sinks: list<Sink>) (body: Logger -> Task<'a>) : Task<'a> =
    Task.scope (fun scope -> task {
        let inbox = Mailbox.make ()
        let pumped = Task.start scope (pump inbox sinks)
        let! result = body (make minLevel inbox [])
        Mailbox.post inbox Stop
        do! Task.wait pumped
        return result
    })
