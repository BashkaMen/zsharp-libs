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

type private Message = Write of Entry | Stop

let private inbox : Channel<Message> = Channel.make 4096

let private off = 4

let private rank (level: Level) : int =
    match level with
    | Debug -> 0
    | Info -> 1
    | Warn -> 2
    | Fail -> 3

let private minRank : Atomic<int> = Atomic.make off

let private write (context: list<string * string>) (level: Level) (x: 'a) =
    if rank level >= Atomic.get minRank then
        Channel.send inbox (Write (entry context level x))

type Logger =
    { context: list<string * string> }
    member l.scope(fields: list<string * string>) : Logger = { context = List.append l.context fields }
    member l.debug(x: 'a) = write l.context Debug x
    member l.info(x: 'a) = write l.context Info x
    member l.warn(x: 'a) = write l.context Warn x
    member l.error(x: 'a) = write l.context Fail x

module Logger =
    let scope (fields: list<string * string>) : Logger = { context = fields }
    let debug (x: 'a) = write [] Debug x
    let info (x: 'a) = write [] Info x
    let warn (x: 'a) = write [] Warn x
    let error (x: 'a) = write [] Fail x

    let rec private forward (sinks: list<Sink>) : Task<unit> = task {
        let! next = Task.blocking (fun () -> Channel.receive inbox)
        match next with
        | Some (Write entry) ->
            List.iter (fun sink -> sink.write entry) sinks
            List.iter (fun sink -> sink.flush ()) sinks
            return! forward sinks
        | Some Stop | None -> ()
    }

    let run (minLevel: Level) (sinks: list<Sink>) (body: unit -> Task<'a>) : Task<'a> =
        Task.scope (fun scope -> task {
            let forwarded = Task.start scope (forward sinks)
            Atomic.set minRank (rank minLevel)
            let! result = body ()
            Atomic.set minRank off
            Channel.send inbox Stop
            do! Task.wait forwarded
            return result
        })

type LoggerConfig = { minLevel: Level; sinks: list<Sink> }
