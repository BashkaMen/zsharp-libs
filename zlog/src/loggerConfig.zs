open Zlog

type private Message = Write of Entry | Flush | Stop

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
