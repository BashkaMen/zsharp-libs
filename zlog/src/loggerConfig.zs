open Zlog

let empty : LoggerConfig = { minLevel = Info; sinks = [] }

let withMinLevel (minLevel: Level) (config: LoggerConfig) : LoggerConfig = { config with minLevel = minLevel }

let withSink (sink: Sink) (config: LoggerConfig) : LoggerConfig = { config with sinks = List.append config.sinks [ sink ] }

let runWith (body: unit -> Task<'a>) (config: LoggerConfig) : Task<'a> = Logger.run config.minLevel config.sinks body
