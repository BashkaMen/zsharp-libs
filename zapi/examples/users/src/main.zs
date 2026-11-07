open Zerver
open App
open Zlog

let main () =
    let endpoints = [
        Endpoints.GetUser.endpoint
        Endpoints.CreateUser.endpoint
        Endpoints.Manifest.endpoint
    ]
    let app =
        LoggerConfig.empty
        |> LoggerConfig.withMinLevel Debug
        |> LoggerConfig.withSink (Sinks.Console.create Format.text)
        |> LoggerConfig.runWith (fun () -> Zapi.serve Zerver.defaultConfig { db = { url = "postgres://local" } } endpoints)
    match Task.run app with
    | Error e ->
        eprintln $"users: {e}"
        1
    | Ok () -> 0
