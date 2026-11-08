open Zconfig

[<Derive(FromConfig)>]
type LoggerConfig = { MinimumLevel: string }

[<Derive(FromConfig)>]
type AppConfig = { HostName: string; Port: int; Timeout: option<int>; Logger: LoggerConfig }

let main () =
    let config : result<AppConfig, string> =
        ConfigReader.empty
        |> ConfigReader.withJsonFile "appsettings.json"
        |> ConfigReader.withValues [ ("Port", "9090") ]
        |> ConfigReader.withEnvVariables
        |> ConfigReader.read
    match config with
    | Ok c ->
        println $"{c}"
        0
    | Error e ->
        eprintln $"basic: {e}"
        1
