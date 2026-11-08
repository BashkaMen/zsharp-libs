open Testing
open Zconfig

[<Derive(FromConfig)>]
type LoggerConfig = { MinimumLevel: string }

[<Derive(FromConfig)>]
type EnvConfig = { HostName: string; Port: int; Logger: LoggerConfig }

[<Derive(FromConfig)>]
type Optional = { Name: string; Timeout: option<int>; Logger: option<LoggerConfig> }

[<Derive(FromConfig)>]
type Home = { HOME: string }

let values = [ ("HostName", "local"); ("Port", "8080"); ("Logger:MinimumLevel", "Info") ]

let read (values: list<string * string>) : result<'a, string> =
    ConfigReader.empty |> ConfigReader.withValues values |> ConfigReader.read

[<Tests>]
let tests = testList "fromConfig" [
    test "reads a nested record" (fun () ->
        assert (read values = Ok { HostName = "local"; Port = 8080; Logger = { MinimumLevel = "Info" } }))
    test "a later value wins" (fun () ->
        assert (Result.map (fun c -> c.Port) (read (List.append values [ ("Port", "9090") ]) : result<EnvConfig, string>) = Ok 9090))
    test "names a missing key" (fun () ->
        assert ((read [ ("HostName", "local"); ("Port", "1") ] : result<EnvConfig, string>) = Error "missing key Logger:MinimumLevel"))
    test "names a value that does not parse" (fun () ->
        assert ((read [ ("HostName", "h"); ("Port", "x") ] : result<EnvConfig, string>) = Error "Port: expected an integer, got x"))
    test "leaves absent options empty" (fun () ->
        assert (read [ ("Name", "n") ] = Ok { Name = "n"; Timeout = None; Logger = None }))
    test "reads env variables" (fun () ->
        let home = ConfigReader.empty |> ConfigReader.withEnvVariables |> ConfigReader.read
        assert (Result.map (fun h -> h.HOME) home = Ok (Option.defaultValue "" (Environment.getVariable "HOME"))))
    test "reads a json file" (fun () ->
        let config = ConfigReader.empty |> ConfigReader.withJsonFile "tests/appsettings.json" |> ConfigReader.read
        assert (config = Ok { HostName = "from-json"; Port = 8080; Logger = { MinimumLevel = "Debug" } }))
    test "a value overrides the json file" (fun () ->
        let config = ConfigReader.empty |> ConfigReader.withJsonFile "tests/appsettings.json" |> ConfigReader.withValues [ ("Logger:MinimumLevel", "Warn") ] |> ConfigReader.read
        assert (Result.map (fun c -> c.Logger.MinimumLevel) (config : result<EnvConfig, string>) = Ok "Warn"))
    test "names a missing file" (fun () ->
        let config : result<EnvConfig, string> = ConfigReader.empty |> ConfigReader.withJsonFile "missing.json" |> ConfigReader.read
        assert (Result.isError config))
]
