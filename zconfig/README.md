# zconfig

Typed configuration for Z#: key-value pairs from several sources, read into a record.

## Usage

```fsharp
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
```

```sh
Logger__MinimumLevel=Debug Timeout=30 zs run
```

```text
{ HostName = localhost; Port = 9090; Timeout = Some 30; Logger = { MinimumLevel = Debug } }
```

- **Keys:** a path with `:` between sections, `Logger:MinimumLevel`. A record field is read from its parent's path plus the field name.
- **Order:** sources are read in `read`, in the order they were added; a later source wins.
- **Errors:** `read` stops at the first one and names the key:

  ```text
  missing key Logger:MinimumLevel
  Port: expected an integer, got x
  appsettings.json: cannot open appsettings.json: No such file or directory
  ```

## Sources

| Source | Keys |
|---|---|
| `withValues pairs` | as given |
| `withEnvVariables` | every variable; `__` becomes `:`, so `Logger__MinimumLevel` is `Logger:MinimumLevel` |
| `withJsonFile path` | objects flatten to `Logger:MinimumLevel`, arrays to `Hosts:0`; numbers and booleans become text, `null` is left out |
| `withSource f` | whatever `f : unit -> result<list<string * string>, string>` returns |

## Types

`[<Derive(FromConfig)>]` reads a record. Fields can be `string`, `int`, `f64`, `bool` (`true` or
`false`), `option<_>` (`None` when neither the key nor anything under it is set), or another
record with the derive. A field with a default keeps it when its key is missing.

A new type gets an impl of the trait, which takes all values and the path to read:

```fsharp
type Level = Debug | Info

impl FromConfig<Level> =
    let fromConfig (config: Map<string, string>) (path: string) : result<Level, string> =
        match Map.tryFind path config with
        | Some "Debug" -> Ok Debug
        | Some "Info" -> Ok Info
        | Some v -> Error $"{path}: unknown level {v}"
        | None -> Error $"missing key {path}"
```

## Limitations

- Keys are case-sensitive.
- No unions or lists yet: an array in JSON gives `Hosts:0`, `Hosts:1`, but nothing reads them into a `list`.
- Environment variables are listed through `src/env.c`, since std only has `Environment.getVariable`.

## Example

```sh
cd examples/basic
zs run
```
