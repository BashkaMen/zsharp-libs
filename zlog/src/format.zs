open Types

let private levelName (level: Level) : string =
    match level with
    | Debug -> "DEBUG"
    | Info -> "INFO"
    | Warn -> "WARN"
    | Fail -> "ERROR"

let text (entry: Entry) : string =
    let fields = entry.fields |> List.map (fun (key, value) -> $" {key}={value}") |> String.concat
    $"{entry.time} {levelName entry.level} {entry.message}{fields}"
