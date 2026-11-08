trait FromConfig<'a> =
    abstract fromConfig : Map<string, string> -> string -> result<'a, string>

let private value (config: Map<string, string>) (path: string) : result<string, string> =
    match Map.tryFind path config with
    | Some v -> Ok v
    | None -> Error $"missing key {path}"

let private parsed (config: Map<string, string>) (path: string) (parse: string -> option<'a>) (expected: string) : result<'a, string> =
    let v = (value config path)?
    match parse v with
    | Some x -> Ok x
    | None -> Error $"{path}: expected {expected}, got {v}"

let containsPath (config: Map<string, string>) (path: string) =
    Map.toList config |> List.exists (fun (k, _) -> k = path || String.startsWith (path + ":") k)

impl FromConfig<string> =
    let fromConfig (config: Map<string, string>) (path: string) = value config path

impl FromConfig<int> =
    let fromConfig (config: Map<string, string>) (path: string) = parsed config path String.parseInt "an integer"

impl FromConfig<f64> =
    let fromConfig (config: Map<string, string>) (path: string) = parsed config path String.parseFloat "a number"

impl FromConfig<bool> =
    let fromConfig (config: Map<string, string>) (path: string) =
        parsed config path (fun v -> match v with | "true" -> Some true | "false" -> Some false | _ -> None) "true or false"

impl FromConfig<option<'a>> =
    let fromConfig (config: Map<string, string>) (path: string) : result<option<'a>, string> =
        if containsPath config path then Result.map Some (fromConfig config path) else Ok None

derive FromConfig =
    let fromConfig config path : result<'self, string> =
        match Shape.kind with
        | RecordKind ->
            Shape.build (fun f ->
                let at = if path = "" then f.name else $"{path}:{f.name}"
                match f.default with
                | Some d when not (containsPath config at) -> Ok d
                | _ -> fromConfig config at)
        | _ -> Error $"{path}: only records derive FromConfig"

type ConfigReader = { sources: list<unit -> result<list<string * string>, string>> }
