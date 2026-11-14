open Zconfig

[<Link "env.c">]
import c "env.h"

extern strlen : ptr<u8> -> usize

let rec private flatten (path: string) (json: Json) : list<string * string> =
    let at key = if path = "" then key else $"{path}:{key}"
    match json with
    | JObject members -> members |> List.collect (fun (k, v) -> flatten (at k) v)
    | JArray items -> items |> List.mapi (fun i x -> flatten (at (string i)) x) |> List.concat
    | JString s -> [ (path, s) ]
    | JInt n -> [ (path, string n) ]
    | JUInt n -> [ (path, string n) ]
    | JNumber x -> [ (path, string x) ]
    | JBool b -> [ (path, if b then "true" else "false") ]
    | JNull -> []

let private environment () : list<string * string> =
    List.init (zconfig_variable_count ()) (fun i ->
        let p: ptr<u8> = unsafe (Ptr.ofCPtr (zconfig_variable i))
        String.ofBytes (unsafe (Span.ofPtr p (int (strlen p)))))
    |> List.choose (fun line ->
        String.indexOf "=" line
        |> Option.map (fun i ->
            (String.join ":" (String.split "__" (String.sub line 0 i)), String.sub line (i + 1) (String.length line - i - 1))))

let empty : ConfigReader = { sources = [] }

let withSource (source: unit -> result<list<string * string>, string>) (reader: ConfigReader) : ConfigReader =
    { reader with sources = List.append reader.sources [ source ] }

let withValues (values: list<string * string>) (reader: ConfigReader) : ConfigReader =
    withSource (fun () -> Ok values) reader

let withEnvVariables (reader: ConfigReader) : ConfigReader =
    withSource (fun () -> Ok (environment ())) reader

let withJsonFile (path: string) (reader: ConfigReader) : ConfigReader =
    withSource (fun () ->
        File.readAllText path
        |> Result.bind Zjson.fromString
        |> Result.map (flatten "")
        |> Result.mapError (fun e -> $"{path}: {e}")) reader

let read (reader: ConfigReader) : result<'a, string> =
    let values =
        (reader.sources
         |> List.fold (fun acc source -> Result.bind (fun xs -> Result.map (List.append xs) (source ())) acc) (Ok []))?
    fromConfig (Map.ofList values) ""
