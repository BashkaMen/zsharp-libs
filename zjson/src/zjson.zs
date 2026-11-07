open Text
open Types

trait Serialize<'a> =
    abstract serialize : JsonWriter -> 'a -> unit

let private writeItems (w: JsonWriter) (xs: 'c) =
    JsonWriter.beginArray w
    for x in xs do
        serialize w x
    JsonWriter.endArray w

let private entries (w: JsonWriter) (kvs: 'c) =
    JsonWriter.beginObject w
    for (k, v) in kvs do
        let scratch = JsonWriter.create 64
        serialize scratch k
        let text = JsonWriter.written scratch
        if Span.length text > 0 && text.[0] = u8 34 then // "
            JsonWriter.quotedKey w text
        else
            JsonWriter.key w (String.ofBytes text)
        serialize w v
    JsonWriter.endObject w

impl Serialize<string> =
    let serialize (w: JsonWriter) (s: string) = JsonWriter.str w s

impl Serialize<char> =
    let serialize (w: JsonWriter) (c: char) = JsonWriter.str w (string c)

impl Serialize<bool> =
    let serialize (w: JsonWriter) (b: bool) = JsonWriter.boolean w b

impl Serialize<int> =
    let serialize (w: JsonWriter) (n: int) = JsonWriter.integer w (i64 n)

impl Serialize<i64> =
    let serialize (w: JsonWriter) (n: i64) = JsonWriter.integer w n

impl Serialize<i32> =
    let serialize (w: JsonWriter) (n: i32) = JsonWriter.integer w (i64 n)

impl Serialize<i16> =
    let serialize (w: JsonWriter) (n: i16) = JsonWriter.integer w (i64 n)

impl Serialize<i8> =
    let serialize (w: JsonWriter) (n: i8) = JsonWriter.integer w (i64 n)

impl Serialize<u64> =
    let serialize (w: JsonWriter) (n: u64) =
        if n > u64 9223372036854775807 then JsonWriter.float w (f64 n) else JsonWriter.integer w (i64 n)

impl Serialize<u32> =
    let serialize (w: JsonWriter) (n: u32) = JsonWriter.integer w (i64 n)

impl Serialize<u16> =
    let serialize (w: JsonWriter) (n: u16) = JsonWriter.integer w (i64 n)

impl Serialize<u8> =
    let serialize (w: JsonWriter) (n: u8) = JsonWriter.integer w (i64 n)

impl Serialize<f64> =
    let serialize (w: JsonWriter) (x: f64) = JsonWriter.float w x

impl Serialize<f32> =
    let serialize (w: JsonWriter) (x: f32) = JsonWriter.float w (f64 x)

impl Serialize<option<'a>> =
    let serialize (w: JsonWriter) (o: option<'a>) =
        match o with
        | Some x -> serialize w x
        | None -> JsonWriter.nul w

impl Serialize<list<'a>> =
    let serialize (w: JsonWriter) (xs: list<'a>) = writeItems w xs

impl Serialize<array<'a>> =
    let serialize (w: JsonWriter) (xs: array<'a>) = writeItems w xs

impl Serialize<Map<'k, 'v>> =
    let serialize (w: JsonWriter) (m: Map<'k, 'v>) = entries w m

impl Serialize<HashMap<'k, 'v>> =
    let serialize (w: JsonWriter) (m: HashMap<'k, 'v>) = entries w m

impl Serialize<Json> =
    let serialize (w: JsonWriter) (j: Json) =
        match j with
        | JNull -> JsonWriter.nul w
        | JBool b -> JsonWriter.boolean w b
        | JInt n -> JsonWriter.integer w n
        | JNumber x -> JsonWriter.float w x
        | JString s -> JsonWriter.str w s
        | JArray xs -> writeItems w xs
        | JObject kvs -> entries w kvs

derive Serialize =
    let serialize w (x: 'self) =
        match Shape.of x with
        | Record r ->
            JsonWriter.beginObject w
            for f in r.fields do
                JsonWriter.key w f.name
                serialize w f.value
            JsonWriter.endObject w
        | Union c ->
            if c.count = 0 then
                JsonWriter.str w c.name
            else
                JsonWriter.beginObject w
                JsonWriter.key w "case"
                JsonWriter.str w c.name
                JsonWriter.key w "fields"
                JsonWriter.beginArray w
                for f in c.fields do
                    serialize w f.value
                JsonWriter.endArray w
                JsonWriter.endObject w

let toBytes (w: JsonWriter) (x: 'a) : span<u8> =
    JsonWriter.reset w
    serialize w x
    JsonWriter.written w

let toString (w: JsonWriter) (x: 'a) : string = String.ofBytes (toBytes w x)

trait Deserialize<'a> =
    abstract deserialize : JsonReader -> result<'a, string>

let private item (r: JsonReader) (index: int) : result<'a, string> =
    match deserialize r with
    | Ok x -> Ok x
    | Error e -> Json.within $"[{index}]" (Error e)

let private items (r: JsonReader) : result<list<'a>, string> =
    (JsonReader.beginArray r)?
    let rec go (i: int) (acc: list<'a>) =
        let more = (JsonReader.element r i)?
        if not more then
            Ok (List.rev acc)
        else
            let x = (item r i)?
            go (i + 1) (x :: acc)
    go 0 []

let private readKey (raw: span<u8>) : result<'k, string> =
    let quoted = JsonReader.create raw
    match deserialize quoted with
    | Ok k when JsonReader.atEnd quoted -> Ok k
    | _ ->
        let bare = JsonReader.create raw.[1 .. Span.length raw - 2]
        match deserialize bare with
        | Ok k when JsonReader.atEnd bare -> Ok k
        | _ -> Error $"a key that doesn't fit the map's key type: {String.ofBytes raw}"

let private readMembers (r: JsonReader) (add: 'k -> 'v -> unit) : result<unit, string> =
    let obj = (JsonReader.beginObject r)?
    let rec go () =
        let more = (JsonReader.hasMember r obj)?
        if not more then
            JsonReader.endObject r obj
        else
            let raw = (JsonReader.rawKey r)?
            let k = (readKey raw)?
            match deserialize r with
            | Ok v ->
                add k v
                go ()
            | Error e -> Json.within (String.ofBytes raw.[1 .. Span.length raw - 2]) (Error e)
    go ()

let private narrow (r: JsonReader) (fits: i64 -> bool) (convert: i64 -> 'n) (name: string) : result<'n, string> =
    let start = r.pos.Value
    match JsonReader.integer r with
    | Ok n when fits n -> Ok (convert n)
    | Ok _ ->
        r.pos.Value <- start
        JsonReader.fail r $"expected an integer that fits {name}"
    | Error e -> Error e

impl Deserialize<string> =
    let deserialize (r: JsonReader) = JsonReader.str r

impl Deserialize<char> =
    let deserialize (r: JsonReader) : result<char, string> =
        let s = (JsonReader.str r)?
        if String.length s > 0 && snd (String.decodeAt s 0) = String.length s then
            Ok (fst (String.decodeAt s 0))
        else
            JsonReader.fail r "expected a string of one character"

impl Deserialize<bool> =
    let deserialize (r: JsonReader) = JsonReader.boolean r

impl Deserialize<i64> =
    let deserialize (r: JsonReader) = JsonReader.integer r

impl Deserialize<int> =
    let deserialize (r: JsonReader) = narrow r (fun n -> i64 (int n) = n) int "int"

impl Deserialize<i32> =
    let deserialize (r: JsonReader) = narrow r (fun n -> i64 (i32 n) = n) i32 "i32"

impl Deserialize<i16> =
    let deserialize (r: JsonReader) = narrow r (fun n -> i64 (i16 n) = n) i16 "i16"

impl Deserialize<i8> =
    let deserialize (r: JsonReader) = narrow r (fun n -> i64 (i8 n) = n) i8 "i8"

impl Deserialize<u32> =
    let deserialize (r: JsonReader) = narrow r (fun n -> i64 (u32 n) = n) u32 "u32"

impl Deserialize<u16> =
    let deserialize (r: JsonReader) = narrow r (fun n -> i64 (u16 n) = n) u16 "u16"

impl Deserialize<u8> =
    let deserialize (r: JsonReader) = narrow r (fun n -> i64 (u8 n) = n) u8 "u8"

impl Deserialize<u64> =
    let deserialize (r: JsonReader) : result<u64, string> =
        let start = r.pos.Value
        match JsonReader.integer r with
        | Ok n when n >= i64 0 -> Ok (u64 n)
        | _ ->
            r.pos.Value <- start
            match JsonReader.float r with
            | Ok x when x >= 0.0 && x < 1.8446744073709552e19 && f64 (u64 x) = x -> Ok (u64 x)
            | _ ->
                r.pos.Value <- start
                JsonReader.fail r "expected an integer that fits u64"

impl Deserialize<f64> =
    let deserialize (r: JsonReader) = JsonReader.float r

impl Deserialize<f32> =
    let deserialize (r: JsonReader) = JsonReader.float r |> Result.map f32

impl Deserialize<option<'a>> =
    let deserialize (r: JsonReader) : result<option<'a>, string> =
        if JsonReader.skipNull r then Ok None else Result.map Some (deserialize r)

impl Deserialize<list<'a>> =
    let deserialize (r: JsonReader) : result<list<'a>, string> = items r

impl Deserialize<array<'a>> =
    let deserialize (r: JsonReader) : result<array<'a>, string> = Result.map Array.ofList (items r)

impl Deserialize<Map<'k, 'v>> =
    let deserialize (r: JsonReader) : result<Map<'k, 'v>, string> =
        let m = ref Map.empty
        (readMembers r (fun k v -> m.Value <- Map.add k v m.Value))?
        Ok m.Value

impl Deserialize<HashMap<'k, 'v>> =
    let deserialize (r: JsonReader) : result<HashMap<'k, 'v>, string> =
        let m = ref HashMap.empty
        (readMembers r (fun k v -> m.Value <- HashMap.add k v m.Value))?
        Ok m.Value

impl Deserialize<Json> =
    let deserialize (r: JsonReader) : result<Json, string> =
        match JsonReader.peek r with
        | 123 -> // {
            let kvs = ref []
            (readMembers r (fun (k: string) (v: Json) -> kvs.Value <- (k, v) :: kvs.Value))?
            Ok (JObject (List.rev kvs.Value))
        | 91 -> Result.map JArray (items r) // [
        | 34 -> Result.map JString (JsonReader.str r) // "
        | 116 | 102 -> Result.map JBool (JsonReader.boolean r) // t f
        | 110 when JsonReader.skipNull r -> Ok JNull // n
        | _ ->
            let start = r.pos.Value
            match JsonReader.integer r with
            | Ok n when not (JsonReader.wasFloat r start) -> Ok (JInt n)
            | _ ->
                r.pos.Value <- start
                Result.map JNumber (JsonReader.float r)

derive Deserialize =
    let deserialize r : result<'self, string> =
        match Shape.kind with
        | RecordKind ->
            let obj = (JsonReader.beginObject r)?
            let value =
                (Shape.build (fun f ->
                    match JsonReader.seekKey r obj f.name with
                    | Ok true -> deserialize r |> Json.within f.name
                    | Ok false ->
                        match f.default with
                        | Some d -> Ok d
                        | None ->
                            deserialize (JsonReader.create b"null")
                            |> Result.mapError (fun _ -> $"missing key {f.name}")
                    | Error e -> Error e))?
            (JsonReader.endObject r obj)?
            Ok value
        | UnionKind ->
            if JsonReader.peek r = 34 then // "
                let name = (JsonReader.str r)?
                match Shape.buildCase name (fun f -> Error $"case {name} needs field {f.index}") with
                | Some built -> built
                | None -> Error $"unknown case {name}"
            else
                let obj = (JsonReader.beginObject r)?
                let hasCase = (JsonReader.seekKey r obj "case")?
                let named = if hasCase then JsonReader.str r |> Json.within "case" else Error "missing key case"
                let name = named?
                let hasFields = (JsonReader.seekKey r obj "fields")?
                let opened = if hasFields then JsonReader.beginArray r |> Json.within "fields" else Ok ()
                opened?
                let consumed = ref 0
                let built =
                    Shape.buildCase name (fun f ->
                        let more = if hasFields then JsonReader.element r f.index else Ok false
                        match more with
                        | Ok true ->
                            consumed.Value <- f.index + 1
                            match deserialize r with
                            | Ok x -> Ok x
                            | Error e -> Json.within $"fields[{f.index}]" (Error e)
                        | Ok false -> Error $"at fields: case {name} needs more fields"
                        | Error e -> Error e)
                match built with
                | None -> Error $"unknown case {name}"
                | Some (Error e) -> Error e
                | Some (Ok value) ->
                    let closed = if hasFields then JsonReader.endArray r consumed.Value else Ok ()
                    closed?
                    (JsonReader.endObject r obj)?
                    Ok value

let fromBytes (bytes: span<u8>) : result<'a, string> =
    let r = JsonReader.create bytes
    let value = (deserialize r)?
    if JsonReader.atEnd r then Ok value else JsonReader.fail r "expected the end after the document"

let fromString (text: string) : result<'a, string> = fromBytes (String.bytes text)
