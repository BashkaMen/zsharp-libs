open Text
open Types

trait Serialize<'a> =
    abstract serialize : JsonWriter -> 'a -> unit

let private elements (w: JsonWriter) (xs: 'c) =
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
    let serialize (w: JsonWriter) (xs: list<'a>) = elements w xs

impl Serialize<array<'a>> =
    let serialize (w: JsonWriter) (xs: array<'a>) = elements w xs

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
        | JArray xs -> elements w xs
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
