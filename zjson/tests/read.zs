open Testing
open Text
open Zjson

[<Derive(Json, Serialize, Deserialize)>]
type User = { id: int; name: string; email: string; active: bool; tags: list<string>; score: f64; nick: option<string> }

[<Derive(Json, Serialize, Deserialize)>]
type Shape =
    | Dot
    | Circle of f64
    | Rect of int * int

[<Derive(Json, Serialize, Deserialize)>]
type Nested = { owner: User; shapes: list<Shape>; byName: Map<string, int>; byId: Map<int, string> }

[<Derive(Json, Serialize, Deserialize)>]
type WithDefault = { id: int; role: string = "guest" }

let private user : User =
    { id = 7; name = "user7"; email = "u7@ex.com"; active = true; tags = [ "a"; "b" ]; score = 2.5; nick = None }

let private tricky : User =
    { id = -9223372036854775807 - 1
      name = "q\"b\\s/\n\r\t\u{8}\u{c}\u{1}\u{1f}"
      email = "юникод ✓ 😀"
      active = false
      tags = [ ""; "x" ]
      score = -1.5e-7
      nick = Some "" }

let private roundTrip (x: 'a) =
    let text = toString (JsonWriter.create 64) x
    match fromString text with
    | Ok y when y = x -> ()
    | Ok y -> panic $"read back {y}\nfrom {text}"
    | Error e -> panic $"{e}\nfrom {text}"

let private sameAsStd (text: string) (read: string -> result<'a, string>) =
    let std = Json.parse text |> Result.bind fromJson
    match (read text, std) with
    | (Ok a, Ok b) when a = b -> ()
    | (Error _, Error _) -> ()
    | (ours, theirs) -> panic $"differ on {text}\nours {ours}\nstd  {theirs}"

let private fails (text: string) (read: string -> result<'a, string>) (expected: string) =
    match read text with
    | Error e when String.contains expected e -> ()
    | Error e -> panic $"error {e}\nwanted one with {expected}"
    | Ok v -> panic $"read {v} from {text}"

let private asUser (text: string) : result<User, string> = fromString text
let private asUsers (text: string) : result<list<User>, string> = fromString text
let private asShapes (text: string) : result<list<Shape>, string> = fromString text
let private asDefault (text: string) : result<WithDefault, string> = fromString text
let private asJson (text: string) : result<Json, string> = fromString text
let private asI8 (text: string) : result<i8, string> = fromString text
let private asI64 (text: string) : result<i64, string> = fromString text

[<Tests>]
let tests =
    testList "read" [
        test "reads back what Serialize writes" (fun () ->
            roundTrip user
            roundTrip tricky
            roundTrip [ Dot; Circle 1.5; Rect (2, -3) ]
            roundTrip { owner = user; shapes = [ Dot; Rect (1, -1) ]; byName = Map.ofList [ ("a", 1); ("b\"", 2) ]; byId = Map.ofList [ (1, "x"); (-2, "y") ] }
            roundTrip [ u64 0; u64 9223372036854775807 ]
            roundTrip [ i8 -128; i8 127 ])
        test "reads keys in any order and skips unknown ones" (fun () ->
            sameAsStd """{"nick":"n","zzz":[1,{"a":"}]"},null],"tags":[],"score":1,"active":true,"email":"e","name":"n","id":1,"x":{}}""" asUser)
        test "reads a missing option key as None" (fun () ->
            sameAsStd """{"id":1,"name":"n","email":"e","active":false,"tags":["t"],"score":0.5}""" asUser)
        test "takes a field's default for a missing key" (fun () ->
            sameAsStd """{"id":3}""" asDefault
            sameAsStd """{"role":"admin","id":3}""" asDefault)
        test "reads every form of a union case" (fun () ->
            sameAsStd """["Dot",{"case":"Dot"},{"fields":[2.5],"case":"Circle"},{"case":"Rect","fields":[2,3]}]""" asShapes)
        test "decodes escapes and surrogate pairs" (fun () ->
            sameAsStd """{"id":1,"name":"é😀\n\"\\\/\t","email":"","active":true,"tags":[],"score":1e3}""" asUser)
        test "reads any document as a Json tree" (fun () ->
            sameAsStd """ {"a":[1,-2.5,1e400,123456789012345678901,true,null,"s"],"b":{}} """ asJson)
        test "names where a value didn't fit" (fun () ->
            fails """[{"id":1,"name":"n","email":"e","active":true,"tags":[],"score":1},{"id":"x"}]""" asUsers "at [1].id"
            fails """{"id":1}""" asUser "missing key name"
            fails """["Square"]""" asShapes "unknown case Square")
        test "rejects malformed documents" (fun () ->
            fails """[1,]""" asShapes "expected"
            fails """{"id":1} x""" asDefault "expected the end"
            fails """{"id":01}""" asDefault "expected an integer"
            fails """{"id":1,}""" asDefault "expected a key"
            fails "\"\u{1}\"" asJson "control character"
            fails """{"id":1""" asDefault "expected ',' or '}'")
        test "checks integer ranges" (fun () ->
            fails "128" asI8 "fits i8"
            fails "9223372036854775808" asI64 "fits i64"
            sameAsStd "-9223372036854775808" asI64
            sameAsStd "2.0" asI64)
        test "rejects invalid UTF-8" (fun () ->
            match (fromBytes [| u8 34; u8 255; u8 34 |] : result<string, string>) with
            | Error e when String.contains "UTF-8" e -> ()
            | other -> panic $"got {other}")
        test "limits nesting depth" (fun () ->
            let nested (n: int) = String.repeat n "[" + String.repeat n "]"
            sameAsStd (nested 64) asJson
            fails (nested 65) asJson "nested deeper than 64"
            fails (nested 100000) asJson "nested deeper than 64"
            match (fromBytesWith 2 (String.bytes "[[[]]]") : result<Json, string>) with
            | Error e when String.contains "deeper than 2" e -> ()
            | other -> panic $"got {other}")
    ]
