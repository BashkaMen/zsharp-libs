open Testing
open Text
open Zjson

[<Derive(Json, Serialize)>]
type User = { id: int; name: string; email: string; active: bool; tags: list<string>; score: f64; nick: option<string> }

[<Derive(Json, Serialize)>]
type Shape =
    | Dot
    | Circle of f64
    | Rect of int * int

[<Derive(Json, Serialize)>]
type Nested = { owner: User; shapes: array<Shape>; byName: Map<string, int>; byId: Map<int, string> }

let private sameIn w (x: 'a) =
    let expected = Json.print (toJson x)
    let actual = String.ofBytes (toBytes w x)
    if actual <> expected then
        panic $"differ:\nexpected {expected}\nactual   {actual}"

let private same (x: 'a) = sameIn (JsonWriter.create 64) x

let private user : User =
    { id = 7; name = "user7"; email = "u7@ex.com"; active = true; tags = [ "a"; "b" ]; score = 2.5; nick = None }

let private tricky : User =
    { id = -9223372036854775807 - 1
      name = "q\"b\\s/\n\r\t\u{8}\u{c}\u{1}\u{1f}"
      email = "юникод ✓ 😀"
      active = false
      tags = [ ""; "x" ]
      score = 0.0 / 0.0
      nick = Some "" }

[<Tests>]
let tests =
    testList "write" [
        test "matches Json.print for a record" (fun () -> same user)
        test "matches Json.print for escapes, extremes and NaN" (fun () -> same tricky)
        test "matches Json.print for empty collections" (fun () ->
            same { user with tags = [] }
            same ([||] : array<int>))
        test "matches Json.print for union cases" (fun () -> same [ Dot; Circle 1.5; Rect (2, 3) ])
        test "matches Json.print for nested records and maps" (fun () ->
            same { owner = user; shapes = [| Dot; Rect (1, -1) |]; byName = Map.ofList [ ("a", 1); ("b\"", 2) ]; byId = Map.ofList [ (1, "x"); (-2, "y") ] })
        test "matches Json.print for numeric widths" (fun () ->
            same [ i8 -128; i8 127 ]
            same [ u8 0; u8 255 ]
            same [ i16 -32768 ]
            same [ u32 4294967295 ]
            same [ u64 0; u64 9223372036854775807; u64 9223372036854775807 * u64 2 + u64 1 ]
            same [ f32 0.5; f32 -1.0 ])
        test "matches Json.print for a Json tree" (fun () -> same (toJson [ user; tricky ]))
        test "a reused writer starts each document from scratch" (fun () ->
            let w = JsonWriter.create 16
            toBytes w tricky |> ignore
            sameIn w user)
    ]
