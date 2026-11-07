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
        test "matches Json.print for floats" (fun () ->
            same [ 0.0; -0.0; 0.1; 0.2 + 0.1; 1.0 / 3.0; 75000.5; -52.2297; 1.0e15; 1.0e-5; 123456789012345.6; 5.0e-324; 1.7976931348623157e308 ]
            same (List.init 20000 (fun i -> float (i - 10000) / 7.0))
            same (List.init 20000 (fun i -> float i * 0.01))
            same (List.init 20000 (fun i -> 1.0 / float (i + 1)))
            same (List.init 20000 (fun i -> float i * 1234.5678e-9)))
        test "matches Json.print for a Json tree" (fun () -> same (toJson [ user; tricky ]))
        test "a reused writer starts each document from scratch" (fun () ->
            let w = JsonWriter.create 16
            toBytes w tricky |> ignore
            sameIn w user)
    ]
