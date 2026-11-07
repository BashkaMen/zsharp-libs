open Testing
open Text
open Zjson

[<Derive(Json, Serialize)>]
type Address = { city: string; zip: string }

[<Derive(Json, Serialize)>]
type User =
    { id: int
      name: string
      email: string
      age: int
      score: f64
      active: bool
      tags: list<string>
      address: Address
      nick: option<string>
      bio: string }

let private users (n: int) : array<User> =
    Array.init n (fun i ->
        { id = i
          name = $"user{i}"
          email = $"user{i}@example.com"
          age = 18 + i % 60
          score = float (i % 4000) + 0.25
          active = i % 3 <> 0
          tags = [ "admin"; "beta" ]
          address = { city = $"city{i % 100}"; zip = $"{10000 + i}" }
          nick = if i % 2 = 0 then Some $"nick{i}" else None
          bio = "Backend engineer who enjoys distributed systems, coffee and \"long\" walks in the mountains." })

let private small = users 10
let private large = users 1000

let private w = JsonWriter.create 65536

[<Tests>]
let tests =
    testList "bench" [
        bench "10 users: Json.print" (fun () -> Json.print (toJson small))
        bench "10 users: zjson toString" (fun () -> toString w small)
        bench "10 users: zjson toBytes" (fun () -> Span.length (toBytes w small))
        bench "1000 users: Json.print" (fun () -> Json.print (toJson large))
        bench "1000 users: zjson toString" (fun () -> toString w large)
        bench "1000 users: zjson toBytes" (fun () -> Span.length (toBytes w large))
        testFaster "zjson toString is faster than Json.print" (fun () -> toString w large) (fun () -> Json.print (toJson large))
    ]
