open Testing
open Text
open Zjson

[<Derive(Json, Serialize, Deserialize)>]
type Address = { city: string; zip: string }

[<Derive(Json, Serialize, Deserialize)>]
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

[<Derive(Json, Serialize, Deserialize)>]
type Post = { id: int; body: string }

let private paragraph = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, \"quoted\" text and a line break\n, then привет мир. "

let private posts : array<Post> =
    Array.init 100 (fun i -> { id = i; body = String.concat [ for _ in 1 .. 2000 / String.length paragraph + 1 -> paragraph ] })

let private small = users 10
let private large = users 1000

let private w = JsonWriter.create 65536

let private smallText = toString w small
let private largeText = toString w large
let private postsText = toString w posts

let private largeBytes = Buffer.ofSpan (String.bytes largeText)

let private stdRead (text: string) : int =
    match (Json.parse text |> Result.bind fromJson : result<array<User>, string>) with
    | Ok users -> Array.length users
    | Error e -> panic e

let private zjsonRead (text: string) : int =
    match (fromString text : result<array<User>, string>) with
    | Ok users -> Array.length users
    | Error e -> panic e

let private stdReadPosts (text: string) : int =
    match (Json.parse text |> Result.bind fromJson : result<array<Post>, string>) with
    | Ok ps -> Array.length ps
    | Error e -> panic e

let private zjsonReadPosts (text: string) : int =
    match (fromString text : result<array<Post>, string>) with
    | Ok ps -> Array.length ps
    | Error e -> panic e

[<Tests>]
let tests =
    testList "bench" [
        bench "100 posts of 2000 chars: Json.print" (fun () -> Json.print (toJson posts))
        bench "100 posts of 2000 chars: zjson toString" (fun () -> toString w posts)
        bench "read 100 posts of 2000 chars: Json.parse + fromJson" (fun () -> stdReadPosts postsText)
        bench "read 100 posts of 2000 chars: zjson fromString" (fun () -> zjsonReadPosts postsText)
        bench "10 users: Json.print" (fun () -> Json.print (toJson small))
        bench "10 users: zjson toString" (fun () -> toString w small)
        bench "10 users: zjson toBytes" (fun () -> Span.length (toBytes w small))
        bench "1000 users: Json.print" (fun () -> Json.print (toJson large))
        bench "1000 users: zjson toString" (fun () -> toString w large)
        bench "1000 users: zjson toBytes" (fun () -> Span.length (toBytes w large))
        bench "read 10 users: Json.parse + fromJson" (fun () -> stdRead smallText)
        bench "read 10 users: zjson fromString" (fun () -> zjsonRead smallText)
        bench "read 1000 users: Json.parse + fromJson" (fun () -> stdRead largeText)
        bench "read 1000 users: zjson fromString" (fun () -> zjsonRead largeText)
        bench "read 1000 users: zjson fromBytes" (fun () ->
            match (fromBytes largeBytes : result<array<User>, string>) with
            | Ok users -> Array.length users
            | Error e -> panic e)
        testFaster "zjson toString is faster than Json.print" (fun () -> toString w large) (fun () -> Json.print (toJson large))
    ]
