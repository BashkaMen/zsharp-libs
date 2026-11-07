open Zerver
open Zjson


[<Derive(ToJson, Serialize)>]
type Hello = { hello: string }

let private stdJson (value: 'a) =
    Http.Response.empty ()
    |> Http.Response.withHeader "content-type" "application/json"
    |> Http.Response.withBody (Http.ResponseBody.fromText (Json.print (toJson value)))

let handler (req: Http.Request) = task {
    match req.path with
    | "/json" -> return Http.Response.empty () |> Http.Response.withJsonOf 200 { hello = "world!" }
    | "/object" -> return Http.Response.empty () |> Http.Response.withJsonOf 200 Data.sample
    | "/list" -> return Http.Response.empty () |> Http.Response.withJsonOf 200 Data.items
    | "/std/json" -> return stdJson { hello = "world!" }
    | "/std/object" -> return stdJson Data.sample
    | "/std/list" -> return stdJson Data.items
    | _ -> return Http.Response.empty () |> Http.Response.withBody (Http.ResponseBody.fromText "hello world!")
}

let main () =
  match Task.run (Zerver.serve Zerver.defaultConfig handler) with
  | Error e ->
    eprintln $"zerver: {e}"
    1
  | Ok () -> 0
