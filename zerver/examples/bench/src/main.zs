open Zerver


[<Derive(ToJson)>]
type Hello = { hello: string }

let handler (req: Http.Request) = task {
    match req.path with
    | "/json" -> return Http.Response.empty |> Http.Response.withJsonOf 200 { hello = "world!" }
    | _ -> return Http.Response.empty |> Http.Response.withBody (Http.ResponseBody.fromText "hello world!")
}

let main () =
  let config = { Zerver.defaultConfig with handler = handler }
  match Task.run (Zerver.serve config) with
  | Error e ->
    eprintln $"zerver: {e}"
    1
  | Ok () -> 0
