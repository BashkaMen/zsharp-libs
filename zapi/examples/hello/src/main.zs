open Zerver

let handler (req: Http.Request) = task {
    return Http.Response.empty () |> Http.Response.withBody (Http.ResponseBody.fromText "hello")
}

let main () =
    match Task.run (Zerver.serve Zerver.defaultConfig handler) with
    | Error e ->
        eprintln $"hello: {e}"
        1
    | Ok () -> 0
