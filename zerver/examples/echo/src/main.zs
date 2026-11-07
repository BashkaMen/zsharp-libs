open Zerver

let private readError (e: Http.BodyReadError) =
    match e with
    | Http.Unknown text -> text

let rec private pipe (body: Http.Body) (chunk: Buffer<u8>) (send: Http.Send) : Task<result<unit, string>> = taskResult {
    let! n = body.read chunk |> TaskResult.mapError readError
    if n = 0 then
        return ()
    else
        do! send chunk n
        return! pipe body chunk send
}

let handler (req: Http.Request) = task {
    let echo : Http.ResponseBody = { length = req.body.length; write = pipe req.body (Buffer.create 16384 (u8 0)) }
    return Http.Response.empty () |> Http.Response.withBody echo
}

let main () =
  match Task.run (Zerver.serve Zerver.defaultConfig handler) with
  | Error e ->
    eprintln $"zerver: {e}"
    1
  | Ok () -> 0
