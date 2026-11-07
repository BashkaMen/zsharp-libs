open Zerver

let rec private pipe (body: Http.Body) (chunk: Buffer<u8>) (send: Http.Send) : Task<result<unit, string>> = task {
    let! n = body.read chunk
    match n with
    | Error e -> return Error e
    | Ok 0 -> return Ok ()
    | Ok n ->
        let! sent = send chunk n
        match sent with
        | Error e -> return Error e
        | Ok () -> return! pipe body chunk send
}

let handler (req: Http.Request) = task {
    let echo : Http.ResponseBody = { length = req.body.length; write = pipe req.body (Buffer.create 16384 (u8 0)) }
    return Http.Response.empty () |> Http.Response.withBody echo
}

let main () =
  let logged =
    Zlog.LoggerConfig.empty
    |> Zlog.LoggerConfig.withSink (Zlog.Sinks.Console.create Zlog.Format.text)
    |> Zlog.LoggerConfig.runWith (fun () -> Zerver.serve Zerver.defaultConfig handler)
  match Task.run logged with
  | Error e ->
    eprintln $"zerver: {e}"
    1
  | Ok () -> 0
