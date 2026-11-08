open Zhttp
open Zlog

let main () =
    let url =
        match Environment.args () with
        | first :: _ -> first
        | [] -> "https://example.com/"
    let send =
        Client.create ()
        |> Client.withTimeout 15000
        |> Client.sender
        |> Middleware.log
    let req = Request.get url |> Request.header "accept" "text/html, text/plain;q=0.9"
    let logged =
        LoggerConfig.empty
        |> LoggerConfig.withSink (Sinks.Console.create Format.text)
        |> LoggerConfig.runWith (fun () -> task {
            let! result = send req
            match result with
            | Error e -> return Error e
            | Ok res ->
                let! text = Response.text res
                match text with
                | Error e -> return Error e
                | Ok text -> return Ok (res.status, text)
        })
    match Task.run logged with
    | Error e ->
        eprintln $"get: {message e}"
        1
    | Ok (status, text) ->
        println $"{status}"
        println text
        0
