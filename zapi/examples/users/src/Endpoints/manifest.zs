open Zerver
open Zapi
open TaskResult
open App

[<Derive(FromRequest)>]
type Request = { format: Path<string> }

type Error = | Unsupported of string

impl ToResponse<Error> =
    let toResponse (e: Error) =
        match e with
        | Unsupported format -> Http.Response.empty () |> Http.Response.withJsonOf 404 $"no manifest as {format}"

let handler (deps: Deps) (request: Request) : Task<result<Reply<NoBody>, Error>> = taskResult {
    match request.format with
    | Path "toml" -> return Reply.file "application/toml" "zs.toml"
    | Path format -> return! error (Unsupported format)
}

let endpoint = Endpoint.get "/manifest/:format" handler
