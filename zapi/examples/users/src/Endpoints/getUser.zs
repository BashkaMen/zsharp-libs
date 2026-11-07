open Zerver
open Zapi
open TaskResult
open App
open Zlog

[<Derive(FromRequest)>]
type Request = {
    id: Path<int>
    traceId: Header<option<string>>
    headers: Headers
}

type Error = | NotFound of int

impl ToResponse<Error> =
    let toResponse (e: Error) =
        match e with
        | NotFound id -> Http.Response.empty () |> Http.Response.withJsonOf 404 $"user {id} not found"

let handler (deps: Deps) (request: Request) : Task<result<Reply<User>, Error>> = taskResult {
    let (Path id) = request.id
    let (Header traceId) = request.traceId
    Logger.debug (event "get user" [ ("id", string id); ("trace", string traceId); ("agent", string (request.headers.get "user-agent")) ])
    match id with
    | 1 -> return Reply.ok { id = 1; name = "ann"; age = 30 }
    | _ -> return! error (NotFound id)
}

let endpoint =
    Endpoint.get "/api/user/:id" handler
    |> Endpoint.wrap Middleware.requestLog
