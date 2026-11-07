open Zerver
open Zjson
open Zapi
open TaskResult
open App
open Auth
open Zlog

[<Derive(FromQuery)>]
type Params = { dryRun: option<bool> }

[<Derive(Serialize, Deserialize)>]
type NewUser = { name: string; age: int }

[<Derive(FromRequest)>]
type Request = {
    author: CurrentUser
    query: Query<Params>
    body: Body<NewUser>
}

type Error =
    | TooYoung of int
    | Taken of string

impl ToResponse<Error> =
    let toResponse (e: Error) =
        match e with
        | TooYoung age -> Http.Response.empty () |> Http.Response.withJsonOf 422 $"too young: {age}"
        | Taken name -> Http.Response.empty () |> Http.Response.withJsonOf 409 $"taken: {name}"

let handler (deps: Deps) (request: Request) : Task<result<Reply<User>, Error>> = taskResult {
    let (Query p) = request.query
    let (Body user) = request.body
    match user with
    | { age = age } when age < 18 -> return! error (TooYoung age)
    | { name = "admin" } -> return! error (Taken user.name)
    | _ ->
        let log = Logger.scope [ ("author", request.author.id) ]
        match p.dryRun with
        | Some true -> log.info (event "dry run" [ ("name", user.name) ])
        | _ -> log.info (event "user created" [ ("name", user.name); ("db", deps.db.url) ])
        return Reply.created "/api/user/2" { id = 2; name = user.name; age = user.age }
}

let endpoint =
    Endpoint.post "/api/user" handler
    |> Endpoint.wrap Middleware.requestLog
