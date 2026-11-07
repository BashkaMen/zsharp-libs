open Zerver
open Zjson
open Zapi

type Db = { url: string }

type Deps = { db: Db }

module Middleware =
    let requestLog (next: share<Handler<Deps>>) : share<Handler<Deps>> =
        fun deps incoming -> task {
            let! response = next deps incoming
            eprintln $"{incoming.request.method} {incoming.request.path} {response.code}"
            return response
        }

type CurrentUser = { id: string }

type AuthError =
    | MissingToken
    | Forbidden of string

impl ToResponse<AuthError> =
    let toResponse (e: AuthError) =
        match e with
        | MissingToken -> Http.Response.empty () |> Http.Response.withCode 401
        | Forbidden token -> Http.Response.empty () |> Http.Response.withJsonOf 403 $"token {token} is not allowed"

impl FromPart<CurrentUser, AuthError> =
    let fromPart (name: string) (raw: Raw) : result<CurrentUser, AuthError> =
        match List.tryFind (fun (k, _) -> k = "authorization") raw.request.headers with
        | None -> Error MissingToken
        | Some (_, "Bearer secret") -> Ok { id = "ann" }
        | Some (_, token) -> Error (Forbidden token)

[<Derive(Serialize, Deserialize)>]
type User = { id: int; name: string; age: int }

module GetUser =
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

    let handler (deps: Deps) (request: Request) : Task<result<Reply<User>, Error>> = task {
        let (Path id) = request.id
        let (Header traceId) = request.traceId
        eprintln $"trace={traceId} agent={request.headers.get "user-agent"} db={deps.db.url}"
        if id = 1 then return Ok (Reply.ok { id = 1; name = "ann"; age = 30 })
        else return Error (NotFound id)
    }

    let endpoint =
        Endpoint.get "/api/user/:id" handler
        |> Endpoint.wrap Middleware.requestLog

module CreateUser =
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

    let handler (deps: Deps) (request: Request) : Task<result<Reply<User>, Error>> = task {
        let (Query p) = request.query
        let (Body user) = request.body
        match user with
        | { age = age } when age < 18 -> return Error (TooYoung age)
        | { name = "admin" } -> return Error (Taken user.name)
        | _ ->
            if p.dryRun <> Some true then eprintln $"insert {user.name} by {request.author.id} into {deps.db.url}"
            return Ok (Reply.created "/api/user/2" { id = 2; name = user.name; age = user.age })
    }

    let endpoint =
        Endpoint.post "/api/user" handler
        |> Endpoint.wrap Middleware.requestLog

module Manifest =
    [<Derive(FromRequest)>]
    type Request = { format: Path<string> }

    type Error = | Unsupported of string

    impl ToResponse<Error> =
        let toResponse (e: Error) =
            match e with
            | Unsupported format -> Http.Response.empty () |> Http.Response.withJsonOf 404 $"no manifest as {format}"

    let handler (deps: Deps) (request: Request) : Task<result<Reply<NoBody>, Error>> = task {
        match request.format with
        | Path "toml" -> return Ok (Reply.file "application/toml" "zs.toml")
        | Path format -> return Error (Unsupported format)
    }

    let endpoint = Endpoint.get "/manifest/:format" handler

let main () =
    let endpoints = [
        GetUser.endpoint
        CreateUser.endpoint
        Manifest.endpoint
    ]
    match Task.run (Zapi.serve Zerver.defaultConfig { db = { url = "postgres://local" } } endpoints) with
    | Error e ->
        eprintln $"users: {e}"
        1
    | Ok () -> 0
