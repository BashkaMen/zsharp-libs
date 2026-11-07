# zapi

Typed endpoints on top of [zerver](../zerver/README.md), in the spirit of minimal APIs: each
endpoint is one module with its request, its errors, its handler and its route.

## Usage

```fsharp
open Zerver
open Zapi
open TaskResult

[<Derive(FromRequest)>]
type Request = {
    id: Path<int>                       // :id from the route
    traceId: Header<option<string>>     // header "trace-id", optional
    headers: Headers                    // every header: request.headers.get "user-agent"
}

type Error = | NotFound of int

impl ToResponse<Error> =
    let toResponse (e: Error) =
        match e with
        | NotFound id -> Http.Response.empty () |> Http.Response.withJsonOf 404 $"user {id} not found"

let handler (deps: Deps) (request: Request) : Task<result<Reply<User>, Error>> = taskResult {
    let (Path id) = request.id
    match id with
    | 1 -> return Reply.ok { id = 1; name = "ann"; age = 30 }
    | _ -> return! error (NotFound id)
}

let endpoint =
    Endpoint.get "/api/user/:id" handler
    |> Endpoint.wrap Middleware.requestLog
```

```fsharp
let main () =
    let endpoints = [ Endpoints.GetUser.endpoint; Endpoints.CreateUser.endpoint ]
    match Task.run (Zapi.serve Zerver.defaultConfig { db = { url = "postgres://local" } } endpoints) with
    | Error e ->
        eprintln $"users: {e}"
        1
    | Ok () -> 0
```

## Requests

`[<Derive(FromRequest)>]` builds a request record field by field; a field's type says where it
comes from, and its name which key:

| Field type | From |
|---|---|
| `Path<'a>` | the route parameter of the same name (`id` ← `/api/user/:id`) |
| `Header<'a>` | the header, the name in kebab case (`traceId` ← `trace-id`) |
| `Headers` | all headers, `.get name` |
| `Query<'a>` | the query string, into a record with `[<Derive(FromQuery)>]` |
| `Body<'a>` | the JSON body, through zjson's `Deserialize` |

Values parse as `string`, `int`, `bool` or `option` of them: an `option` field takes `None` when
the key is absent, any other is `400` with where it failed (`path id: expected an int, got abc`).

**Own binders:** `impl FromPart<T, E>` on your type, with `impl ToResponse<E>` for its error, puts a
`T` field in any request and answers with your status when it doesn't bind:

```fsharp
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
```

## Responses

A handler is `'deps -> 'req -> Task<result<Reply<'ok>, 'err>>`, with `impl ToResponse<'err>`.

| `Reply` | Response |
|---|---|
| `Reply.ok x` / `Json (code, x)` | `x` as JSON, `200` or `code` |
| `Reply.created location x` | `201`, `location` header, `x` as JSON |
| `Reply.noContent ()` | `204` |
| `Reply.file contentType path` | the file's bytes, `404` when it can't be read |
| `Custom response` | any `Http.Response` |

`Reply<NoBody>` is for replies without a JSON value.

## Endpoints and middleware

- **Routes:** `Endpoint.get`, `post`, `put`, `delete` with a path pattern; `:name` segments bind to
  `Path` fields. The first endpoint whose method and pattern match takes the request; none is `404`.
- **Middleware:** `share<Handler<'deps>> -> share<Handler<'deps>>`, added with `Endpoint.wrap`;
  the last `wrap` runs first.
- **Deps:** one value for the whole app, passed to `serve` and to every handler. It's shared by
  every worker thread, so it must be `Share`: data, `Atomic`, `Mutex`, `Channel`.

## Limitations

- Binders are synchronous: the derive builds the record through `result`s, so a binder can't wait on
  a task. A check that needs a database belongs in the handler or a middleware.
- The body is read whole before binding, up to zerver's `max_request_size`.
- No URL decoding in paths and query strings.
- Routes are tried in order: `/user/:id` listed before `/user/me` takes `/user/me` too.

## Example

```sh
cd examples/users
zs run
curl localhost:8080/api/user/1
curl -H 'authorization: Bearer secret' -d '{"name":"bob","age":30}' localhost:8080/api/user
```
