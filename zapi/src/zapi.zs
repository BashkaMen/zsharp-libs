open Zerver
open Zjson
open TaskResult

type Incoming = { request: Http.Request; params: list<string * string> }
type Raw = { request: Http.Request; params: list<string * string>; body: Buffer<u8> }

trait FromValue<'a> =
    abstract fromValue : option<string> -> result<'a, string>

impl FromValue<string> =
    let fromValue (s: option<string>) =
        match s with
        | Some s -> Ok s
        | None -> Error "missing"

impl FromValue<int> =
    let fromValue (s: option<string>) =
        match s with
        | None -> Error "missing"
        | Some s ->
            match String.parseInt s with
            | Some n -> Ok n
            | None -> Error $"expected an int, got {s}"

impl FromValue<bool> =
    let fromValue (s: option<string>) =
        match s with
        | Some "true" -> Ok true
        | Some "false" -> Ok false
        | Some s -> Error $"expected true or false, got {s}"
        | None -> Error "missing"

impl FromValue<option<'a>> =
    let fromValue (s: option<string>) =
        match s with
        | None -> Ok None
        | Some _ -> fromValue s |> Result.map Some

trait FromQuery<'a> =
    abstract fromQuery : list<string * string> -> result<'a, string>

derive FromQuery =
    let fromQuery (pairs: list<string * string>) : result<'self, string> =
        match Shape.kind with
        | RecordKind ->
            Shape.build (fun f ->
                List.tryFind (fun (k, _) -> k = f.name) pairs
                |> Option.map snd
                |> fromValue
                |> Result.mapError (fun e -> $"query {f.name}: {e}"))
        | UnionKind -> Error "FromQuery needs a record"

type Path<'a> = | Path of 'a
type Header<'a> = | Header of 'a
type Query<'a> = | Query of 'a
type Body<'a> = | Body of 'a
type Headers =
    { values: Map<string, string> }
    member h.get(name: string) : option<string> = Map.tryFind name h.values

trait ToResponse<'e> =
    abstract toResponse : 'e -> Http.Response

type BadRequest = | BadRequest of string

impl ToResponse<BadRequest> =
    let toResponse (e: BadRequest) =
        let (BadRequest message) = e
        Http.Response.empty () |> Http.Response.withCode 400 |> Http.Response.withBody (Http.ResponseBody.fromText message)

trait FromPart<'a, 'e> =
    abstract fromPart : string -> Raw -> result<'a, 'e>

let private lookup (key: string) (pairs: list<string * string>) =
    List.tryFind (fun (k, _) -> k = key) pairs |> Option.map snd

let private headerName (field: string) =
    String.chars field
    |> List.collect (fun c -> if Char.isUpper c then [ '-'; Char.toLower c ] else [ c ])
    |> String.ofChars

let private queryPairs (query: string) =
    String.split "&" query
    |> List.choose (fun part ->
        match String.split "=" part with
        | [ "" ] -> None
        | [ k ] -> Some (k, "")
        | k :: v :: _ -> Some (k, v)
        | [] -> None)

impl FromPart<Path<'a>, BadRequest> =
    let fromPart (name: string) (raw: Raw) =
        fromValue (lookup name raw.params) |> Result.map Path |> Result.mapError (fun e -> BadRequest $"path {name}: {e}")

impl FromPart<Header<'a>, BadRequest> =
    let fromPart (name: string) (raw: Raw) =
        let header = headerName name
        fromValue (lookup header raw.request.headers) |> Result.map Header |> Result.mapError (fun e -> BadRequest $"header {header}: {e}")

impl FromPart<Headers, BadRequest> =
    let fromPart (name: string) (raw: Raw) : result<Headers, BadRequest> = Ok { values = Map.ofList raw.request.headers }

impl FromPart<Query<'a>, BadRequest> =
    let fromPart (name: string) (raw: Raw) = fromQuery (queryPairs raw.request.query) |> Result.map Query |> Result.mapError BadRequest

impl FromPart<Body<'a>, BadRequest> =
    let fromPart (name: string) (raw: Raw) =
        if raw.request.body.length = 0 then Error (BadRequest "body: missing")
        else fromBytes (Buffer.slice raw.body 0 raw.request.body.length) |> Result.map Body |> Result.mapError (fun e -> BadRequest $"body: {e}")

trait FromRequest<'a> =
    abstract fromRequest : Raw -> result<'a, Http.Response>

derive FromRequest =
    let fromRequest (raw: Raw) : result<'self, Http.Response> =
        match Shape.kind with
        | RecordKind -> Shape.build (fun f -> fromPart f.name raw |> Result.mapError toResponse)
        | UnionKind -> Error (toResponse (BadRequest "FromRequest needs a record"))

[<Derive(Serialize)>]
type NoBody = | NoBody

type Reply<'a> =
    | Json of int * 'a
    | Created of string * 'a
    | NoContent
    | File of string * string
    | Custom of Http.Response

module Reply =
    let ok (value: 'a) : Reply<'a> = Json (200, value)
    let created (location: string) (value: 'a) : Reply<'a> = Created (location, value)
    let noContent () : Reply<NoBody> = NoContent
    let file (contentType: string) (path: string) : Reply<NoBody> = File (contentType, path)

let private render (reply: Reply<'a>) : Http.Response =
    match reply with
    | Json (code, value) -> Http.Response.empty () |> Http.Response.withJsonOf code value
    | Created (location, value) ->
        Http.Response.empty () |> Http.Response.withJsonOf 201 value |> Http.Response.withHeader "location" location
    | NoContent -> Http.Response.empty () |> Http.Response.withCode 204
    | File (contentType, path) ->
        match File.readAllBytes path with
        | Ok bytes ->
            let buffer = Buffer.ofSpan bytes
            let body : Http.ResponseBody = { length = Buffer.length buffer; write = fun send -> send buffer (Buffer.length buffer) }
            Http.Response.empty ()
            |> Http.Response.withHeader "content-type" contentType
            |> Http.Response.withBody body
        | Error _ -> Http.Response.empty () |> Http.Response.withCode 404
    | Custom response -> response

let rec private readInto (body: Http.Body) (all: Buffer<u8>) (chunk: Buffer<u8>) (at: int) : Task<result<unit, string>> = task {
    if at >= body.length then return Ok ()
    else
        let! n = body.read chunk
        match n with
        | Error e -> return Error e
        | Ok 0 -> return Ok ()
        | Ok n ->
            for i in 0 .. n - 1 do
                all.[at + i] <- chunk.[i]
            return! readInto body all chunk (at + n)
}


let private handle (endpoint: 'req -> Task<result<Reply<'ok>, 'err>>) (incoming: Incoming) : Task<Http.Response> = task {
    let request = incoming.request
    let all = Buffer.create request.body.length (u8 0)
    let! response = taskResult {
        do! readInto request.body all (Buffer.create 16384 (u8 0)) 0 |> TaskResult.mapError (fun e -> toResponse (BadRequest e))
        let! req = TaskResult.ofResult (fromRequest { request = request; params = incoming.params; body = all })
        let! reply = endpoint req |> TaskResult.mapError toResponse
        return render reply
    }
    match response with
    | Ok r -> return r
    | Error r -> return r
}

type Handler<'ctx> = 'ctx -> Incoming -> Task<Http.Response>
type Endpoint<'ctx> = Http.Method * string * share<Handler<'ctx>>
type Middleware<'ctx> = share<Handler<'ctx>> -> share<Handler<'ctx>>

module Endpoint =
    let private make (method: Http.Method) (path: string) (endpoint: share<'ctx -> 'req -> Task<result<Reply<'ok>, 'err>>>) : Endpoint<'ctx> =
        let h : share<Handler<'ctx>> = fun ctx incoming -> handle (endpoint ctx) incoming
        (method, path, h)
    let get path endpoint = make Http.Get path endpoint
    let post path endpoint = make Http.Post path endpoint
    let put path endpoint = make Http.Put path endpoint
    let delete path endpoint = make Http.Delete path endpoint
    let wrap (middleware: Middleware<'ctx>) (e: Endpoint<'ctx>) : Endpoint<'ctx> =
        let (method, path, h) = e
        (method, path, middleware h)

let private matchPath (pattern: string) (path: string) : option<list<string * string>> =
    let rec go (ps: list<string>) (xs: list<string>) (acc: list<string * string>) =
        match (ps, xs) with
        | ([], []) -> Some (List.rev acc)
        | (p :: ps, x :: xs) when String.startsWith ":" p -> go ps xs ((String.sub p 1 (String.length p - 1), x) :: acc)
        | (p :: ps, x :: xs) when p = x -> go ps xs acc
        | _ -> None
    go (String.split "/" pattern) (String.split "/" path) []

let rec private dispatch (deps: 'deps) (endpoints: list<Endpoint<'deps>>) (request: Http.Request) : Task<Http.Response> =
    match endpoints with
    | [] -> Task.ret (Http.Response.empty () |> Http.Response.withCode 404)
    | (method, pattern, h) :: rest ->
        match matchPath pattern request.path with
        | Some params when method = request.method -> h deps { request = request; params = params }
        | _ -> dispatch deps rest request

let serve (config: ZerverOptions) (deps: 'deps) (endpoints: list<Endpoint<'deps>>) =
    Zerver.serve config (fun request -> dispatch deps endpoints request)
