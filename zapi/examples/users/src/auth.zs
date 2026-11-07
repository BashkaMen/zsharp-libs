open Zerver
open Zapi

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
