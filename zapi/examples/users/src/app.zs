open Zerver
open Zjson
open Zapi
open Zlog

type Db = { url: string }

type Deps = { db: Db }

module Middleware =
    let requestLog (next: share<Handler<Deps>>) : share<Handler<Deps>> =
        fun deps incoming -> task {
            let! response = next deps incoming
            Logger.info (event "request" [ ("method", string incoming.request.method); ("path", incoming.request.path); ("code", string response.code) ])
            return response
        }

[<Derive(Serialize, Deserialize)>]
type User = { id: int; name: string; age: int }
