open Zpg

[<Derive(FromRow)>]
type User = { id: int; name: string; note: option<string> }

let private url () =
    match Environment.getVariable "ZPG_URL" with
    | Some s when s <> "" -> s
    | _ -> "host=127.0.0.1 port=5432 dbname=zpg_test user=zpg password=zpg"

let private load (conn: Conn) : Task<result<list<User>, string>> = task {
    let! dropped = exec conn "drop table if exists zpg_example" []
    let _ = dropped?
    let! created = exec conn "create table zpg_example (id int primary key, name text not null, note text)" []
    let _ = created?
    let! inserted =
        exec conn "insert into zpg_example (id, name, note) values ($1, $2, $3), ($4, $5, $6)" [
            Int 1; Text "ada"; Null; Int 2; Text "grace"; Text "cobol"
        ]
    let _ = inserted?
    return! query conn "select id, name, note from zpg_example order by id" []
}

let main () =
    match Pool.create (url ()) { maxSize = 4 } with
    | Error e ->
        eprintln $"zpg: {e}"
        1
    | Ok pool ->
        use p = pool
        match Task.run (Pool.withConnection p load) with
        | Error e ->
            eprintln $"zpg: {e}"
            1
        | Ok users ->
            for u in users do
                match u.note with
                | Some n -> println $"{u.id} {u.name} {n}"
                | None -> println $"{u.id} {u.name}"
            0
