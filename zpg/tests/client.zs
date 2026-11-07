open Testing
open Zpg

[<Derive(FromRow)>]
type User = { id: int; name: string; note: option<string>; active: bool; score: f64 }

[<Derive(FromRow)>]
type Pid = { pid: int }

[<Derive(FromRow)>]
type Count = { n: int }

[<Derive(FromRow)>]
type WithDefault = { id: int; role: string = "guest" }

let private url () =
    match Environment.getVariable "ZPG_URL" with
    | Some s when s <> "" -> s
    | _ -> "host=127.0.0.1 port=5432 dbname=zpg_test user=zpg password=zpg"

let private pool (n: int) : Pool =
    match Pool.create (url ()) { maxSize = n } with
    | Ok p -> p
    | Error e -> panic e

let private sql (conn: Conn) (text: string) (args: list<Value>) : Task<result<unit, string>> = task {
    let! n = exec conn text args
    let _ = n?
    return Ok ()
}

let private readUsers (conn: Conn) : Task<result<list<User>, string>> =
    query conn "select id, name, note, active, score from zpg_t_read order by id" []

let private readCount (conn: Conn) (table: string) : Task<result<list<Count>, string>> =
    query conn $"select count(*)::int as n from {table}" []

let private readPid (conn: Conn) : Task<result<Pid, string>> =
    queryOne conn "select pg_backend_pid() as pid" []

let private readDefault (conn: Conn) : Task<result<list<WithDefault>, string>> =
    query conn "select 7 as id" []

let private readAnswer (conn: Conn) : Task<result<Count, string>> =
    queryOne conn "select 41 + $1 as n" [ Int 1 ]

[<Tests>]
let tests =
    testList "zpg" [
        test "reads a parameterized row into a record" (fun () -> task {
                use db = pool 2
                let! got = Pool.withConnection db (fun conn -> task {
                        let! dropped = sql conn "drop table if exists zpg_t_read" []
                        dropped?
                        let! created =
                            sql conn "create table zpg_t_read (id int primary key, name text, note text, active bool, score float8)" []
                        created?
                        let! inserted =
                            sql conn "insert into zpg_t_read (id, name, note, active, score) values ($1, $2, $3, $4, $5), ($6, $7, $8, $9, $10)" [ Int 1; Text "ada"; Null; Bool true; Float 1.5; Int 2; Text "a'b \"q\""; Text "юникод ✓"; Bool false; Float -0.25 ]
                        inserted?
                        return! readUsers conn
                    })
                let users = got?
                assert (users = [
                        { id = 1; name = "ada"; note = None; active = true; score = 1.5 };
                        { id = 2; name = "a'b \"q\""; note = Some "юникод ✓"; active = false; score = -0.25 }
                    ])
                return Ok ()
            })

        test "reports Postgres's message and sqlstate" (fun () -> task {
                use db = pool 1
                let! got =
                    Pool.withConnection db (fun conn -> exec conn "select * from zpg_no_such_table" [])
                match got with
                | Ok _ -> return Error "the query should have failed"
                | Error e ->
                    assert (String.contains "42P01" e)
                    assert (String.contains "zpg_no_such_table" e)
                    return Ok ()
            })

        test "commits and rolls back" (fun () -> task {
                use db = pool 1
                let! got = Pool.withConnection db (fun conn -> task {
                        let! dropped = sql conn "drop table if exists zpg_t_tx" []
                        dropped?
                        let! created = sql conn "create table zpg_t_tx (id int primary key)" []
                        created?
                        let! rolled = transaction conn (fun () -> task {
                                let! inserted = sql conn "insert into zpg_t_tx (id) values ($1)" [ Int 1 ]
                                inserted?
                                return Error "nope"
                            })
                        match rolled with
                        | Ok _ -> return Error "the body should have failed"
                        | Error e ->
                            assert (String.contains "nope" e)
                            let! committed =
                                transaction conn (fun () ->
                                    sql conn "insert into zpg_t_tx (id) values ($1)" [ Int 2 ])
                            let _ = committed?
                            return! readCount conn "zpg_t_tx"
                    })
                let counts = got?
                assert (counts = [ { n = 1 } ])
                return Ok ()
            })

        test "reuses a pooled connection" (fun () -> task {
                use db = pool 1
                let! first = Pool.withConnection db readPid
                let! second = Pool.withConnection db readPid
                let a = first?
                let b = second?
                assert (a.pid = b.pid && a.pid > 0)
                return Ok ()
            })

        test "takes a field's default when the column is absent" (fun () -> task {
                use db = pool 1
                let! got = Pool.withConnection db readDefault
                let rows = got?
                assert (rows = [ { id = 7; role = "guest" } ])
                return Ok ()
            })

        test "begin and rollback are explicit" (fun () -> task {
                use db = pool 1
                let! got = Pool.withConnection db (fun conn -> task {
                        let! dropped = sql conn "drop table if exists zpg_t_begin" []
                        dropped?
                        let! created = sql conn "create table zpg_t_begin (id int primary key)" []
                        created?
                        let! began = begin conn
                        began?
                        let! inserted = sql conn "insert into zpg_t_begin (id) values (1)" []
                        inserted?
                        let! rolled = rollback conn
                        rolled?
                        return! readCount conn "zpg_t_begin"
                    })
                let counts = got?
                assert (counts = [ { n = 0 } ])
                return Ok ()
            })

        test "a connect of its own runs a query" (fun () -> task {
                let! opened = connect (url ())
                let conn = opened?
                use c = conn
                let! row = readAnswer c
                let got = row?
                assert (got.n = 42)
                return Ok ()
            })

        test "keeps at most maxSize connections checked out" (fun () -> task {
                use db = pool 2
                let held = Atomic.make 0
                let peak = Atomic.make 0
                let one (i: int) = task {
                    let! ran = Pool.withConnection db (fun conn -> task {
                            let n = Atomic.add held 1
                            let rec raise () =
                                let p = Atomic.get peak
                                if n > p && Atomic.compareExchange peak p n <> p then raise ()
                            raise ()
                            do! Task.sleep 80
                            Atomic.add held (0 - 1) |> ignore
                            return! exec conn "select $1::int" [ Int i ]
                        })
                    let got = ran?
                    assert (got = 1)
                    return Ok ()
                }
                let! all = Task.all (List.init 6 (fun i -> Task.spawn (fun () -> one i)))
                List.iter (fun (r: result<unit, string>) ->
                    match r with
                    | Ok () -> ()
                    | Error e -> panic e) all
                assert (Atomic.get peak = 2)
                return Ok ()
            })

        test "cancelling a query gives the pool its slot back" (fun () -> task {
                use db = pool 1
                let hung = Pool.withConnection db (fun conn -> exec conn "select pg_sleep(30)" [])
                let! first = Task.timeout 200 hung
                match first with
                | Some _ -> return Error "pg_sleep should still have been running"
                | None ->
                    let! second =
                        Task.timeout 2000 (Pool.withConnection db (fun conn -> exec conn "select 1" []))
                    match second with
                    | Some (Ok n) ->
                        assert (n = 1)
                        return Ok ()
                    | Some (Error e) -> return Error e
                    | None -> return Error "the pool did not recover after cancelling a query"
            })

        test "a second command is an error and the connection still works" (fun () -> task {
            use db = pool 1
            let! got = Pool.withConnection db (fun conn -> task {
                let! failed = exec conn "select 1; select 2" []
                match failed with
                | Ok _ -> return Error "two commands should be rejected"
                | Error e ->
                    assert (String.contains "42601" e)
                    return! exec conn "select 1" []
            })
            let n = got?
            assert (n = 1)
            return Ok ()
        })

        test "rejects a maxSize below 1" (fun () ->
            match Pool.create (url ()) { maxSize = 0 } with
            | Error e -> assert (String.contains "maxSize" e)
            | Ok _ -> panic "maxSize 0 should be an error")
    ]
