open Testing
open Zpg

[<Derive(FromRow)>]
type User = { id: int; name: string; note: option<string>; active: bool; score: f64 }

[<Derive(FromRow)>]
type Pid = { pid: int }

[<Derive(FromRow)>]
type Count = { n: int }

[<Derive(FromRow)>]
type Num = { x: f64 }

[<Derive(FromRow)>]
type Xact = { xid: option<string> }

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
                        let! dropped = sql conn "drop table if exists pg_temp.zpg_t_read" []
                        dropped?
                        let! created =
                            sql conn "create temp table zpg_t_read (id int primary key, name text, note text, active bool, score float8)" []
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
                        let! dropped = sql conn "drop table if exists pg_temp.zpg_t_tx" []
                        dropped?
                        let! created = sql conn "create temp table zpg_t_tx (id int primary key)" []
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
                        let! dropped = sql conn "drop table if exists pg_temp.zpg_t_begin" []
                        dropped?
                        let! created = sql conn "create temp table zpg_t_begin (id int primary key)" []
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

        test "a float goes to the server and back unchanged" (fun () -> task {
                use db = pool 1
                let xs = [ 0.1 + 0.2; 1.0 / 3.0; 4.9e-324; 1.7976931348623157e+308; -2.5; 100.0 ]
                let! got = Pool.withConnection db (fun conn -> task {
                        let rec each (rest: list<f64>) (acc: list<f64>) = task {
                            match rest with
                            | [] -> return Ok (List.rev acc)
                            | x :: more ->
                                let! row = (queryOne conn "select $1::float8 as x" [ Float x ] : Task<result<Num, string>>)
                                let r = row?
                                return! each more (r.x :: acc)
                        }
                        return! each xs []
                    })
                let back = got?
                assert (back = xs)
                return Ok ()
            })

        test "a connection given back mid-transaction is not reused in it" (fun () -> task {
                use db = pool 1
                let! first = Pool.withConnection db (fun conn -> task {
                        let! began = exec conn "begin" []
                        let _ = began?
                        // gives the transaction an id
                        return! exec conn "select pg_current_xact_id()" []
                    })
                let _ = first?
                let! second =
                    Pool.withConnection db (fun conn ->
                        (queryOne conn "select pg_current_xact_id_if_assigned()::text as xid" [] : Task<result<Xact, string>>))
                let x = second?
                assert (x.xid = None)
                return Ok ()
            })

        test "a commit the server turns into a rollback is an error" (fun () -> task {
                use db = pool 1
                let! got = Pool.withConnection db (fun conn ->
                    transaction conn (fun () -> task {
                        let! failed = exec conn "select 1/0" []
                        // the error is ignored, so the body says Ok
                        match failed with
                        | _ -> return Ok ()
                    }))
                match got with
                | Ok () -> return Error "COMMIT of an aborted transaction should not be Ok"
                | Error e ->
                    assert (String.contains "rolled back" e)
                    return Ok ()
            })

        test "cancelling an acquire while it connects frees the slot" (fun () -> task {
                // A listener that never answers: the TCP handshake completes, the startup
                // message gets no reply, so the connect waits until it is cancelled.
                // the first free port from 54329, so tests running at once don't collide
                let rec listen (port: int) =
                    match Tcp.listen "127.0.0.1" port with
                    | Ok l -> Ok (l, port)
                    | Error e -> if port < 54429 then listen (port + 1) else Error e
                match listen 54329 with
                | Error e -> return Error e
                | Ok (listener, port) ->
                    use l = listener
                    use db =
                        match Pool.create $"host=127.0.0.1 port={port} dbname=x user=x connect_timeout=30" { maxSize = 1 } with
                        | Ok p -> p
                        | Error e -> panic e
                    let! first = Task.timeout 200 (Pool.acquire db)
                    match first with
                    | Some _ -> return Error "the connect should still have been waiting"
                    | None ->
                        assert (Atomic.get db.live = 0)
                        return Ok ()
            })

        test "reads many rows" (fun () -> task {
                use db = pool 1
                let! got = Pool.withConnection db (fun conn ->
                    (query conn "select g as n from generate_series(1, 300000) as g" [] : Task<result<list<Count>, string>>))
                let rows = got?
                assert (List.length rows = 300000)
                return Ok ()
            })

        test "many tasks share a small pool, some cancelled on the way" (fun () -> task {
                use db = pool 3
                let one (i: int) = task {
                    // every third gives up early: while waiting for a slot, connecting, or mid-query
                    let work = Pool.withConnection db (fun conn -> exec conn "select pg_sleep(0.005), $1::int" [ Int i ])
                    if i % 3 = 0 then
                        let! cut = Task.timeout (i % 7 + 1) work
                        match cut with
                        | Some (Error e) -> return Error e
                        | _ -> return Ok ()
                    else
                        let! ran = work
                        match ran with
                        | Ok 1 -> return Ok ()
                        | Ok n -> return Error $"one row expected, got {n}"
                        | Error e -> return Error e
                }
                let! all = Task.all (List.init 60 (fun i -> Task.spawn (fun () -> one i)))
                List.iter (fun (r: result<unit, string>) ->
                    match r with
                    | Ok () -> ()
                    | Error e -> panic e) all
                assert (Atomic.get db.live <= 3)
                // and the pool still hands out every slot
                let! held = Task.timeout 3000 (Task.all (List.init 3 (fun _ -> Pool.withConnection db (fun conn -> exec conn "select pg_sleep(0.05)" []))))
                match held with
                | None -> return Error "the pool lost a slot"
                | Some rs ->
                    assert (List.forall Result.isOk rs)
                    return Ok ()
            })

        test "rejects a maxSize below 1" (fun () ->
            match Pool.create (url ()) { maxSize = 0 } with
            | Error e -> assert (String.contains "maxSize" e)
            | Ok _ -> panic "maxSize 0 should be an error")
    ]
