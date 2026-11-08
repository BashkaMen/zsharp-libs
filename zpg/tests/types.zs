open Testing
open Zjson
open Zpg

// Each type goes to the server as a parameter and comes back as a column, or is read from a
// literal the server writes, so what's checked is Postgres's own text format.

[<Derive(Serialize, Deserialize)>]
type Meta = { tags: list<string>; n: int; note: option<string> }

[<Derive(FromRow)>]
type Moment = { t: DateTime }

[<Derive(FromRow)>]
type Day = { d: Date }

[<Derive(FromRow)>]
type Clock = { t: TimeOfDay }

[<Derive(FromRow)>]
type Span = { d: Interval }

[<Derive(FromRow)>]
type Doc = { j: Jsonb<Meta> }

[<Derive(FromRow)>]
type Raw = { s: string }

[<Derive(FromRow)>]
type Blob = { b: Bytea }

[<Derive(FromRow)>]
type Ints = { xs: list<int> }

[<Derive(FromRow)>]
type IntArray = { xs: array<int> }

[<Derive(FromRow)>]
type Texts = { xs: list<option<string>> }

[<Derive(FromRow)>]
type Grid = { xs: list<list<int>> }

[<Derive(FromRow)>]
type Num = { x: f64 }

[<Derive(FromRow)>]
type Count = { n: int }

[<Derive(FromRow)>]
type Event = {
    id: int
    at: DateTime
    day: option<Date>
    meta: Jsonb<Meta>
    tags: list<string>
}

let private url () =
    match Environment.getVariable "ZPG_URL" with
    | Some s when s <> "" -> s
    | _ -> "host=127.0.0.1 port=5432 dbname=zpg_test user=zpg password=zpg"

let private pool (n: int) : Pool =
    match Pool.create (url ()) { maxSize = n } with
    | Ok p -> p
    | Error e -> panic e

let private moment (y: int) (mo: int) (d: int) (h: int) (mi: int) (s: int) (ns: int) : DateTime =
    match DateTime.ofParts y mo d h mi s ns with
    | Some t -> t
    | None -> panic "not a moment"

// One row of `sql` on a fresh pooled connection.
let private one (sql: string) (args: list<Value>) : Task<result<'a, string>> = task {
    use db = pool 1
    return! Pool.withConnection db (fun conn -> queryOne conn sql args)
}

// `sql` after `setup` on the same connection.
let private after (setup: list<string>) (sql: string) (args: list<Value>) : Task<result<'a, string>> = task {
    use db = pool 1
    return! Pool.withConnection db (fun conn -> task {
        let rec each (rest: list<string>) = task {
            match rest with
            | [] -> return Ok ()
            | s :: more ->
                let! ran = exec conn s []
                let _ = ran?
                return! each more
        }
        let! done = each setup
        done?
        return! queryOne conn sql args
    })
}

let private fails (got: result<'a, string>) (part: string) : result<unit, string> =
    match got with
    | Ok _ -> Error $"expected an error with '{part}'"
    | Error e ->
        if String.contains part e then Ok () else Error $"expected '{part}' in: {e}"

[<Tests>]
let tests =
    testList "zpg types" [
        test "timestamptz goes and comes back" (fun () -> task {
                let t = moment 2026 11 4 8 30 15 123456000
                let! got = (one "select $1::timestamptz as t" [ Timestamp t ] : Task<result<Moment, string>>)
                let m = got?
                assert (m.t = t)
                return Ok ()
            })

        test "timestamptz reads in any session time zone" (fun () -> task {
                let t = moment 2026 7 1 12 0 0 0
                let zones = [ "UTC"; "Asia/Kolkata"; "America/St_Johns"; "Europe/Kyiv"; "Pacific/Chatham" ]
                let rec each (rest: list<string>) = task {
                    match rest with
                    | [] -> return Ok ()
                    | zone :: more ->
                        let! got =
                            (after [ $"set timezone = '{zone}'" ] "select $1::timestamptz as t" [ Timestamp t ] : Task<result<Moment, string>>)
                        let m = got?
                        assert (m.t = t)
                        return! each more
                }
                return! each zones
            })

        test "an offset with seconds (local mean time) reads" (fun () -> task {
                // Kyiv's offset in 1900 was +02:02:04
                let! got =
                    (after [ "set timezone = 'Europe/Kyiv'" ] "select '1900-01-01 00:00:00+00'::timestamptz as t" [] : Task<result<Moment, string>>)
                let m = got?
                assert (m.t = moment 1900 1 1 0 0 0 0)
                return Ok ()
            })

        test "timestamp without a zone is UTC" (fun () -> task {
                let t = moment 1999 12 31 23 59 59 500000000
                let! got =
                    (after [ "set timezone = 'Asia/Tokyo'" ] "select $1::timestamp as t" [ Timestamp t ] : Task<result<Moment, string>>)
                let m = got?
                assert (m.t = t)
                let! literal = (one "select '2026-11-04 08:30:00'::timestamp as t" [] : Task<result<Moment, string>>)
                let l = literal?
                assert (l.t = moment 2026 11 4 8 30 0 0)
                return Ok ()
            })

        test "a date reads as Date, and as DateTime at midnight" (fun () -> task {
                let leap = { year = 2024; month = 2; day = 29 }
                let! got = (one "select $1::date as d" [ Date leap ] : Task<result<Day, string>>)
                let d = got?
                assert (d.d = leap)
                let! early = (one "select '0044-03-15'::date as d" [] : Task<result<Day, string>>)
                let e = early?
                assert (e.d = { year = 44; month = 3; day = 15 })
                let! asMoment = (one "select '2026-11-04'::date as t" [] : Task<result<Moment, string>>)
                let m = asMoment?
                assert (m.t = moment 2026 11 4 0 0 0 0)
                return Ok ()
            })

        test "time of day" (fun () -> task {
                let t = { hour = 23; minute = 5; second = 9; nanos = 250000000 }
                let! got = (one "select $1::time as t" [ Time t ] : Task<result<Clock, string>>)
                let c = got?
                assert (c.t = t)
                let! midnight = (one "select '24:00:00'::time as t" [] : Task<result<Clock, string>>)
                let m = midnight?
                assert (m.t = { hour = 24; minute = 0; second = 0; nanos = 0 })
                return Ok ()
            })

        test "interval as Duration" (fun () -> task {
                let ns (n: i64) : Duration = n * 1<ns>
                let second = i64 1000000000
                let spans = [
                    ns ((i64 86400 + i64 7384) * second + i64 500000000)
                    ns (i64 0 - i64 90 * second - i64 500000000)
                    ns (i64 0)
                    ns (i64 1000 * i64 3600 * second)
                ]
                let rec each (rest: list<Duration>) = task {
                    match rest with
                    | [] -> return Ok ()
                    | d :: more ->
                        let! got = (one "select $1::interval as d" [ Interval d ] : Task<result<Span, string>>)
                        let s = got?
                        assert (s.d.duration = d)
                        return! each more
                }
                let! all = each spans
                all?
                let! mixed = (one "select interval '-1 day 2 hours' as d" [] : Task<result<Span, string>>)
                let m = mixed?
                assert (m.d.duration = ns ((i64 7200 - i64 86400) * second))
                let! months = (one "select interval '1 mon' as d" [] : Task<result<Span, string>>)
                return fails months "months"
            })

        test "jsonb with zjson, and json as text" (fun () -> task {
                let meta = { tags = [ "a"; "b \"q\"" ]; n = 7; note = None }
                let! got = (one "select $1::jsonb as j" [ Value.json meta ] : Task<result<Doc, string>>)
                let d = got?
                assert (d.j.value = meta)
                let! field = (one "select $1::jsonb ->> 'n' as s" [ Value.json meta ] : Task<result<Raw, string>>)
                let f = field?
                assert (f.s = "7")
                let! raw = (one "select $1::json as s" [ Json "{\"k\": [1, 2]}" ] : Task<result<Raw, string>>)
                let r = raw?
                assert (r.s = "{\"k\": [1, 2]}")
                let! bad = (one "select '{\"tags\": 1}'::jsonb as j" [] : Task<result<Doc, string>>)
                return fails bad "json"
            })

        test "bytea" (fun () -> task {
                let bytes = [| u8 0; u8 1; u8 127; u8 128; u8 255; u8 92; u8 39 |]
                let! got = (one "select $1::bytea as b" [ Bytes bytes ] : Task<result<Blob, string>>)
                let b = got?
                assert (Array.toList b.b.bytes = Array.toList bytes)
                let! empty = (one "select ''::bytea as b" [] : Task<result<Blob, string>>)
                let e = empty?
                assert (Array.length e.b.bytes = 0)
                return Ok ()
            })

        test "an int array, and = any($1)" (fun () -> task {
                let! got = (one "select $1::int[] as xs" [ Array [ Int 3; Int -1; Int 0 ] ] : Task<result<Ints, string>>)
                let r = got?
                assert (r.xs = [ 3; -1; 0 ])
                let! asArray = (one "select array[1, 2]::int[] as xs" [] : Task<result<IntArray, string>>)
                let a = asArray?
                assert (Array.toList a.xs = [ 1; 2 ])
                let! empty = (one "select '{}'::int[] as xs" [] : Task<result<Ints, string>>)
                let e = empty?
                assert (e.xs = [])
                let! counted =
                    (one "select count(*)::int as n from generate_series(1, 10) as g where g = any($1)" [ Array [ Int 2; Int 4; Int 99 ] ] : Task<result<Count, string>>)
                let c = counted?
                assert (c.n = 2)
                return Ok ()
            })

        test "a text array keeps quotes, commas, backslashes and NULL" (fun () -> task {
                let items = [ Some "plain"; Some "a,b"; Some "say \"hi\""; Some "back\\slash"; Some ""; Some "NULL"; None; Some "{x}"; Some "юникод ✓"; Some " pad " ]
                let values =
                    List.map (fun (item: option<string>) ->
                        match item with
                        | Some s -> Text s
                        | None -> Null) items
                let! got = (one "select $1::text[] as xs" [ Array values ] : Task<result<Texts, string>>)
                let r = got?
                assert (r.xs = items)
                return Ok ()
            })

        test "a two-dimensional array" (fun () -> task {
                let grid = Array [ Array [ Int 1; Int 2 ]; Array [ Int 3; Int 4 ] ]
                let! got = (one "select $1::int[][] as xs" [ grid ] : Task<result<Grid, string>>)
                let g = got?
                assert (g.xs = [ [ 1; 2 ]; [ 3; 4 ] ])
                return Ok ()
            })

        test "NaN and infinities" (fun () -> task {
                let! inf = (one "select $1::float8 as x" [ Float (1.0 / 0.0) ] : Task<result<Num, string>>)
                let i = inf?
                assert (i.x = 1.0 / 0.0)
                let! neg = (one "select '-Infinity'::float8 as x" [] : Task<result<Num, string>>)
                let n = neg?
                assert (n.x = -1.0 / 0.0)
                let! nan = (one "select $1::float8 as x" [ Float (0.0 / 0.0) ] : Task<result<Num, string>>)
                let m = nan?
                assert (m.x <> m.x)
                return Ok ()
            })

        test "a record of several types from a table" (fun () -> task {
                use db = pool 1
                let meta = { tags = [ "x" ]; n = 1; note = Some "n" }
                let at = moment 2026 1 2 3 4 5 0
                let! got = Pool.withConnection db (fun conn -> task {
                        let! dropped = exec conn "drop table if exists pg_temp.zpg_t_types" []
                        let _ = dropped?
                        let! created =
                            exec conn "create temp table zpg_t_types (id int primary key, at timestamptz not null, day date, meta jsonb not null, tags text[] not null)" []
                        let _ = created?
                        let! inserted =
                            exec conn "insert into zpg_t_types values ($1, $2, $3, $4, $5), ($6, $7, $8, $9, $10)" [
                                Int 1; Timestamp at; Date { year = 2026; month = 1; day = 2 }; Value.json meta; Array [ Text "a"; Text "b" ]
                                Int 2; Timestamp at; Null; Json "{\"tags\": [], \"n\": 2}"; Array []
                            ]
                        let _ = inserted?
                        return! (query conn "select id, at, day, meta, tags from zpg_t_types order by id" [] : Task<result<list<Event>, string>>)
                    })
                let rows = got?
                assert (rows = [
                        { id = 1; at = at; day = Some { year = 2026; month = 1; day = 2 }; meta = { value = meta }; tags = [ "a"; "b" ] }
                        { id = 2; at = at; day = None; meta = { value = { tags = []; n = 2; note = None } }; tags = [] }
                    ])
                return Ok ()
            })

        test "Row.read reads any FromCell type" (fun () -> task {
                use db = pool 1
                let! got = Pool.withConnection db (fun conn -> task {
                        let! rows = (query conn "select '2026-11-04 08:30:00+00'::timestamptz as t, array[1,2] as xs, null::date as d" [] : Task<result<list<Row>, string>>)
                        return rows
                    })
                match got with
                | Error e -> return Error e
                | Ok [ row ] ->
                    let readT: result<DateTime, string> = Row.read row "t"
                    let t = readT?
                    let readXs: result<list<int>, string> = Row.read row "xs"
                    let xs = readXs?
                    let readD: result<option<Date>, string> = Row.read row "d"
                    let d = readD?
                    assert (t = moment 2026 11 4 8 30 0 0)
                    assert (xs = [ 1; 2 ])
                    assert (d = None)
                    let missing: result<DateTime, string> = Row.read row "d"
                    return fails missing "column d"
                | Ok _ -> return Error "one row expected"
            })

        test "server output settings are set to what zpg reads" (fun () -> task {
                let conninfo = url () + " options='-c DateStyle=German -c IntervalStyle=iso_8601'"
                match Pool.create conninfo { maxSize = 1 } with
                | Error e -> return Error e
                | Ok created ->
                    use db = created
                    let! got = Pool.withConnection db (fun conn ->
                        (queryOne conn "select '2026-11-04 08:30:00+00'::timestamptz as t" [] : Task<result<Moment, string>>))
                    let m = got?
                    assert (m.t = moment 2026 11 4 8 30 0 0)
                    let! span = Pool.withConnection db (fun conn ->
                        (queryOne conn "select interval '1 day 01:00:00' as d" [] : Task<result<Span, string>>))
                    let s = span?
                    assert (s.d.duration = (i64 90000 * i64 1000000000) * 1<ns>)
                    return Ok ()
            })

        test "infinity and BC are errors, not wrong values" (fun () -> task {
                let! inf = (one "select 'infinity'::timestamptz as t" [] : Task<result<Moment, string>>)
                (fails inf "timestamp")?
                let! bc = (one "select '0044-03-15 BC'::date as d" [] : Task<result<Day, string>>)
                return fails bc "date"
            })

        test "a NUL in a text parameter is an error" (fun () -> task {
                let! got = (one "select $1 as s" [ Text ("a" + String.ofByte (u8 0) + "b") ] : Task<result<Raw, string>>)
                return fails got "NUL"
            })
    ]
