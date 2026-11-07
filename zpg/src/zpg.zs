// A Postgres client over libpq. Queries are non-blocking: the connection's socket is waited on
// with Task.waitFor, so other tasks on the loop keep running. The pool is safe to share across
// threads (a server's loop per core); a single connection is used by one task at a time.
open Internal
open Libpq

/// A value bound to `$1`, `$2`, … Text is sent as text, which is what Postgres's text format reads.
type Value =
    | Null
    | Text of string
    | Int of int
    | Bool of bool
    | Float of f64

/// One result row. Cells are text, or `None` for NULL; `Row.get` finds a column by its result name.
type Row = {
    names: array<string>
    cells: array<option<string>>
}

type private Outcome = { rows: list<Row>; affected: int }

// ---------------------------------------------------------------- rows

module Row =
    let private index (row: Row) (name: string) : int =
        let mutable found = -1
        let mutable i = 0
        while found < 0 && i < Array.length row.names do
            if row.names.[i] = name then found <- i
            i <- i + 1
        found

    /// The column's text, or `None` when it is NULL. `None` when the result has no such column.
    let tryGet (row: Row) (name: string) : option<option<string>> =
        let i = index row name
        if i < 0 then None else Some row.cells.[i]

    /// The column's text, or `None` when it is NULL.
    let get (row: Row) (name: string) : result<option<string>, string> =
        match tryGet row name with
        | Some cell -> Ok cell
        | None -> Error $"no column {name}"

    let private text (row: Row) (name: string) : result<string, string> =
        match get row name with
        | Ok (Some s) -> Ok s
        | Ok None -> Error $"column {name} is null"
        | Error e -> Error e

    /// The column as a string.
    let string (row: Row) (name: string) : result<string, string> = text row name

    /// The column as an `int`.
    let int (row: Row) (name: string) : result<int, string> =
        let s = (text row name)?
        match String.parseInt s with
        | Some n -> Ok n
        | None -> Error $"column {name} is not an int: {s}"

    /// The column as an `i64`.
    let int64 (row: Row) (name: string) : result<i64, string> =
        let s = (text row name)?
        match Int.parse s with
        | Some n -> Ok n
        | None -> Error $"column {name} is not an i64: {s}"

    /// The column as an `f64`.
    let float (row: Row) (name: string) : result<f64, string> =
        let s = (text row name)?
        match String.parseFloat s with
        | Some x -> Ok x
        | None -> Error $"column {name} is not a float: {s}"

    /// The column as a `bool` (`t`/`f`, as Postgres writes a boolean).
    let bool (row: Row) (name: string) : result<bool, string> =
        let s = (text row name)?
        let t = String.toLower s
        if t = "t" || t = "true" || t = "1" then Ok true
        else if t = "f" || t = "false" || t = "0" then Ok false
        else Error $"column {name} is not a bool: {s}"

/// A cell of a row, as a Z# value. `[<Derive(FromRow)>]` reads each field with this.
trait FromCell<'a> =
    abstract fromCell : option<string> -> result<'a, string>

impl FromCell<string> =
    let fromCell (cell: option<string>) : result<string, string> =
        match cell with
        | Some s -> Ok s
        | None -> Error "null"

impl FromCell<bool> =
    let fromCell (cell: option<string>) : result<bool, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            let t = String.toLower s
            if t = "t" || t = "true" || t = "1" then Ok true
            else if t = "f" || t = "false" || t = "0" then Ok false
            else Error $"not a bool: {s}"

impl FromCell<int> =
    let fromCell (cell: option<string>) : result<int, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            match String.parseInt s with
            | Some n -> Ok n
            | None -> Error $"not an int: {s}"

impl FromCell<i64> =
    let fromCell (cell: option<string>) : result<i64, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            match Int.parse s with
            | Some n -> Ok n
            | None -> Error $"not an i64: {s}"

impl FromCell<i32> =
    let fromCell (cell: option<string>) : result<i32, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            match Int.parse s with
            | Some n -> Ok n
            | None -> Error $"not an i32: {s}"

impl FromCell<f64> =
    let fromCell (cell: option<string>) : result<f64, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            match String.parseFloat s with
            | Some x -> Ok x
            | None -> Error $"not a float: {s}"

impl FromCell<option<'a>> =
    let fromCell (cell: option<string>) : result<option<'a>, string> =
        match cell with
        | None -> Ok None
        | Some _ -> Result.map Some (fromCell cell)

/// A record read from a row: each field from the column of the same name. A missing column takes
/// the field's default, or `None` for an `option`, and is an error otherwise. NULL is `None` for
/// an `option` and an error otherwise.
trait FromRow<'a> =
    abstract fromRow : Row -> result<'a, string>

derive FromRow =
    let fromRow (row: Row) : result<'self, string> =
        match Shape.kind with
        | UnionKind -> Error "FromRow reads a record"
        | RecordKind ->
            Shape.build (fun f ->
                match Row.tryGet row f.name with
                | Some cell ->
                    match fromCell cell with
                    | Ok v -> Ok v
                    | Error e -> Error $"column {f.name}: {e}"
                | None ->
                    match f.default with
                    | Some d -> Ok d
                    | None ->
                        match fromCell None with
                        | Ok v -> Ok v
                        | Error _ -> Error $"no column {f.name}")

let private render (v: Value) : option<string> =
    match v with
    | Null -> None
    | Text s -> Some s
    | Int n -> Some (string n)
    | Bool b -> Some (if b then "true" else "false")
    | Float x -> Some (formatFloat x)

let private message (conn: cptr) : string = copy (C.PQerrorMessage conn)

let private describe (conn: cptr) (res: cptr) : string =
    // 'C' is PG_DIAG_SQLSTATE. The header's char macro isn't a value import c keeps.
    let state = copy (C.PQresultErrorField res (i32 67))
    let msg = copy (C.PQresultErrorMessage res)
    let text = if msg <> "" then msg else message conn
    if state = "" then text else $"{state} {text}"

// ---------------------------------------------------------------- the protocol

let private finishId (id: int) =
    if id <> 0 then C.PQfinish (ptrOf id)

let private healthy (id: int) : bool = C.PQstatus (ptrOf id) = C.CONNECTION_OK

let private send (conn: cptr) (sql: string) (args: list<option<string>>) : result<unit, string> =
    let n = List.length args
    let buf: ptr<cptr> = if n = 0 then unsafe Ptr.null () else unsafe Ptr.alloc n
    let rec fill (i: int) (rest: list<option<string>>) =
        match rest with
        | [] -> ()
        | cell :: more ->
            let p =
                match cell with
                | None -> nullPtr ()
                | Some text -> zpg_cstr text
            unsafe Ptr.write (Ptr.add buf i) p
            fill (i + 1) more
    if n > 0 then fill 0 args
    let values = if n = 0 then nullPtr () else Ptr.toCPtr buf
    let rc = zpg_send_params conn sql (i32 n) values
    if n > 0 then unsafe Ptr.free buf
    // `args` stays live until here, so the text the pointers borrow is still there for the call.
    let _ = List.length args
    if rc = i32 1 then Ok () else Error (message conn)

let rec private flush (conn: cptr) : Task<result<unit, string>> = task {
    let n = C.PQflush conn
    if n = i32 0 then
        return Ok ()
    else if n = i32 1 then
        let fd = int (C.PQsocket conn)
        if fd < 0 then
            return Error (message conn)
        else
            do! Task.waitFor fd true
            return! flush conn
    else
        return Error (message conn)
}

let rec private untilIdle (conn: cptr) : Task<result<unit, string>> = task {
    if C.PQconsumeInput conn = i32 0 then
        return Error (message conn)
    else if C.PQisBusy conn = i32 0 then
        return Ok ()
    else
        let fd = int (C.PQsocket conn)
        if fd < 0 then
            return Error (message conn)
        else
            do! Task.waitFor fd false
            return! untilIdle conn
}

let private fieldNames (res: cptr) : array<string> =
    Array.init (int (C.PQnfields res)) (fun i -> copy (C.PQfname res (i32 i)))

let private oneRow (res: cptr) (names: array<string>) (row: int) : Row =
    let cells =
        Array.init (Array.length names) (fun i ->
            if C.PQgetisnull res (i32 row) (i32 i) <> i32 0 then
                None
            else
                Some (copy (C.PQgetvalue res (i32 row) (i32 i))))
    { names = names; cells = cells }

let private commandCount (res: cptr) : int =
    match String.parseInt (copy (C.PQcmdTuples res)) with
    | Some n -> n
    | None -> 0

// One result from the connection, cleared before returning. `None` when the command is done.
// `keep` is false when the connection can't be used again (COPY, or a status we don't drain).
// Rows are kept only while `reading` (the first result that has any); later results are drained.
let private takeOne (conn: cptr) (rows: list<Row>) (affected: int) (reading: bool) : option<list<Row> * int * option<string> * bool * bool> =
    let res = C.PQgetResult conn
    if isNull res then
        None
    else
        let status = C.PQresultStatus res
        let step =
            if status = C.PGRES_TUPLES_OK || status = C.PGRES_SINGLE_TUPLE then
                let n = int (C.PQntuples res)
                let acc =
                    if reading then
                        let names = fieldNames res
                        let mutable acc = rows
                        for i in 0 .. n - 1 do
                            acc <- oneRow res names i :: acc
                        acc
                    else
                        rows
                (acc, affected + n, None, true, false)
            else if status = C.PGRES_COMMAND_OK || status = C.PGRES_EMPTY_QUERY then
                (rows, affected + commandCount res, None, true, reading)
            else if status = C.PGRES_FATAL_ERROR || status = C.PGRES_NONFATAL_ERROR || status = C.PGRES_BAD_RESPONSE then
                (rows, affected, Some (describe conn res), true, reading)
            else
                (rows,
                    affected,
                    Some "the result is a status this client doesn't read (COPY isn't supported)",
                    false,
                    reading)
        C.PQclear res
        Some step

let rec private collect (conn: cptr) (rows: list<Row>) (affected: int) (reading: bool) : list<Row> * int * option<string> * bool =
    match takeOne conn rows affected reading with
    | None -> (rows, affected, None, true)
    | Some (rows2, affected2, failed, false, _) -> (rows2, affected2, failed, false)
    | Some (rows2, affected2, failed, true, reading2) ->
        let (rows3, affected3, later, keep) = collect conn rows2 affected2 reading2
        let err =
            match (failed, later) with
            | (Some e, _) -> Some e
            | (None, e) -> e
        (rows3, affected3, err, keep)

let private perform (conn: cptr) (sql: string) (args: list<option<string>>) : Task<result<Outcome, string> * bool> = task {
    match send conn sql args with
    | Error e -> return (Error e, healthy (idOf conn))
    | Ok () ->
        let! flushed = flush conn
        match flushed with
        | Error e -> return (Error e, false)
        | Ok () ->
            let! idle = untilIdle conn
            match idle with
            | Error e -> return (Error e, false)
            | Ok () ->
                let (rows, affected, failed, keep) = collect conn [] 0 true
                let ran =
                    match failed with
                    | Some e -> Error e
                    | None -> Ok { rows = List.rev rows; affected = affected }
                return (ran, keep && healthy (idOf conn))
}

let rec private poll (conn: cptr) : Task<result<unit, string>> = task {
    let status = C.PQconnectPoll conn
    if status = C.PGRES_POLLING_OK then
        if C.PQstatus conn = C.CONNECTION_OK then return Ok () else return Error (message conn)
    else if status = C.PGRES_POLLING_READING || status = C.PGRES_POLLING_WRITING then
        let fd = int (C.PQsocket conn)
        if fd < 0 then
            return Error (message conn)
        else
            do! Task.waitFor fd (status = C.PGRES_POLLING_WRITING)
            return! poll conn
    else if status = C.PGRES_POLLING_ACTIVE then
        return! poll conn
    else
        return Error (message conn)
}

// PQconnectStart is the part that can block, on a DNS lookup. It runs on the loop's helper
// thread. The poll that follows waits on the socket.
let private openConn (info: string) : Task<result<int, string>> = task {
    let! id = Task.blocking (fun () -> idOf (C.PQconnectStart info))
    if id = 0 then
        return Error "out of memory opening a connection"
    else
        let conn = ptrOf id
        if C.PQstatus conn = C.CONNECTION_BAD || C.PQsocket conn < i32 0 then
            let msg = message conn
            finishId id
            return Error msg
        else if C.PQsetnonblocking conn (i32 1) <> i32 0 then
            let msg = message conn
            finishId id
            return Error msg
        else
            let! polled = poll conn
            match polled with
            | Error e ->
                finishId id
                return Error e
            | Ok () ->
                if C.PQsetClientEncoding conn "UTF8" <> i32 0 then
                    let msg = message conn
                    finishId id
                    return Error msg
                else
                    return Ok id
}

// ---------------------------------------------------------------- pool

// What a queued acquire is waiting for. The kind changes under the pool lock, then a byte on
// the socket wakes the task. 0 queued, 1 a connection is in `conn`, 2 try again, 3 the pool
// closed, 4 this task has taken the result, 5 cancelled.
let private queued = 0
let private delivered = 1
let private retry = 2
let private shut = 3
let private accepted = 4
let private cancelled = 5

type private Waiter = {
    readFd: int
    writeFd: int
    kind: Atomic<int>
    conn: Atomic<int>
}

type private State = { idle: list<int>; waiters: list<Waiter> }

/// How many connections a pool opens. `maxSize` counts connections being opened too, so a pool
/// never has more than that many server connections.
type Config = { maxSize: int = 10 }

type Pool = {
    conninfo: string
    maxSize: int
    state: Mutex<State>
    live: Atomic<int>
    closed: Atomic<int>
}

/// A connection checked out of a pool, or from `connect`. `use` gives it back (a pool connection
/// that was mid-query, or in a transaction, is closed instead, and the server rolls the
/// transaction back).
type Conn = {
    pool: option<Pool>
    id: int
    dirty: Atomic<int>
    inTx: Atomic<int>
    busy: Atomic<int>
    released: Atomic<int>
}

let private lease (pool: option<Pool>) (id: int) : Conn = {
    pool = pool
    id = id
    dirty = Atomic.make 0
    inTx = Atomic.make 0
    busy = Atomic.make 0
    released = Atomic.make 0
}

let private wakePair () : result<int * int, string> =
    let r: ptr<i32> = unsafe Ptr.alloc 1
    let w: ptr<i32> = unsafe Ptr.alloc 1
    let rc = zpg_wake_pair r w
    let pair =
        if rc < i32 0 then
            Error (osError rc)
        else
            Ok (int (unsafe Ptr.read r), int (unsafe Ptr.read w))
    unsafe
        Ptr.free r
        Ptr.free w
    pair

// Hands `id` to the oldest queued waiter and wakes it. `kind` is `delivered` (and `id` is the
// connection) or `retry`. Already-cancelled waiters are skipped. Returns the waiters still queued.
let rec private hand (ws: list<Waiter>) (kind: int) (id: int) : list<Waiter> * bool =
    match ws with
    | [] -> ([], false)
    | w :: rest ->
        if Atomic.get w.kind = queued then
            if kind = delivered then Atomic.set w.conn id
            Atomic.set w.kind kind
            zpg_wake (i32 w.writeFd) |> ignore
            (rest, true)
        else
            let (tail, ok) = hand rest kind id
            (w :: tail, ok)

let private releaseId (pool: Pool) (id: int) =
    let drop = ref false
    Mutex.update pool.state (fun s ->
        if Atomic.get pool.closed <> 0 then
            drop.Value <- true
            s
        else
            let (rest, gave) = hand s.waiters delivered id
            if gave then { s with waiters = rest } else { idle = id :: s.idle; waiters = rest })
    |> ignore
    if drop.Value then
        finishId id
        Atomic.add pool.live (0 - 1) |> ignore

// A connection was closed. One waiter, if there is one, goes round acquire again: a slot is free.
let private nudge (pool: Pool) =
    Mutex.update pool.state (fun s ->
        if Atomic.get pool.closed <> 0 then
            s
        else
            let (rest, _) = hand s.waiters retry 0
            { s with waiters = rest })
    |> ignore

let private discard (pool: Pool) (id: int) =
    finishId id
    Atomic.add pool.live (0 - 1) |> ignore
    nudge pool

let private giveBack (pool: Pool) (id: int) (dirty: bool) =
    if dirty || not (healthy id) then discard pool id else releaseId pool id

/// `use` gives a pooled connection back, or closes a connection from `connect`.
impl Dispose<Conn> =
    let dispose (c: Conn) =
        if Atomic.exchange c.released 1 = 0 then
            match c.pool with
            | None -> finishId c.id
            | Some pool ->
                let dirty = Atomic.get c.dirty <> 0 || Atomic.get c.inTx <> 0
                giveBack pool c.id dirty

type private Guard = { pool: Pool; waiter: Waiter }

impl Dispose<Guard> =
    let dispose (g: Guard) =
        let stolen = ref 0
        let kind = Atomic.get g.waiter.kind
        if kind = queued || kind = delivered then
            Mutex.update g.pool.state (fun s ->
                let now = Atomic.get g.waiter.kind
                if now = queued || now = delivered then
                    if now = delivered then stolen.Value <- Atomic.get g.waiter.conn
                    Atomic.set g.waiter.kind cancelled
                    { s with waiters = List.filter (fun w -> w.readFd <> g.waiter.readFd) s.waiters }
                else
                    s)
            |> ignore
        if stolen.Value <> 0 then releaseId g.pool stolen.Value
        zpg_close (i32 g.waiter.readFd)
        zpg_close (i32 g.waiter.writeFd)

let private popIdle (pool: Pool) : option<int> =
    let taken = ref None
    Mutex.update pool.state (fun s ->
        if Atomic.get pool.closed <> 0 then
            s
        else
            match s.idle with
            | id :: rest ->
                taken.Value <- Some id
                { s with idle = rest }
            | [] -> s)
    |> ignore
    taken.Value

type private Slot =
    | Got of int
    | Wait
    | Shut

let private enqueue (pool: Pool) (waiter: Waiter) : Slot =
    let slot = ref Wait
    Mutex.update pool.state (fun s ->
        if Atomic.get pool.closed <> 0 then
            slot.Value <- Shut
            s
        else
            match s.idle with
            | id :: rest ->
                slot.Value <- Got id
                { s with idle = rest }
            | [] ->
                slot.Value <- Wait
                { s with waiters = List.append s.waiters [ waiter ] })
    |> ignore
    slot.Value

let rec private awaitKind (waiter: Waiter) : Task<int> = task {
    do! Task.waitFor waiter.readFd false
    zpg_drain (i32 waiter.readFd) |> ignore
    let kind = Atomic.get waiter.kind
    if kind = queued then return! awaitKind waiter else return kind
}

let private enqueueAndWait (pool: Pool) : Task<result<int, string>> = task {
    match wakePair () with
    | Error e -> return Error e
    | Ok (r, w) ->
        let waiter = { readFd = r; writeFd = w; kind = Atomic.make queued; conn = Atomic.make 0 }
        use guard = { pool = pool; waiter = waiter }
        match enqueue pool waiter with
        | Shut -> return Error "the pool is closed"
        | Got id ->
            Atomic.set waiter.kind accepted
            return Ok id
        | Wait ->
            let! kind = awaitKind waiter
            let id = Atomic.get waiter.conn
            Atomic.set waiter.kind accepted
            if kind = delivered then return Ok id
            else if kind = retry then return Ok 0
            else return Error "the pool is closed"
}

module Pool =
    /// A pool of at most `config.maxSize` connections to `conninfo` (a libpq connection string,
    /// `host=127.0.0.1 dbname=app` or a URL). Nothing connects until the first acquire.
    let create (conninfo: string) (config: Config) : result<Pool, string> =
        if config.maxSize < 1 then
            Error "maxSize must be at least 1"
        else
            let created = {
                conninfo = conninfo
                maxSize = config.maxSize
                state = Mutex.make { idle = []; waiters = [] }
                live = Atomic.make 0
                closed = Atomic.make 0
            }
            Ok created

    /// Closes idle connections and wakes tasks waiting for one. A connection already checked out
    /// is closed when it is given back.
    let close (pool: Pool) =
        let idle = ref ([] : list<int>)
        let waiting = ref ([] : list<Waiter>)
        Mutex.update pool.state (fun s ->
            Atomic.set pool.closed 1
            idle.Value <- s.idle
            waiting.Value <- s.waiters
            { idle = []; waiters = [] })
        |> ignore
        List.iter finishId idle.Value
        if List.length idle.Value > 0 then Atomic.add pool.live (0 - List.length idle.Value) |> ignore
        List.iter (fun (w: Waiter) ->
            if Atomic.get w.kind = queued then
                Atomic.set w.kind shut
                zpg_wake (i32 w.writeFd) |> ignore) waiting.Value

    /// A connection from the pool, waiting while `maxSize` of them are already checked out.
    let rec acquire (pool: Pool) : Task<result<Conn, string>> = task {
        if Atomic.get pool.closed <> 0 then
            return Error "the pool is closed"
        else
            match popIdle pool with
            | Some id -> return Ok (lease (Some pool) id)
            | None ->
                let n = Atomic.get pool.live
                if n < pool.maxSize && Atomic.compareExchange pool.live n (n + 1) = n then
                    if Atomic.get pool.closed <> 0 then
                        Atomic.add pool.live (0 - 1) |> ignore
                        return Error "the pool is closed"
                    else
                        let! opened = openConn pool.conninfo
                        match opened with
                        | Error e ->
                            Atomic.add pool.live (0 - 1) |> ignore
                            nudge pool
                            return Error e
                        | Ok id ->
                            if Atomic.get pool.closed <> 0 then
                                discard pool id
                                return Error "the pool is closed"
                            else
                                return Ok (lease (Some pool) id)
                else
                    let! hit = enqueueAndWait pool
                    match hit with
                    | Error e -> return Error e
                    | Ok 0 -> return! acquire pool
                    | Ok id -> return Ok (lease (Some pool) id)
    }

    /// `acquire`, then `body`, then the connection is given back, including when `body` fails or
    /// the task is cancelled.
    let withConnection (pool: Pool) (body: Conn -> Task<result<'a, string>>) : Task<result<'a, string>> = task {
        let! got = acquire pool
        match got with
        | Error e -> return Error e
        | Ok conn ->
            use c = conn
            return! body c
    }

/// `use` closes a pool: idle connections go, and a checked-out one goes when it is given back.
impl Dispose<Pool> =
    let dispose (pool: Pool) = Pool.close pool

// ---------------------------------------------------------------- queries

let rec private mapRows (rows: list<Row>) : result<list<'a>, string> =
    match rows with
    | [] -> Ok []
    | row :: rest ->
        match fromRow row with
        | Error e -> Error e
        | Ok v -> Result.map (fun vs -> v :: vs) (mapRows rest)

let private run (conn: Conn) (sql: string) (args: list<Value>) : Task<result<Outcome, string>> = task {
    if Atomic.compareExchange conn.busy 0 1 <> 0 then
        return Error "the connection is already in use"
    else
        // Set for the whole call, so cancelling while it waits closes the connection instead of
        // handing back one whose command is still running.
        Atomic.set conn.dirty 1
        let! (ran, keep) = perform (ptrOf conn.id) sql (List.map render args)
        if keep then Atomic.set conn.dirty 0
        Atomic.set conn.busy 0
        return ran
}

/// Runs `sql` and reads every row of the first result as `'a` (`[<Derive(FromRow)>]` on a record,
/// or `fromRow` of your own). `$1`, `$2`, … are `args`. A command that returns no rows gives an
/// empty list.
let query (conn: Conn) (sql: string) (args: list<Value>) : Task<result<list<'a>, string>> = task {
    let! ran = run conn sql args
    match ran with
    | Error e -> return Error e
    | Ok outcome -> return mapRows outcome.rows
}

/// One row, or an error when the result has none or more than one.
let queryOne (conn: Conn) (sql: string) (args: list<Value>) : Task<result<'a, string>> = task {
    let! rows = query conn sql args
    match rows with
    | Error e -> return Error e
    | Ok [ x ] -> return Ok x
    | Ok [] -> return Error "the query returned no rows"
    | Ok _ -> return Error "the query returned more than one row"
}

/// Runs `sql` and gives how many rows it affected (the row count of a `SELECT`).
let exec (conn: Conn) (sql: string) (args: list<Value>) : Task<result<int, string>> = task {
    let! ran = run conn sql args
    match ran with
    | Error e -> return Error e
    | Ok outcome -> return Ok outcome.affected
}

/// A connection of its own, not from a pool. `use` closes it.
let connect (conninfo: string) : Task<result<Conn, string>> = task {
    let! opened = openConn conninfo
    match opened with
    | Error e -> return Error e
    | Ok id -> return Ok (lease None id)
}

/// `BEGIN`. Giving the connection back before `commit` or `rollback` closes it, and the server
/// rolls the transaction back.
let begin (conn: Conn) : Task<result<unit, string>> = task {
    let! ran = exec conn "BEGIN" []
    match ran with
    | Error e -> return Error e
    | Ok _ ->
        Atomic.set conn.inTx 1
        return Ok ()
}

/// `COMMIT`.
let commit (conn: Conn) : Task<result<unit, string>> = task {
    let! ran = exec conn "COMMIT" []
    Atomic.set conn.inTx 0
    match ran with
    | Ok _ -> return Ok ()
    | Error e ->
        Atomic.set conn.dirty 1
        return Error e
}

/// `ROLLBACK`.
let rollback (conn: Conn) : Task<result<unit, string>> = task {
    let! ran = exec conn "ROLLBACK" []
    Atomic.set conn.inTx 0
    match ran with
    | Ok _ -> return Ok ()
    | Error e ->
        Atomic.set conn.dirty 1
        return Error e
}

/// `begin`, then `body`. `Ok` commits; `Error`, or cancelling the task, rolls back (a cancel
/// closes the connection, which rolls the server's transaction back).
let transaction (conn: Conn) (body: unit -> Task<result<'a, string>>) : Task<result<'a, string>> = task {
    let! began = begin conn
    match began with
    | Error e -> return Error e
    | Ok () ->
        let! ran = body ()
        match ran with
        | Error e ->
            let! rolled = rollback conn
            match rolled with
            | Ok () -> return Error e
            | Error re -> return Error $"{e} (rollback failed: {re})"
        | Ok v ->
            let! committed = commit conn
            match committed with
            | Ok () -> return Ok v
            | Error e -> return Error e
}
