// A Postgres client over libpq. Queries are non-blocking: the connection's socket is waited on
// with Task.waitFor, so other tasks on the loop keep running. The pool is safe to share across
// threads (a server's loop per core); a single connection is used by one task at a time.
open Internal
open Libpq
open Zjson

/// A calendar date, a `date` column: no time and no zone.
type Date = { year: int; month: int; day: int }

/// A time of day, a `time` column (`24:00:00` included): no date and no zone.
type TimeOfDay = { hour: int; minute: int; second: int; nanos: int }

/// A `json` or `jsonb` column read with zjson's `Deserialize`, or written with `Value.json`.
type Jsonb<'a> = { value: 'a }

/// An `interval` column of a fixed length (no years or months; a day is 24 hours).
/// `Value.Interval` writes one. A wrapper because a `Duration` is an `i64` to traits.
type Interval = { duration: Duration }

/// A `bytea` column's bytes. `Value.Bytes` writes one.
type Bytea = { bytes: array<u8> }

/// A value bound to `$1`, `$2`, … Each is sent as the text Postgres reads for its type, with the
/// type left for the server to infer from where the parameter is used (`$1::jsonb` to be explicit).
type Value =
    | Null
    | Text of string
    | Int of int
    | Bool of bool
    | Float of f64
    /// A moment: `timestamptz` stores it, `timestamp` stores its UTC wall-clock time.
    | Timestamp of DateTime
    | Date of Date
    | Time of TimeOfDay
    /// An `interval` of a fixed length. Postgres keeps microseconds.
    | Interval of Duration
    /// JSON text for a `json` or `jsonb` column, sent as is. `Value.json` writes a value with zjson.
    | Json of string
    | Bytes of array<u8>
    /// A one-dimensional array (`= any($1)`); an `Array` inside it is a dimension more.
    | Array of list<Value>

/// One result row. Cells are text, or `None` for NULL; `Row.get` finds a column by its result name.
type Row = {
    names: array<string>
    cells: array<option<string>>
}

// `status` is the last command's tag (`INSERT 0 1`, `COMMIT`, or `ROLLBACK` for a COMMIT of a
// failed transaction).
type private Outcome = { rows: list<Row>; affected: int; status: string }

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

/// `timestamptz`, `timestamp` (taken as UTC) or `date` (midnight UTC).
impl FromCell<DateTime> =
    let fromCell (cell: option<string>) : result<DateTime, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            match PgText.parseTimestamp s with
            | Some t -> Ok t
            | None -> Error $"not a timestamp DateTime holds: {s}"

impl FromCell<Date> =
    let fromCell (cell: option<string>) : result<Date, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            match PgText.parseDate s with
            | Some (y, m, d) -> Ok { year = y; month = m; day = d }
            | None -> Error $"not a date: {s}"

impl FromCell<TimeOfDay> =
    let fromCell (cell: option<string>) : result<TimeOfDay, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            match PgText.parseTime s with
            | Some (h, m, sec, ns) -> Ok { hour = h; minute = m; second = sec; nanos = ns }
            | None -> Error $"not a time: {s}"

/// An `interval` without years or months; a day is 24 hours.
impl FromCell<Interval> =
    let fromCell (cell: option<string>) : result<Interval, string> =
        match cell with
        | None -> Error "null"
        | Some s -> Result.map (fun (n: i64) -> { duration = n * 1<ns> }) (PgText.parseInterval s)

/// `bytea`.
impl FromCell<Bytea> =
    let fromCell (cell: option<string>) : result<Bytea, string> =
        match cell with
        | None -> Error "null"
        | Some s -> Result.map (fun (b: array<u8>) -> { bytes = b }) (PgText.parseBytes s)

/// A one-dimensional array, each element read as `'a` (`option<'a>` when it may be NULL). A
/// `list<list<'a>>` reads a two-dimensional one.
impl FromCell<list<'a>> =
    let fromCell (cell: option<string>) : result<list<'a>, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            let items = (PgText.parseArray s)?
            let rec each (rest: list<option<string>>) (i: int) (acc: list<'a>) : result<list<'a>, string> =
                match rest with
                | [] -> Ok (List.rev acc)
                | item :: more ->
                    match fromCell item with
                    | Ok v -> each more (i + 1) (v :: acc)
                    | Error e -> Error $"[{i}]: {e}"
            each items 0 []

/// A one-dimensional array, as `list<'a>` reads it.
impl FromCell<array<'a>> =
    let fromCell (cell: option<string>) : result<array<'a>, string> =
        let items: result<list<'a>, string> = fromCell cell
        Result.map Array.ofList items

/// `json` or `jsonb`, read with zjson.
impl FromCell<Jsonb<'a>> =
    let fromCell (cell: option<string>) : result<Jsonb<'a>, string> =
        match cell with
        | None -> Error "null"
        | Some s ->
            match Zjson.fromString s with
            | Ok v -> Ok { value = v }
            | Error e -> Error $"json: {e}"

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

    /// The column as any type with a `FromCell` (`DateTime`, `option<Date>`, `list<int>`, …).
    let read (row: Row) (name: string) : result<'a, string> =
        match tryGet row name with
        | Some cell -> Result.mapError (fun (e: string) -> $"column {name}: {e}") (fromCell cell)
        | None -> Error $"no column {name}"

    /// The column as a `bool` (`t`/`f`, as Postgres writes a boolean).
    let bool (row: Row) (name: string) : result<bool, string> =
        let s = (text row name)?
        let t = String.toLower s
        if t = "t" || t = "true" || t = "1" then Ok true
        else if t = "f" || t = "false" || t = "0" then Ok false
        else Error $"column {name} is not a bool: {s}"

/// A record read from a row: each field from the column of the same name. A missing column takes
/// the field's default, or `None` for an `option`, and is an error otherwise. NULL is `None` for
/// an `option` and an error otherwise.
trait FromRow<'a> =
    abstract fromRow : Row -> result<'a, string>

/// The row itself: `query conn sql args : Task<result<list<Row>, _>>`, then `Row.read`.
impl FromRow<Row> =
    let fromRow (row: Row) : result<Row, string> = Ok row

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

module Value =
    /// `x` as JSON, written with zjson's `Serialize`, for a `json` or `jsonb` parameter.
    let json (x: 'a) : Value = Json (Zjson.toString (JsonWriter.create 256) x)

let rec private render (v: Value) : option<string> =
    match v with
    | Null -> None
    | Text s -> Some s
    | Int n -> Some (string n)
    | Bool b -> Some (if b then "true" else "false")
    | Float x -> Some (formatFloat x)
    | Timestamp t -> Some (PgText.formatTimestamp t)
    | Date d -> Some (PgText.formatDate d.year d.month d.day)
    | Time t -> Some (PgText.formatTime t.hour t.minute t.second t.nanos)
    | Interval d -> Some (PgText.formatInterval (d / 1<ns>))
    | Json s -> Some s
    | Bytes b -> Some (PgText.formatBytes b)
    | Array items ->
        let element (item: Value) : option<string * bool> =
            match item with
            | Array _ -> Option.map (fun (text: string) -> (text, true)) (render item)
            | _ -> Option.map (fun (text: string) -> (text, false)) (render item)
        Some (PgText.formatArray (List.map element items))

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

// Fit for the next borrower: connected, and not inside a transaction (one opened with a plain
// `exec conn "BEGIN"`, or one an error has aborted).
let private reusable (id: int) : bool =
    healthy id && C.PQtransactionStatus (ptrOf id) = C.PQTRANS_IDLE

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

// Every result of the command, each cleared. Rows are kept from the first result that has any;
// later results are drained. The error is the first one. `keep` is false when the connection
// can't be used again (COPY, or a status this client doesn't drain).
let private collect (conn: cptr) : Outcome * option<string> * bool =
    let mutable rows = ([] : list<Row>)
    let mutable affected = 0
    let mutable status = ""
    let mutable failed = (None : option<string>)
    let mutable keep = true
    let mutable reading = true
    let mutable more = true
    while more do
        let res = C.PQgetResult conn
        if isNull res then
            more <- false
        else
            let st = C.PQresultStatus res
            if st = C.PGRES_TUPLES_OK || st = C.PGRES_SINGLE_TUPLE then
                let n = int (C.PQntuples res)
                if reading then
                    let names = fieldNames res
                    for i in 0 .. n - 1 do
                        rows <- oneRow res names i :: rows
                    reading <- false
                affected <- affected + n
                status <- copy (C.PQcmdStatus res)
            else if st = C.PGRES_COMMAND_OK || st = C.PGRES_EMPTY_QUERY then
                affected <- affected + commandCount res
                status <- copy (C.PQcmdStatus res)
            else if st = C.PGRES_FATAL_ERROR || st = C.PGRES_NONFATAL_ERROR || st = C.PGRES_BAD_RESPONSE then
                if Option.isNone failed then failed <- Some (describe conn res)
            else
                failed <- Some "the result is a status this client doesn't read (COPY isn't supported)"
                keep <- false
                more <- false
            C.PQclear res
    ({ rows = List.rev rows; affected = affected; status = status }, failed, keep)

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
                let (outcome, failed, keep) = collect conn
                let ran =
                    match failed with
                    | Some e -> Error e
                    | None -> Ok outcome
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

// A connection being opened. `cell` holds its id while this task owns it: 0 before
// PQconnectStart has returned, -1 once disposed. Disposing it (an error, or a cancel) closes the
// connection; the helper thread closes it itself when the task has already gone.
type private Opening = { cell: Atomic<int> }

impl Dispose<Opening> =
    let dispose (o: Opening) =
        let id = Atomic.exchange o.cell (0 - 1)
        if id > 0 then finishId id

// A setting the text format here relies on, set when the server's isn't it.
let private ensure (conn: cptr) (name: string) (want: string) (sql: string) : Task<result<unit, string>> = task {
    let current = copy (C.PQparameterStatus conn name)
    if current = want || String.startsWith (want + ",") current then
        return Ok ()
    else
        let! (ran, _) = perform conn sql []
        match ran with
        | Ok _ -> return Ok ()
        | Error e -> return Error e
}

// PQconnectStart is the part that can block, on a DNS lookup. It runs on the loop's helper
// thread. The poll that follows waits on the socket.
let private openConn (info: string) : Task<result<int, string>> = task {
    use opening = { cell = Atomic.make 0 }
    let cell = opening.cell
    let! id =
        Task.blocking (fun () ->
            let id = idOf (C.PQconnectStart info)
            if id <> 0 && Atomic.compareExchange cell 0 id <> 0 then
                // the task was cancelled while this ran: nobody else will close it
                finishId id
            id)
    if id = 0 then
        return Error "out of memory opening a connection"
    else
        let conn = ptrOf id
        if C.PQstatus conn = C.CONNECTION_BAD || C.PQsocket conn < i32 0 then
            return Error (message conn)
        else if C.PQsetnonblocking conn (i32 1) <> i32 0 then
            return Error (message conn)
        else
            let! polled = poll conn
            match polled with
            | Error e -> return Error e
            | Ok () ->
                if C.PQsetClientEncoding conn "UTF8" <> i32 0 then
                    return Error (message conn)
                else
                    let! dates = ensure conn "DateStyle" "ISO" "SET DateStyle TO ISO"
                    match dates with
                    | Error e -> return Error e
                    | Ok () ->
                        let! intervals = ensure conn "IntervalStyle" "postgres" "SET IntervalStyle TO postgres"
                        match intervals with
                        | Error e -> return Error e
                        | Ok () ->
                            // ours now: the dispose leaves it open
                            Atomic.set cell 0
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
    busy: Atomic<int>
    released: Atomic<int>
}

let private lease (pool: option<Pool>) (id: int) : Conn = {
    pool = pool
    id = id
    dirty = Atomic.make 0
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
    if dirty || not (reusable id) then discard pool id else releaseId pool id

/// `use` gives a pooled connection back, or closes a connection from `connect`.
impl Dispose<Conn> =
    let dispose (c: Conn) =
        if Atomic.exchange c.released 1 = 0 then
            match c.pool with
            | None -> finishId c.id
            | Some pool ->
                giveBack pool c.id (Atomic.get c.dirty <> 0)

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
        // woken to retry, then cancelled before it could: the free slot goes to the next waiter
        if kind = retry then nudge g.pool
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
    | Again
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
                // A connection closed after acquire saw the pool full. Its nudge took the lock
                // before this one and found no waiter, so look again rather than wait for it.
                if Atomic.get pool.live < pool.maxSize then
                    slot.Value <- Again
                    s
                else
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
        | Again ->
            Atomic.set waiter.kind accepted
            return Ok 0
        | Wait ->
            let! kind = awaitKind waiter
            let id = Atomic.get waiter.conn
            Atomic.set waiter.kind accepted
            if kind = delivered then return Ok id
            else if kind = retry then return Ok 0
            else return Error "the pool is closed"
}

// A slot counted in `live` for a connection still being opened. Disposed before the connection
// is kept (`kept`), it frees the slot and lets a waiter use it.
type private Reserved = { pool: Pool; kept: Atomic<int> }

impl Dispose<Reserved> =
    let dispose (r: Reserved) =
        if Atomic.get r.kept = 0 then
            Atomic.add r.pool.live (0 - 1) |> ignore
            nudge r.pool

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
                if n >= pool.maxSize then
                    let! hit = enqueueAndWait pool
                    match hit with
                    | Error e -> return Error e
                    | Ok 0 -> return! acquire pool
                    | Ok id -> return Ok (lease (Some pool) id)
                else if Atomic.compareExchange pool.live n (n + 1) <> n then
                    // another task took or freed a slot meanwhile: look again
                    return! acquire pool
                else
                    // the slot is this task's until the connection is leased; an error or a
                    // cancel while connecting gives it back
                    use slot = { pool = pool; kept = Atomic.make 0 }
                    if Atomic.get pool.closed <> 0 then
                        return Error "the pool is closed"
                    else
                        let! opened = openConn pool.conninfo
                        match opened with
                        | Error e -> return Error e
                        | Ok id ->
                            Atomic.set slot.kept 1
                            if Atomic.get pool.closed <> 0 then
                                discard pool id
                                return Error "the pool is closed"
                            else
                                return Ok (lease (Some pool) id)
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
        let cells = List.map render args
        // libpq takes C strings, so a NUL would end the value early without a word
        let nul = String.ofByte (u8 0)
        let hasNul (cell: option<string>) =
            match cell with
            | Some text -> String.contains nul text
            | None -> false
        if List.exists hasNul cells then
            Atomic.set conn.busy 0
            return Error "a parameter holds a NUL byte, which Postgres text can't"
        else
            // Set for the whole call, so cancelling while it waits closes the connection instead
            // of handing back one whose command is still running.
            Atomic.set conn.dirty 1
            let! (ran, keep) = perform (ptrOf conn.id) sql cells
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
    let! ran = run conn "BEGIN" []
    match ran with
    | Error e -> return Error e
    | Ok _ -> return Ok ()
}

/// `COMMIT`. An error when the server rolled back instead: a statement in the transaction had
/// failed, so none of it was kept.
let commit (conn: Conn) : Task<result<unit, string>> = task {
    let! ran = run conn "COMMIT" []
    match ran with
    | Error e -> return Error e
    | Ok outcome ->
        if outcome.status = "ROLLBACK" then
            return Error "the transaction was rolled back: a statement in it had failed"
        else
            return Ok ()
}

/// `ROLLBACK`.
let rollback (conn: Conn) : Task<result<unit, string>> = task {
    let! ran = run conn "ROLLBACK" []
    match ran with
    | Error e -> return Error e
    | Ok _ -> return Ok ()
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
