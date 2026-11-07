# zpg

A Postgres client for Z#: libpq, with a connection pool, parameterized queries and transactions.
Queries wait on the connection's socket, so a task that's waiting doesn't stall the event loop.

## Usage

```fsharp
open Zpg

[<Derive(FromRow)>]
type User = { id: int; name: string; note: option<string> }

let load (conn: Conn) : Task<result<list<User>, string>> =
    query conn "select id, name, note from users where id = $1" [ Int 7 ]

let main () =
    match Pool.create "host=127.0.0.1 dbname=app" { maxSize = 10 } with
    | Error e ->
        eprintln $"zpg: {e}"
        1
    | Ok pool ->
        match Task.run (Pool.withConnection pool load) with
        | Ok users ->
            for u in users do println $"{u.id} {u.name}"
            Pool.close pool
            0
        | Error e ->
            eprintln $"zpg: {e}"
            Pool.close pool
            1
```

`Pool.withConnection` checks a connection out and gives it back when the body finishes, fails, or
the task is cancelled. `use conn = …` does the same for a connection you acquired yourself.

## Queries

| | |
|---|---|
| `query conn sql args` | every row of the result, as `'a` |
| `queryOne conn sql args` | the one row, or an error when there isn't one |
| `exec conn sql args` | how many rows the command affected |
| `connect conninfo` | one connection, not from a pool; `use` closes it |

`args` is a `list<Value>`: `Null`, `Text`, `Int`, `Bool` and `Float`, bound to `$1`, `$2`, …
A record with `[<Derive(FromRow)>]` reads each field from the column of the same name. A missing
column takes the field's default, or `None` for an `option`. NULL is `None` for an `option` and
an error for anything else. `Row.string`, `Row.int`, `Row.int64`, `Row.bool` and `Row.float` read
one column when you don't want a record. `impl FromCell<T>` teaches a field type of your own.

An error is Postgres's message with its SQLSTATE in front (`42P01 relation "users" does not exist`).

## Pool

`Pool.create conninfo { maxSize = 10 }`. `conninfo` is a libpq connection string or URL. Connections
are opened on demand, and never more than `maxSize`, counting one that's still connecting. The
pool is an ordinary value: share it with every task and every thread.

`Pool.acquire` waits while `maxSize` connections are already checked out. `Pool.close` (or `use`
on the pool) closes the idle ones; a connection that's still out is closed when it's given back.

## Transactions

`transaction conn (fun () -> …)` runs `BEGIN`, then the body. `Ok` commits. `Error` rolls back.
`begin`, `commit` and `rollback` are the three statements on their own.

Giving a connection back while a transaction is still open closes it, and the server rolls the
transaction back. The same happens when the task is cancelled.

## How a query waits

libpq is linked in (`import c` of `libpq-fe.h`, plus `c/zpg.c` for the parts a Z# value can't
hold). A connection is non-blocking (`PQsetnonblocking`). Sending a command and reading the result
are `PQsendQueryParams`, `PQflush`, `PQconsumeInput` and `PQisBusy`, and where libpq would block
the task does `Task.waitFor` on `PQsocket` instead. Other tasks on that loop keep running, and a
pool can have a query in flight on every connection at once.

`PQconnectStart` is the one call that can block, on a DNS lookup, so it runs on `Task.blocking`'s
helper thread. The poll after that is on the socket, like a query. A connection string with
`hostaddr` and no `host` doesn't look a name up.

The pool has to hand a connection from whatever thread released it to whatever thread is waiting.
A `PGconn *` isn't `Share`, so the pool stores the pointer as an `int` and turns it back at the
call. A waiter blocked in `Task.waitFor` is woken by one byte on a socket pair.

## Limitations

- Text format only. A field is a `string`, `bool`, `int`, `i32`, `i64`, `f64` or an `option` of
  one of those. A timestamp, a json value, a bytea or an array comes back as text if the field is
  a `string`.
- One statement per call. Postgres rejects several commands in one parameterized query
  (`42601 cannot insert multiple commands into a prepared statement`). Whatever comes back is
  drained, so the connection can be used again; a later result's rows are not returned.
- `COPY` isn't supported. A result that leaves the connection in copy state is thrown away with
  the connection.
- One task uses a connection at a time. A second call returns `the connection is already in use`.
- Cancelling a task while a command is in flight closes that connection rather than reusing it.
  An open transaction is rolled back by the server when the connection closes.
- Postgres folds an unquoted name to lowercase, so a field `userName` doesn't match a column
  `username`. Alias it: `select user_name as userName`.
- Each event loop has one helper thread for `Task.blocking`. Connects on one loop run there one
  at a time. Queries don't.
- libpq is a dynamic library. The program needs `libpq` at build time (`pkg-config`) and at run
  time. Linux and macOS. Not Windows.
- No `LISTEN`/`NOTIFY`, no prepared-statement cache, no pipelining.

## Example

```sh
cd examples/basic
zs run
```

The example expects Postgres at `ZPG_URL`, or `host=127.0.0.1 port=5432 dbname=zpg_test user=zpg password=zpg`.

## Tests

`zs test` talks to that same database. Create it once:

```sh
sudo -u postgres psql -c "CREATE ROLE zpg LOGIN PASSWORD 'zpg' SUPERUSER"
sudo -u postgres createdb -O zpg zpg_test
```

```sh
zs test
ZPG_URL=postgres://zpg:zpg@127.0.0.1/zpg_test zs test
```
