# taskResult

A `taskResult { }` builder for `Task<result<'a, 'e>>`: `let!` waits for the task and stops at the
first `Error`, `return x` is `Ok x`.

```fsharp
open TaskResult

let ratio (a: string) (b: string) : Task<result<int, string>> = taskResult {
    let! x = parse a
    let! y = parse b
    match y with
    | 0 -> return! error "division by zero"
    | _ -> return x / y
}
```

| | |
|---|---|
| `error e` | a task of `Error e`, for `return!` |
| `ofResult r` | a `result` as a task, for `let!` |
| `ofTask t` | a task's value as `Ok` |
| `map f t`, `mapError f t` | the value or the error, mapped |

`let!` takes only `Task<result<_, _>>`: wrap a plain `result` with `ofResult` and a plain task with
`ofTask`. `use` and `for` aren't supported inside the builder.

Without the builder, `?` gives the same early exit inside `task { }`: `let! r = t` then `let x = r?`.

```sh
zs test
```
