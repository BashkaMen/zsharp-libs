open Testing
open TaskResult

let parse (s: string) : Task<result<int, string>> =
    match String.parseInt s with
    | Some n -> Task.ret (Ok n)
    | None -> Task.ret (Error $"not a number: {s}")

let ratio (a: string) (b: string) : Task<result<int, string>> = taskResult {
    let! x = parse a
    let! y = parse b
    match y with
    | 0 -> return! error "division by zero"
    | _ -> return x / y
}

[<Tests>]
let tests = testList "taskResult" [
    test "returns the value" (fun () -> assert (Task.run (ratio "84" "2") = Ok 42))
    test "stops at the first error" (fun () -> assert (Task.run (ratio "x" "2") = Error "not a number: x"))
    test "returns an error from return!" (fun () -> assert (Task.run (ratio "1" "0") = Error "division by zero"))
    test "maps the error" (fun () -> assert (Task.run (ratio "x" "1" |> mapError String.length) = Error 15))
    test "lifts a task" (fun () -> assert (Task.run (ofTask (Task.ret 1) |> map (fun x -> x + 1)) = (Ok 2 : result<int, string>)))
]
