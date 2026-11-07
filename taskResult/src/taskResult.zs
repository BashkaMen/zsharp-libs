module Builder =
    let bind (m: Task<result<'a, 'e>>) (f: 'a -> Task<result<'b, 'e>>) : Task<result<'b, 'e>> = task {
        let! r = m
        match r with
        | Ok x -> return! f x
        | Error e -> return Error e
    }

    let ret (x: 'a) : Task<result<'a, 'e>> = Task.ret (Ok x)

    let zero () : Task<result<unit, 'e>> = Task.ret (Ok ())

    let delay (f: unit -> Task<result<'a, 'e>>) : Task<result<'a, 'e>> = task { return! f () }

let error (e: 'e) : Task<result<'a, 'e>> = Task.ret (Error e)

let ofResult (r: result<'a, 'e>) : Task<result<'a, 'e>> = Task.ret r

let ofTask (t: Task<'a>) : Task<result<'a, 'e>> = task {
    let! x = t
    return Ok x
}

let map (f: 'a -> 'b) (t: Task<result<'a, 'e>>) : Task<result<'b, 'e>> = task {
    let! r = t
    return Result.map f r
}

let mapError (f: 'e -> 'f) (t: Task<result<'a, 'e>>) : Task<result<'a, 'f>> = task {
    let! r = t
    return Result.mapError f r
}
