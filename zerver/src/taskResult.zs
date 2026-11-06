let ofTask (t: Task<'a>) : Task<result<'a, 'e>> = Task.map Ok t

let ofResult (r: result<'a, 'e>) : Task<result<'a, 'e>> = Task.ret r

let mapError (f: 'e -> 'f) (t: Task<result<'a, 'e>>) : Task<result<'a, 'f>> = Task.map (Result.mapError f) t

module Builder =
    let bind (t: Task<result<'a, 'e>>) (f: 'a -> Task<result<'b, 'e>>) : Task<result<'b, 'e>> =
        Task.bind t (fun r ->
            match r with
            | Ok x -> f x
            | Error e -> Task.ret (Error e))

    let ret (x: 'a) : Task<result<'a, 'e>> = Task.ret (Ok x)

    let zero () : Task<result<unit, 'e>> = Task.ret (Ok ())

    let delay (f: unit -> Task<result<'a, 'e>>) : Task<result<'a, 'e>> = Task.bind (Task.ret ()) f
