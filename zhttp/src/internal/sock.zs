// Non-blocking socket reads and writes. A call that would block waits on the event loop
// (`Task.waitFor`), the same way `Tcp` does, and gives up when `until` (a `Runtime.ticks`
// deadline) has passed.

let private wouldBlock = -1000

let wait (fd: int) (write: bool) (until: option<u64>) : Task<result<unit, string>> = task {
    match until with
    | None ->
        do! Task.waitFor fd write
        return Ok ()
    | Some until ->
        let now = Runtime.ticks ()
        if now >= until then
            return Error "timeout"
        else
            let ms = int ((until - now) / u64 1000000)
            let ms = if ms < 1 then 1 else ms
            let readable = Task.map (fun () -> true) (Task.waitFor fd write)
            let timedOut = Task.map (fun () -> false) (Task.sleep ms)
            let! ready = Task.race readable timedOut
            if ready then return Ok () else return Error "timeout"
}

let rec read (fd: int) (buf: Buffer<u8>) (n: int) (until: option<u64>) : Task<result<int, string>> = task {
    let got = Native.fdRead fd buf 0 n
    if got = wouldBlock then
        let! ready = wait fd false until
        match ready with
        | Error e -> return Error e
        | Ok () -> return! read fd buf n until
    else if got < 0 then
        return Error (Native.errorText got)
    else
        return Ok got
}

let rec write (fd: int) (buf: Buffer<u8>) (n: int) (until: option<u64>) : Task<result<unit, string>> =
    let rec from (off: int) = task {
        if off >= n then
            return Ok ()
        else
            let w = Native.fdWrite fd buf off (n - off)
            if w = wouldBlock then
                let! ready = wait fd true until
                match ready with
                | Error e -> return Error e
                | Ok () -> return! from off
            else if w < 0 then
                return Error (Native.errorText w)
            else
                return! from (off + w)
    }
    from 0

extern zs_fd_idle : i32 -> i32

/// Whether a keep-alive socket is still open and has not already received bytes.
let idle (fd: int) : bool = zs_fd_idle (i32 fd) = i32 1
