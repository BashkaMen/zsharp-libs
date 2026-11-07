open Types

let create (capacity: int) : JsonWriter =
    { buf = ref (Buffer.create (max capacity 64) (u8 0)); len = ref 0; needComma = ref false }

let reset (w: JsonWriter) =
    w.len.Value <- 0
    w.needComma.Value <- false

let written (w: JsonWriter) : span<u8> = Buffer.slice w.buf.Value 0 w.len.Value

let private reserve (w: JsonWriter) (n: int) : int =
    let at = w.len.Value
    let room = Buffer.length w.buf.Value
    if at + n > room then
        let grown = Buffer.create (max (2 * room) (at + n)) (u8 0)
        Buffer.blit w.buf.Value 0 grown 0 at
        w.buf.Value <- grown
    at

let private put (w: JsonWriter) (b: u8) =
    let at = reserve w 1
    w.buf.Value.[at] <- b
    w.len.Value <- at + 1

let private append (w: JsonWriter) (bytes: span<u8>) =
    let n = Span.length bytes
    let at = reserve w n
    Buffer.blit bytes 0 w.buf.Value at n
    w.len.Value <- at + n

let private sep (w: JsonWriter) =
    if w.needComma.Value then
        put w (u8 44) // ,

let private afterValue (w: JsonWriter) = w.needComma.Value <- true

let private hex = "0123456789abcdef"

let private quoted (w: JsonWriter) (s: span<u8>) =
    let n = Span.length s
    let start = reserve w (6 * n + 2)
    let b = w.buf.Value
    let mutable at = start
    b.[at] <- u8 34 // "
    at <- at + 1
    let mutable from = 0
    for i in 0 .. n - 1 do
        let c = s.[i]
        if c < u8 32 || c = u8 34 || c = u8 92 then // control chars, " and \
            let run = i - from
            Buffer.blit s from b at run
            at <- at + run
            b.[at] <- u8 92 // \
            let simple =
                match int c with
                | 34 | 92 -> c // " and \ stay as they are
                | 10 -> u8 110 // \n -> n
                | 13 -> u8 114 // \r -> r
                | 9 -> u8 116 // \t -> t
                | 8 -> u8 98 // \b -> b
                | 12 -> u8 102 // \f -> f
                | _ -> u8 0
            if simple <> u8 0 then
                b.[at + 1] <- simple
                at <- at + 2
            else
                b.[at + 1] <- u8 117 // u
                b.[at + 2] <- u8 48 // 0
                b.[at + 3] <- u8 48 // 0
                b.[at + 4] <- String.byte hex (int (c >>> u8 4))
                b.[at + 5] <- String.byte hex (int (c &&& u8 15))
                at <- at + 6
            from <- i + 1
    let run = n - from
    Buffer.blit s from b at run
    at <- at + run
    b.[at] <- u8 34 // "
    w.len.Value <- at + 1

let private decimal (w: JsonWriter) (n: i64) =
    let neg = n < i64 0
    let x = if neg then n else -n
    let mutable digits = 1
    let mutable p = x
    while p <= i64 -10 do
        p <- p / i64 10
        digits <- digits + 1
    let total = if neg then digits + 1 else digits
    let at = reserve w total
    let b = w.buf.Value
    if neg then
        b.[at] <- u8 45 // -
    let mutable k = at + total - 1
    let mutable q = x
    for _ in 1 .. digits do
        b.[k] <- u8 (i64 48 - q % i64 10) // '0' + digit
        q <- q / i64 10
        k <- k - 1
    w.len.Value <- at + total

let beginObject (w: JsonWriter) =
    sep w
    put w (u8 123) // {
    w.needComma.Value <- false

let endObject (w: JsonWriter) =
    put w (u8 125) // }
    afterValue w

let beginArray (w: JsonWriter) =
    sep w
    put w (u8 91) // [
    w.needComma.Value <- false

let endArray (w: JsonWriter) =
    put w (u8 93) // ]
    afterValue w

let key (w: JsonWriter) (name: string) =
    sep w
    quoted w (String.bytes name)
    put w (u8 58) // :
    w.needComma.Value <- false

let quotedKey (w: JsonWriter) (encoded: span<u8>) =
    sep w
    append w encoded
    put w (u8 58) // :
    w.needComma.Value <- false

let str (w: JsonWriter) (s: string) =
    sep w
    quoted w (String.bytes s)
    afterValue w

let integer (w: JsonWriter) (n: i64) =
    sep w
    decimal w n
    afterValue w

let private scaledDigits (a: f64) : int =
    let mutable k = 0
    let mutable p = 1.0
    let mutable found = -1
    while found < 0 && k <= 15 && a * p < 1.0e15 do
        if Math.round (a * p) / p = a then
            found <- k
        else
            k <- k + 1
            p <- p * 10.0
    found

let private fixedPoint (w: JsonWriter) (x: f64) : bool =
    let a = if x < 0.0 then -x else x
    let k = if a >= 0.0001 && a < 1.0e15 then scaledDigits a else -1
    if k < 0 then
        false
    else
        let mutable p = i64 1
        for _ in 1 .. k do
            p <- p * i64 10
        let m = i64 (Math.round (a * f64 p))
        if x < 0.0 then
            put w (u8 45) // -
        decimal w (m / p)
        if k > 0 then
            let at = reserve w (k + 1)
            let b = w.buf.Value
            b.[at] <- u8 46 // .
            let mutable q = m % p
            for i in 0 .. k - 1 do
                b.[at + k - i] <- u8 (i64 48 + q % i64 10) // '0' + digit
                q <- q / i64 10
            w.len.Value <- at + k + 1
        true

let float (w: JsonWriter) (x: f64) =
    sep w
    if not (fixedPoint w x) then
        append w (if x - x = 0.0 then String.bytes (string x) else b"null")
    afterValue w

let boolean (w: JsonWriter) (b: bool) =
    sep w
    append w (if b then b"true" else b"false")
    afterValue w

let nul (w: JsonWriter) =
    sep w
    append w b"null"
    afterValue w
