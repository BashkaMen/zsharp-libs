open Text
open Types

let createWith (maxDepth: int) (bytes: span<u8>) : JsonReader =
    { src = bytes; pos = ref 0; depth = ref 0; maxDepth = maxDepth }

let create (bytes: span<u8>) : JsonReader = createWith 64 bytes

let fail (r: JsonReader) (message: string) : result<'a, string> = Error $"{message} at byte {r.pos.Value}"

let private at (r: JsonReader) (i: int) : int = if i < Span.length r.src then int r.src.[i] else -1

let private digit (b: int) = b >= 48 && b <= 57 // 0..9

let private skipWs (r: JsonReader) =
    let mutable i = r.pos.Value
    while at r i = 32 || at r i = 10 || at r i = 13 || at r i = 9 do // space \n \r \t
        i <- i + 1
    r.pos.Value <- i

let peek (r: JsonReader) : int =
    skipWs r
    at r r.pos.Value

let atEnd (r: JsonReader) : bool =
    skipWs r
    r.pos.Value >= Span.length r.src

let private expect (r: JsonReader) (b: int) (what: string) : result<unit, string> =
    if peek r <> b then
        fail r $"expected {what}"
    else
        r.pos.Value <- r.pos.Value + 1
        Ok ()

let private word (r: JsonReader) (w: span<u8>) : bool =
    let p = r.pos.Value
    let n = Span.length w
    let mutable same = p + n <= Span.length r.src
    for i in 0 .. n - 1 do
        if same && r.src.[p + i] <> w.[i] then
            same <- false
    if same then
        r.pos.Value <- p + n
    same

let skipNull (r: JsonReader) : bool = peek r = 110 && word r b"null" // n

let boolean (r: JsonReader) : result<bool, string> =
    match peek r with
    | 116 when word r b"true" -> Ok true // t
    | 102 when word r b"false" -> Ok false // f
    | _ -> fail r "expected true or false"

let private digitsEnd (r: JsonReader) (i: int) : int =
    let mutable j = i
    while digit (at r j) do
        j <- j + 1
    j

let private numberEnd (r: JsonReader) : int * bool =
    let start = r.pos.Value
    let i = if at r start = 45 then start + 1 else start // -
    if not (digit (at r i)) || (at r i = 48 && digit (at r (i + 1))) then // 0
        (-1, false)
    else
        let afterInt = digitsEnd r i
        let afterFrac =
            if at r afterInt <> 46 then afterInt // .
            else if digit (at r (afterInt + 1)) then digitsEnd r (afterInt + 1)
            else -1
        let e = if afterFrac < 0 then -1 else at r afterFrac
        let afterExp =
            if afterFrac < 0 then -1
            else if e <> 101 && e <> 69 then afterFrac // e E
            else
                let sign = at r (afterFrac + 1)
                let d = if sign = 43 || sign = 45 then afterFrac + 2 else afterFrac + 1 // + -
                if digit (at r d) then digitsEnd r d else -1
        (afterExp, afterExp = afterInt)

let private floatText (r: JsonReader) (stop: int) : result<f64, string> =
    match String.parseFloat (String.ofBytes r.src.[r.pos.Value .. stop - 1]) with
    | Some x ->
        r.pos.Value <- stop
        Ok x
    | None -> fail r "a number out of range"

let wasFloat (r: JsonReader) (start: int) : bool =
    let mutable i = start
    while i < r.pos.Value && at r i <> 46 && at r i <> 101 && at r i <> 69 do // . e E
        i <- i + 1
    i < r.pos.Value

let float (r: JsonReader) : result<f64, string> =
    skipWs r
    let (stop, _) = numberEnd r
    if stop < 0 then fail r "expected a number" else floatText r stop

let private negative (r: JsonReader) (from: int) (stop: int) : option<i64> =
    let lowest = i64 -9223372036854775807 - i64 1
    let mutable n = i64 0
    let mutable fits = true
    for k in from .. stop - 1 do
        let d = i64 (at r k - 48)
        if fits && n < (lowest + d) / i64 10 then
            fits <- false
        if fits then
            n <- n * i64 10 - d
    if fits then Some n else None

let integer (r: JsonReader) : result<i64, string> =
    skipWs r
    let start = r.pos.Value
    let (stop, integral) = numberEnd r
    let neg = at r start = 45 // -
    let digits = if neg then start + 1 else start
    let exact = if stop < 0 || not integral then None else negative r digits stop
    match exact with
    | _ when stop < 0 -> fail r "expected an integer"
    | Some n when neg ->
        r.pos.Value <- stop
        Ok n
    | Some n when n <> i64 -9223372036854775807 - i64 1 ->
        r.pos.Value <- stop
        Ok -n
    | _ ->
        match floatText r stop with
        | Ok x when x - x = 0.0 && f64 (i64 x) = x && x < 9.2e18 && x > -9.2e18 -> Ok (i64 x)
        | Ok _ ->
            r.pos.Value <- start
            fail r "expected an integer that fits i64"
        | Error e -> Error e

let private hexDigit (b: int) =
    if digit b then b - 48
    else if b >= 97 && b <= 102 then b - 87 // a..f
    else if b >= 65 && b <= 70 then b - 55 // A..F
    else -1

let private hexCode (r: JsonReader) (i: int) =
    let mutable v = 0
    for k in i .. i + 3 do
        let h = hexDigit (at r k)
        v <- if v < 0 || h < 0 then -1 else v * 16 + h
    v

let private closing (r: JsonReader) (from: int) : int * bool =
    let n = Span.length r.src
    let mutable i = from
    let mutable escapes = false
    while i < n && r.src.[i] <> u8 34 do // "
        if r.src.[i] = u8 92 then // \
            escapes <- true
            i <- i + 2
        else
            i <- i + 1
    if i < n then (i, escapes) else (-1, escapes)

let private plainRun (r: JsonReader) (from: int) (stop: int) : int * bool =
    let mutable i = from
    let mutable high = false
    while i < stop && r.src.[i] >= u8 32 && r.src.[i] <> u8 34 && r.src.[i] <> u8 92 do // " \
        high <- high || r.src.[i] >= u8 0x80
        i <- i + 1
    (i, high)

let private validText (r: JsonReader) (bytes: span<u8>) (high: bool) : result<string, string> =
    match (if high then Utf8.validate bytes else Ok ()) with
    | Ok () -> Ok (String.ofBytes bytes)
    | Error at -> Error $"invalid UTF-8 at byte {r.pos.Value + at}"

let private encode (out: Buffer<u8>) (k: int) (cp: int) : int =
    if cp < 0x80 then
        out.[k] <- u8 cp
        k + 1
    else if cp < 0x800 then
        out.[k] <- u8 (0xC0 ||| (cp >>> 6))
        out.[k + 1] <- u8 (0x80 ||| (cp &&& 0x3F))
        k + 2
    else if cp < 0x10000 then
        out.[k] <- u8 (0xE0 ||| (cp >>> 12))
        out.[k + 1] <- u8 (0x80 ||| ((cp >>> 6) &&& 0x3F))
        out.[k + 2] <- u8 (0x80 ||| (cp &&& 0x3F))
        k + 3
    else
        out.[k] <- u8 (0xF0 ||| (cp >>> 18))
        out.[k + 1] <- u8 (0x80 ||| ((cp >>> 12) &&& 0x3F))
        out.[k + 2] <- u8 (0x80 ||| ((cp >>> 6) &&& 0x3F))
        out.[k + 3] <- u8 (0x80 ||| (cp &&& 0x3F))
        k + 4

let private decode (r: JsonReader) (from: int) : result<string, string> =
    let (stop, _) = closing r from
    if stop < 0 then
        r.pos.Value <- Span.length r.src
        fail r "a string that doesn't end"
    else
        let out = Buffer.create (stop - from) (u8 0)
        let mutable i = from
        let mutable k = 0
        let mutable high = false
        let mutable problem = 0
        while i < stop && problem = 0 do
            let b = r.src.[i]
            if b < u8 32 then
                problem <- 1
            else if b <> u8 92 then // \
                let (j, runHigh) = plainRun r i stop
                high <- high || runHigh
                Buffer.blit r.src i out k (j - i)
                k <- k + (j - i)
                i <- j
            else
                let simple =
                    match at r (i + 1) with
                    | 34 | 92 | 47 -> at r (i + 1) // " \ /
                    | 98 -> 8 // \b
                    | 102 -> 12 // \f
                    | 110 -> 10 // \n
                    | 114 -> 13 // \r
                    | 116 -> 9 // \t
                    | _ -> -1
                let u = if at r (i + 1) = 117 then hexCode r (i + 2) else -1 // u
                let low = if at r (i + 6) = 92 && at r (i + 7) = 117 then hexCode r (i + 8) else -1 // \u
                if simple >= 0 then
                    out.[k] <- u8 simple
                    k <- k + 1
                    i <- i + 2
                else if u >= 0xD800 && u <= 0xDBFF && low >= 0xDC00 && low <= 0xDFFF then
                    k <- encode out k (0x10000 + ((u - 0xD800) <<< 10) + (low - 0xDC00))
                    i <- i + 12
                else if u >= 0xD800 && u <= 0xDFFF then
                    problem <- 2
                else if u >= 0 then
                    k <- encode out k u
                    i <- i + 6
                else
                    problem <- 3
        r.pos.Value <- i
        match problem with
        | 1 -> fail r "a control character in a string"
        | 2 -> fail r "a lone surrogate"
        | 3 -> fail r "an unknown escape"
        | _ ->
            let result = validText r out.[0 .. k - 1] high
            r.pos.Value <- stop + 1
            result

let str (r: JsonReader) : result<string, string> =
    if peek r <> 34 then // "
        fail r "expected a string"
    else
        let from = r.pos.Value + 1
        let (i, high) = plainRun r from (Span.length r.src)
        if at r i <> 34 then // "
            decode r from
        else
            r.pos.Value <- from
            let result = validText r r.src.[from .. i - 1] high
            r.pos.Value <- i + 1
            result

let skipValue (r: JsonReader) : result<unit, string> =
    match peek r with
    | 34 -> // "
        match closing r (r.pos.Value + 1) with
        | (stop, _) when stop >= 0 ->
            r.pos.Value <- stop + 1
            Ok ()
        | _ -> fail r "a string that doesn't end"
    | 123 | 91 -> // { [
        let n = Span.length r.src
        let mutable i = r.pos.Value
        let mutable depth = 0
        let mutable ended = false
        while not ended && i < n do
            let b = r.src.[i]
            if b = u8 34 then // "
                let (stop, _) = closing r (i + 1)
                i <- if stop < 0 then n else stop + 1
            else
                if b = u8 123 || b = u8 91 then // { [
                    depth <- depth + 1
                if b = u8 125 || b = u8 93 then // } ]
                    depth <- depth - 1
                ended <- depth = 0
                i <- i + 1
        if not ended then
            fail r "an array or object that doesn't end"
        else
            r.pos.Value <- i
            Ok ()
    | 116 when word r b"true" -> Ok () // t
    | 102 when word r b"false" -> Ok () // f
    | 110 when word r b"null" -> Ok () // n
    | _ ->
        match numberEnd r with
        | (stop, _) when stop >= 0 ->
            r.pos.Value <- stop
            Ok ()
        | _ -> fail r "expected a value"

let private enter (r: JsonReader) : result<unit, string> =
    if r.depth.Value >= r.maxDepth then
        fail r $"nested deeper than {r.maxDepth} levels"
    else
        r.depth.Value <- r.depth.Value + 1
        Ok ()

let beginArray (r: JsonReader) : result<unit, string> =
    (enter r)?
    expect r 91 "'['" // [

let element (r: JsonReader) (index: int) : result<bool, string> =
    match peek r with
    | 93 -> // ]
        r.pos.Value <- r.pos.Value + 1
        r.depth.Value <- r.depth.Value - 1
        Ok false
    | _ when index = 0 -> Ok true
    | 44 -> // ,
        r.pos.Value <- r.pos.Value + 1
        Ok true
    | _ -> fail r "expected ',' or ']'"

let endArray (r: JsonReader) (index: int) : result<unit, string> =
    let rec go (i: int) =
        let more = (element r i)?
        if not more then Ok ()
        else
            (skipValue r)?
            go (i + 1)
    go index

let beginObject (r: JsonReader) : result<int, string> =
    (enter r)?
    (expect r 123 "'{'")? // {
    Ok r.pos.Value

let hasMember (r: JsonReader) (obj: int) : result<bool, string> =
    let first = r.pos.Value = obj
    match peek r with
    | 125 -> Ok false // }
    | 34 when first -> Ok true // "
    | 44 when not first -> // ,
        r.pos.Value <- r.pos.Value + 1
        if peek r = 34 then Ok true else fail r "expected a key"
    | _ when first -> fail r "expected a key or '}'"
    | _ -> fail r "expected ',' or '}'"

let rawKey (r: JsonReader) : result<span<u8>, string> =
    let quote = r.pos.Value
    match closing r (quote + 1) with
    | (stop, _) when stop >= 0 ->
        r.pos.Value <- stop + 1
        (expect r 58 "':' after a key")? // :
        skipWs r
        Ok r.src.[quote .. stop]
    | _ -> fail r "a key that doesn't end"

let private sameBytes (a: span<u8>) (b: span<u8>) : bool =
    let mutable same = Span.length a = Span.length b
    for i in 0 .. Span.length a - 1 do
        if same && a.[i] <> b.[i] then
            same <- false
    same

let private keyIs (r: JsonReader) (name: span<u8>) : result<bool, string> =
    let quote = r.pos.Value
    let (stop, escapes) = closing r (quote + 1)
    if stop >= 0 && not escapes then
        let _ = (rawKey r)?
        Ok (sameBytes r.src.[quote + 1 .. stop - 1] name)
    else
        let key = (str r)?
        (expect r 58 "':' after a key")? // :
        skipWs r
        Ok (sameBytes (String.bytes key) name)

let seekKey (r: JsonReader) (obj: int) (name: string) : result<bool, string> =
    let key = String.bytes name
    let hint = r.pos.Value
    let rec scan (wrapped: bool) : result<bool, string> =
        if wrapped && r.pos.Value >= hint then
            Ok false
        else
            let more = (hasMember r obj)?
            if not more && (wrapped || hint = obj) then
                Ok false
            else if not more then
                r.pos.Value <- obj
                scan true
            else
                let found = (keyIs r key)?
                if found then
                    Ok true
                else
                    (skipValue r)?
                    scan wrapped
    scan false

let endObject (r: JsonReader) (obj: int) : result<unit, string> =
    let rec go () =
        let more = (hasMember r obj)?
        if not more then
            r.pos.Value <- r.pos.Value + 1
            r.depth.Value <- r.depth.Value - 1
            Ok ()
        else
            let _ = (rawKey r)?
            (skipValue r)?
            go ()
    go ()
