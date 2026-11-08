// Postgres's text format for the types zpg reads and writes beyond plain numbers and strings:
// dates and times, intervals, bytea and arrays. Nothing here is part of the public API.
//
// Reading assumes the output settings zpg sets when it connects: `DateStyle` ISO and
// `IntervalStyle` postgres (the server's defaults), and `bytea_output` hex (the default).

let private at (s: string) (i: int) : int = if i >= 0 && i < String.length s then int (String.byte s i) else -1

let private isDigit (s: string) (i: int) : bool = at s i >= 48 && at s i <= 57

// The number the `len` digits at `i` write, or -1 when one isn't a digit.
let private digits (s: string) (i: int) (len: int) : int =
    let mutable v = 0
    for k in i .. i + len - 1 do
        v <- if v < 0 || not (isDigit s k) then -1 else v * 10 + at s k - 48
    v

// The end of the run of digits that starts at `i`.
let private digitsEnd (s: string) (i: int) : int =
    let mutable j = i
    while isDigit s j do
        j <- j + 1
    j

let private pad (width: int) (n: int) : string = String.padLeftWith width '0' (string n)

let private leap (year: int) : bool = year % 4 = 0 && (year % 100 <> 0 || year % 400 = 0)

let daysIn (year: int) (month: int) : int =
    match month with
    | 2 -> if leap year then 29 else 28
    | 4
    | 6
    | 9
    | 11 -> 30
    | _ -> 31

let validDate (year: int) (month: int) (day: int) : bool =
    year >= 1 && month >= 1 && month <= 12 && day >= 1 && day <= daysIn year month

let validTime (hour: int) (minute: int) (second: int) (nanos: int) : bool =
    hour >= 0 && minute >= 0 && minute <= 59 && second >= 0 && second <= 59 && nanos >= 0 && nanos <= 999999999
    && (hour < 24 || (hour = 24 && minute = 0 && second = 0 && nanos = 0))

// A fraction of a second after the '.' at `i`: its nanoseconds, and the index after it. Digits
// past the ninth are dropped.
let private fraction (s: string) (i: int) : int * int =
    let stop = digitsEnd s i
    let mutable nanos = 0
    let mutable scale = 100000000
    for k in i .. stop - 1 do
        nanos <- nanos + (at s k - 48) * scale
        scale <- scale / 10
    (nanos, stop)

// ---------------------------------------------------------------- dates

/// `2026-11-04` as (year, month, day). Years before 1 (` BC`) and `infinity` are `None`.
let parseDate (s: string) : option<int * int * int> =
    let yearEnd = digitsEnd s 0
    if yearEnd < 4 || String.length s <> yearEnd + 6 || at s yearEnd <> 45 || at s (yearEnd + 3) <> 45 then
        None
    else
        let year = digits s 0 yearEnd
        let month = digits s (yearEnd + 1) 2
        let day = digits s (yearEnd + 4) 2
        if validDate year month day then Some (year, month, day) else None

let formatDate (year: int) (month: int) (day: int) : string =
    $"{pad 4 year}-{pad 2 month}-{pad 2 day}"

// ---------------------------------------------------------------- times of day

// `08:30:00` or `08:30:00.25` from `i`: (hour, minute, second, nanos) and the index after it.
let private clock (s: string) (i: int) : option<int * int * int * int * int> =
    let hour = digits s i 2
    let minute = digits s (i + 3) 2
    let second = digits s (i + 6) 2
    if hour < 0 || at s (i + 2) <> 58 || minute < 0 || at s (i + 5) <> 58 || second < 0 then
        None
    else if at s (i + 8) = 46 then
        if not (isDigit s (i + 9)) then
            None
        else
            let (nanos, stop) = fraction s (i + 9)
            Some (hour, minute, second, nanos, stop)
    else
        Some (hour, minute, second, 0, i + 8)

/// `08:30:00.25` (a `time`) as (hour, minute, second, nanoseconds). `24:00:00` is a time too.
let parseTime (s: string) : option<int * int * int * int> =
    match clock s 0 with
    | Some (h, m, sec, ns, stop) when stop = String.length s && validTime h m sec ns -> Some (h, m, sec, ns)
    | _ -> None

let private fractionText (nanos: int) : string =
    if nanos = 0 then
        ""
    else
        let mutable text = pad 9 nanos
        while String.endsWith "0" text do
            text <- String.sub text 0 (String.length text - 1)
        "." + text

let formatTime (hour: int) (minute: int) (second: int) (nanos: int) : string =
    $"{pad 2 hour}:{pad 2 minute}:{pad 2 second}{fractionText nanos}"

// ---------------------------------------------------------------- timestamps

/// A `timestamp` (`2026-11-04 08:30:00.25`, taken as UTC) or a `timestamptz`
/// (`2026-11-04 10:30:00+02`, `+05:30`, `-03:30:15`), or a `date` (midnight UTC). `None` for
/// `infinity`, a BC date, or a moment outside `DateTime`'s years (1678 to 2262).
let parseTimestamp (s: string) : option<DateTime> =
    let n = String.length s
    let dateEnd = digitsEnd s 0 + 6
    if n = dateEnd then
        match parseDate s with
        | Some (y, m, d) -> DateTime.ofParts y m d 0 0 0 0
        | None -> None
    else if at s dateEnd <> 32 && at s dateEnd <> 84 then
        None
    else
        match (parseDate (String.sub s 0 dateEnd), clock s (dateEnd + 1)) with
        | (Some (y, mo, d), Some (h, mi, sec, ns, stop)) ->
            // the zone: none, or +HH, +HH:MM, +HH:MM:SS
            let sign = if at s stop = 43 then 1 else if at s stop = 45 then -1 else 0
            let offset =
                if stop = n then
                    Some 0
                else if sign = 0 then
                    None
                else
                    let oh = digits s (stop + 1) 2
                    let om = if at s (stop + 3) = 58 then digits s (stop + 4) 2 else 0
                    let os = if at s (stop + 3) = 58 && at s (stop + 6) = 58 then digits s (stop + 7) 2 else 0
                    let len = if at s (stop + 3) <> 58 then 3 else if at s (stop + 6) <> 58 then 6 else 9
                    if oh < 0 || om < 0 || os < 0 || stop + len <> n then None else Some (sign * (oh * 3600 + om * 60 + os))
            match offset with
            | None -> None
            | Some off ->
                // 24:00:00 is the next day's midnight
                let (hour, extra) = if h = 24 && mi = 0 && sec = 0 && ns = 0 then (0, i64 86400) else (h, i64 0)
                match DateTime.ofParts y mo d hour mi sec ns with
                | Some t -> Some { sinceEpoch = t.sinceEpoch + (extra - i64 off) * 1000000000<ns> }
                | None -> None
        | _ -> None

/// A moment as Postgres reads it: ISO 8601 in UTC, `2026-11-04T08:30:00.25Z`. A `timestamptz`
/// column stores that moment; a `timestamp` column stores its UTC wall-clock time.
let formatTimestamp (t: DateTime) : string = DateTime.toIso t

// ---------------------------------------------------------------- intervals

/// An `interval` in Postgres's own style (`1 day 02:03:04.5`, `-00:00:01`, `3 days`) as
/// nanoseconds. A day is 24 hours. An interval with years or months has no fixed length and is an
/// error, as is one past what an `i64` of nanoseconds holds (292 years).
let parseInterval (s: string) : result<i64, string> =
    let parts = List.filter (fun (p: string) -> p <> "") (String.split " " s)
    let perSecond: i64 = 1000000000
    let rec go (rest: list<string>) (total: i64) : result<i64, string> =
        match rest with
        | [] -> Ok total
        | [ time ] when String.contains ":" time ->
            let negative = at time 0 = 45
            let start = if negative || at time 0 = 43 then 1 else 0
            let hoursEnd = digitsEnd time start
            if hoursEnd = start || at time hoursEnd <> 58 then
                Error $"not an interval: {s}"
            else
                // hours can be longer than two digits; minutes and seconds follow them
                let hours: i64 = (Int.parse (String.sub time start (hoursEnd - start)) |> Option.defaultValue (i64 -1))
                let minutes = digits time (hoursEnd + 1) 2
                let seconds = digits time (hoursEnd + 4) 2
                let (nanos, stop) =
                    if at time (hoursEnd + 6) = 46 then fraction time (hoursEnd + 7) else (0, hoursEnd + 6)
                if hours < i64 0 || minutes < 0 || at time (hoursEnd + 3) <> 58 || seconds < 0 || stop <> String.length time then
                    Error $"not an interval: {s}"
                else
                    let span = (hours * i64 3600 + i64 minutes * i64 60 + i64 seconds) * perSecond + i64 nanos
                    Ok (total + (if negative then i64 0 - span else span))
        | count :: unit :: more ->
            let parsed: option<i64> = Int.parse count
            match parsed with
            | None -> Error $"not an interval: {s}"
            | Some n ->
                if unit = "day" || unit = "days" then
                    go more (total + n * i64 86400 * perSecond)
                else if unit = "year" || unit = "years" || unit = "mon" || unit = "mons" then
                    if n = i64 0 then go more total
                    else Error $"an interval with years or months has no fixed length: {s}"
                else
                    Error $"not an interval: {s}"
        | _ -> Error $"not an interval: {s}"
    go parts (i64 0)

/// Nanoseconds as an interval Postgres reads (`-90.5 seconds`). Postgres keeps microseconds, so
/// it rounds the rest.
let formatInterval (nanos: i64) : string =
    let negative = nanos < i64 0
    // the magnitude as seconds and nanoseconds, without negating i64's minimum
    let secs = if negative then i64 0 - (nanos / i64 1000000000) else nanos / i64 1000000000
    let rest = if negative then i64 0 - (nanos % i64 1000000000) else nanos % i64 1000000000
    let sign = if negative then "-" else ""
    $"{sign}{secs}{fractionText (int rest)} seconds"

// ---------------------------------------------------------------- bytea

/// bytea's hex output, `\x0aff`, as bytes. The older `escape` output is an error.
let parseBytes (s: string) : result<array<u8>, string> =
    if not (String.startsWith "\\x" s) then
        Error "bytea isn't in hex (set bytea_output to hex)"
    else
        match Hex.decode (String.sub s 2 (String.length s - 2)) with
        | Some bytes -> Ok bytes
        | None -> Error "bytea isn't in hex"

let formatBytes (bytes: array<u8>) : string = "\\x" + Hex.encode bytes

// ---------------------------------------------------------------- arrays

/// A one-dimensional array's elements, each its text or `None` for NULL: `{1,2,NULL}`,
/// `{"a b","say \"hi\""}`. An element that is itself an array (`{{1,2},{3,4}}`) is its own
/// text, `{1,2}`, for the element type to read in turn.
let parseArray (s: string) : result<list<option<string>>, string> =
    let n = String.length s
    if n < 2 || at s 0 <> 123 || at s (n - 1) <> 125 then
        Error $"not an array: {s}"
    else if n = 2 then
        Ok []
    else
        let items = ref ([] : list<option<string>>)
        let failed = ref ""
        let mutable i = 1
        let mutable more = true
        while more do
            if at s i = 34 then
                // a quoted element: \ escapes the next byte
                let mutable j = i + 1
                let mutable chunks = ([] : list<string>)
                let mutable from = j
                while j < n - 1 && at s j <> 34 do
                    if at s j = 92 then
                        chunks <- String.sub s from (j - from) :: chunks
                        from <- j + 1
                        j <- j + 2
                    else
                        j <- j + 1
                chunks <- String.sub s from (j - from) :: chunks
                items.Value <- Some (String.concat (List.rev chunks)) :: items.Value
                i <- j + 1
            else if at s i = 123 then
                // a nested array, kept whole: braces inside quotes don't count
                let mutable depth = 0
                let mutable j = i
                let mutable quoted = false
                let mutable go = true
                while go && j < n - 1 do
                    let c = at s j
                    if quoted then
                        if c = 92 then j <- j + 1
                        else if c = 34 then quoted <- false
                    else if c = 34 then
                        quoted <- true
                    else if c = 123 then
                        depth <- depth + 1
                    else if c = 125 then
                        depth <- depth - 1
                        if depth = 0 then go <- false
                    j <- j + 1
                items.Value <- Some (String.sub s i (j - i)) :: items.Value
                i <- j
            else
                let mutable j = i
                while j < n - 1 && at s j <> 44 do
                    j <- j + 1
                let text = String.sub s i (j - i)
                items.Value <- (if String.toLower text = "null" then None else Some text) :: items.Value
                i <- j
            if i >= n - 1 then
                more <- false
            else if at s i = 44 then
                i <- i + 1
            else
                failed.Value <- $"not an array: {s}"
                more <- false
        if failed.Value <> "" then Error failed.Value else Ok (List.rev items.Value)

// An element written so the array reads it back: always quoted, `"` and `\` escaped.
let private quote (text: string) : string =
    "\"" + String.replace (String.replace text "\\" "\\\\") "\"" "\\\"" + "\""

/// An array literal of elements: `None` is NULL, `Some (text, true)` a nested array written as is,
/// `Some (text, false)` an element, quoted.
let formatArray (items: list<option<string * bool>>) : string =
    let parts =
        List.map (fun (item: option<string * bool>) ->
            match item with
            | None -> "NULL"
            | Some (text, true) -> text
            | Some (text, false) -> quote text) items
    "{" + String.join "," parts + "}"
