// libpq, plus the small C shim in c/zpg.c. Nothing here is part of the public API.
module C = import c "libpq-fe.h"

extern zpg_ptr_to_int : cptr -> int
extern zpg_int_to_ptr : int -> cptr
extern zpg_is_null : cptr -> i32
extern zpg_cstr : cstr -> cptr
extern zpg_send_params : cptr -> cstr -> i32 -> cptr -> i32
extern zpg_format_float : f64 -> ptr<u8> -> i32 -> i32
extern zpg_wake_pair : ptr<i32> -> ptr<i32> -> i32
extern zpg_wake : i32 -> i32
extern zpg_drain : i32 -> i32
extern zpg_close : i32 -> unit

let nullPtr () : cptr = Ptr.toCPtr (Ptr.null () : ptr<u8>)

let isNull (p: cptr) : bool = zpg_is_null p <> i32 0

let ptrOf (id: int) : cptr = zpg_int_to_ptr id

let idOf (p: cptr) : int = zpg_ptr_to_int p

// libpq's strings point into the connection or the result, and often end in a newline.
// A missing field is a null pointer; zpg_cstr leaves it null so it isn't copied.
let copy (p: cstr) : string =
    if isNull (zpg_cstr p) then "" else String.trimEnd (String.ofCStr p)

let osError (rc: i32) : string = unsafe Native.errorText (int rc)

/// A float as text the server accepts, with a '.' whatever the process locale is.
let formatFloat (x: f64) : string =
    let buf: ptr<u8> = unsafe Ptr.alloc 64
    let n = zpg_format_float x buf (i32 64)
    let text =
        if n < i32 0 || n >= i32 64 then
            panic "float rendering did not fit"
        else
            unsafe String.ofBytes (Span.ofPtr buf (int n))
    unsafe Ptr.free buf
    text
