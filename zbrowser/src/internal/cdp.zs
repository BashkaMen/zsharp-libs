// The Chrome DevTools Protocol over a WebSocket: commands `{id, method, params, sessionId?}`,
// their replies `{id, result}` or `{id, error}`, and events `{method, params, sessionId?}`.
// Nothing here is part of the public API.
//
// One reader task (`run`) owns the socket's read side: a reply fulfils the promise `send` left in
// `pending` under its id, an event goes to the waiters `waitEvent` left in `waiters`. Everything
// runs on one event loop, so the refs need no lock.
//
// TODO(impl):
// - send: take an id, add a promise to `pending`, write the command, `Task.wait` the promise.
//   On `Task.timeout` or cancel, remove the id so a late reply is dropped.
// - run: loop WebSocket.receive -> Json.parse -> reply or event. On close or a read error,
//   fulfil every pending promise and waiter with `Closed`.
// - waitEvent: the waiter must be registered before the command that causes the event is sent
//   (Page.navigate -> Page.loadEventFired), or the event can come first and be missed.

open Zjson
open WebSocket

/// Why a command failed.
type CdpError =
    /// The connection closed or failed to read or write.
    | Closed of string
    /// Chrome answered with an error: its code and message.
    | Remote of int * string

// What waits for an event: its method, session and the promise to fulfil.
type private Waiter =
    { method: string
      sessionId: option<string>
      promise: Promise<result<Json, CdpError>> }

/// A connection to the browser. Pages share it, each with its own `sessionId`.
type Connection =
    { socket: Socket
      nextId: ref<int>
      pending: ref<HashMap<int, Promise<result<Json, CdpError>>>>
      waiters: ref<list<Waiter>> }

/// Sends a command and waits for its reply's `result`. `sessionId` is `None` for the browser
/// target (`Target.*`), `Some` for a page.
let send (c: Connection) (sessionId: option<string>) (method: string) (params: Json) : Task<result<Json, CdpError>> = task {
    return Error (Closed $"{method}: not implemented")
}

/// Waits for the next event `method` on the session; its `params`.
let waitEvent (c: Connection) (sessionId: option<string>) (method: string) : Task<result<Json, CdpError>> = task {
    return Error (Closed $"{method}: not implemented")
}

/// Reads the socket until it closes, handing out replies and events. Started once, alongside the
/// connection's users (`Task.start` in the browser's scope).
let run (c: Connection) : Task<unit> = task {
    return ()
}
