# zbrowser

Driving Chrome or Chromium from Z# through the [Chrome DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/)
(CDP), in the spirit of Puppeteer: launch a browser, open pages, navigate, run JavaScript, click
and type, take screenshots and PDFs.

> **Status: API skeleton.** The types and signatures are here and type-check; every call returns
> `NotImplemented` until it's written. `TODO(impl)` comments in the sources say how each one goes.

## Usage

```fsharp
open Zbrowser

let title (page: Page) : Task<result<string, BrowserError>> = Page.evaluate page "document.title"

let shoot (b: Browser) : Task<result<string, BrowserError>> = task {
    let! page = Browser.newPage b
    let page = page?
    let! loaded = Page.goto page "https://example.com"
    loaded?
    let! name = title page
    let name = name?
    let! png = Page.screenshot page
    let png = png?
    match File.writeAllBytes "page.png" png with
    | Error e -> return Error (Launch e)
    | Ok () -> return Ok name
}

let main () =
    match Task.run (Browser.launch { headless = true } shoot) with
    | Ok name ->
        println $"{name} -> page.png"
        0
    | Error e ->
        eprintln $"zbrowser: {e}"
        1
```

`Browser.launch` starts the browser, runs the body and closes the browser once the body has
finished, failed or been cancelled, like zpg's `Pool.withConnection`. The connection's reader task
runs alongside the body in a `Task.scope`, so there's no `close` to forget.

## API

| | CDP behind it |
|---|---|
| `Browser.launch options body` | starts the process, reads `DevTools listening on ws://…` from stderr, connects |
| `Browser.connect url body` | connects to a browser that's already running |
| `Browser.newPage b` | `Target.createTarget`, `Target.attachToTarget {flatten: true}`, `Page.enable`, `Runtime.enable` |
| `Browser.version b` | `Browser.getVersion` |
| `Page.goto p url` | `Page.navigate`, then waits for `Page.loadEventFired` |
| `Page.evaluate p js` | `Runtime.evaluate {returnByValue, awaitPromise}`, the value read with zjson's `Deserialize` |
| `Page.waitFor p selector` | an injected function that polls until the element is there and visible |
| `Page.click p selector` | `waitFor`, the element's box, `Input.dispatchMouseEvent` move, press, release |
| `Page.fill p selector text` | `waitFor`, focus, select all, `Input.insertText` |
| `Page.press p key` | `Input.dispatchKeyEvent` keyDown, keyUp |
| `Page.content p` | `document.documentElement.outerHTML` |
| `Page.screenshot p` | `Page.captureScreenshot`, Base64-decoded PNG |
| `Page.pdf p` | `Page.printToPDF`, Base64-decoded |
| `Page.close p` | `Target.closeTarget` |

`LaunchOptions` names only what it changes: `executable` (`"chromium"`), `headless` (`true`),
`args` (more flags) and `timeoutMs` (`30000`, for starting and for each call that waits).

Errors are a `BrowserError`: `Launch`, `Connection`, `Protocol (method, code, message)`,
`Timeout`, `Script` (the page's JavaScript threw), `NotImplemented`.

## Design

```
Browser.launch ──► Process.start chromium --remote-debugging-port=0 --headless=new …
                   stderr: "DevTools listening on ws://127.0.0.1:PORT/devtools/browser/ID"
                        │
                        ▼
              Internal.WebSocket ── text frames over Tcp ──► Chrome
                        ▲
                        │
              Internal.Cdp.Connection
                ├─ send: id → Promise in `pending`, write {id, method, params, sessionId}
                ├─ waitEvent: Promise in `waiters`, fulfilled by the next matching event
                └─ run: one reader task, reply → pending[id], event → waiters
```

- **Transport: a WebSocket, not `--remote-debugging-pipe`.** The pipe transport wants the browser
  to get fds 3 and 4, and std's `Process.start` gives a child only stdin, stdout and stderr. The
  WebSocket client is small (text frames, masking, ping/pong, close) and also lets
  `Browser.connect` reach a browser that's already running, in Docker say.
- **One reader, many waiters.** Replies come back in any order and events come between them, so
  one task reads and hands each message out: a reply fulfils the `Promise` its `send` left under
  its id (`Task.promise`, `Task.fulfill`, `Task.wait`), an event the waiters for its method and
  session. Everything runs on one event loop, so plain `ref`s hold the tables.
- **Sessions.** `Target.attachToTarget` with `flatten: true` gives each page a `sessionId` that its
  commands carry, so every page shares the one connection.
- **Waiting for an event registers first.** `goto` registers its wait for `Page.loadEventFired`
  before it sends `Page.navigate`; the other way round the event can come first and be missed.
- **Waiting for elements happens in the page.** `waitFor` injects one function that polls with
  `requestAnimationFrame` and resolves once the selector matches a visible element; it's awaited
  through `Runtime.evaluate`'s `awaitPromise`, with the timeout around it, rather than a round
  trip per poll.

## Not in the first version

Frames and iframes, request interception (`Fetch.*`), cookies, file uploads, downloads, dialogs,
several browser contexts, Firefox.

## Running

Needs Chrome or Chromium on PATH (`executable` names another). From `examples/screenshot`:

```sh
zs run -- https://example.com
```
