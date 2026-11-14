// Driving Chrome or Chromium from Z# through the Chrome DevTools Protocol, in the spirit of
// Puppeteer: launch a browser, open pages, navigate, run JavaScript, click and type, take
// screenshots. The protocol runs over a WebSocket to the browser's `--remote-debugging-port`.
//
// API skeleton: every call below returns `NotImplemented` until it's written. The README lists
// the CDP methods behind each one.
open Zjson
open Internal

/// Why a call failed.
type BrowserError =
    /// The browser didn't start, or didn't print its DevTools address in time.
    | Launch of string
    /// The connection to the browser closed, or failed to read or write.
    | Connection of string
    /// The browser refused a command: the method, its error code and message.
    | Protocol of string * int * string
    /// What was waited for (a selector, a navigation) didn't happen within the timeout.
    | Timeout of string
    /// The page's JavaScript threw: the exception's text.
    | Script of string
    /// Not written yet: the function's name.
    | NotImplemented of string

/// How `Browser.launch` starts the browser, naming only what it changes from the defaults:
/// `Browser.launch { headless = false } body`.
type LaunchOptions =
    { /// The browser's executable, looked up on PATH unless it has a `/`.
      executable: string = "chromium"
      /// Without a window (`--headless=new`).
      headless: bool = true
      /// More command-line flags, after the ones zbrowser passes.
      args: list<string> = []
      /// The default timeout, in milliseconds, for starting and for each page call that waits.
      timeoutMs: int = 30000 }

/// A running browser and the connection to it. Only valid inside `Browser.launch`'s body.
type Browser =
    { conn: Cdp.Connection
      process: option<Process>
      timeoutMs: int }

/// A tab: its target, and the CDP session its commands go to.
type Page =
    { conn: Cdp.Connection
      targetId: string
      sessionId: string
      timeoutMs: int }

let private notYet (name: string) : Task<result<'a, BrowserError>> = task {
    return Error (NotImplemented name)
}

/// Functions on the browser.
module Browser =
    /// Starts the browser, runs `body` with it, and closes it once `body` has finished, failed or
    /// been cancelled. The connection's reader runs alongside `body` in a `Task.scope`, which is
    /// why there's no `Browser.close` to forget.
    ///
    /// TODO(impl): Process.start with `--remote-debugging-port=0`, `--user-data-dir=<temp>`,
    /// `--headless=new`, `--no-first-run`, stderr Piped; read stderr for the line
    /// `DevTools listening on ws://...`; WebSocket.connect; Task.start (Cdp.run conn) in a scope;
    /// run body; Browser.close command, Process.wait (kill after a timeout).
    let launch (options: LaunchOptions) (body: Browser -> Task<result<'a, BrowserError>>) : Task<result<'a, BrowserError>> =
        notYet "Browser.launch"

    /// Connects to a browser that is already running (its `webSocketDebuggerUrl`) and runs `body`.
    /// The browser keeps running afterwards.
    let connect (url: string) (body: Browser -> Task<result<'a, BrowserError>>) : Task<result<'a, BrowserError>> =
        notYet "Browser.connect"

    /// Opens a new tab on `about:blank`.
    ///
    /// TODO(impl): Target.createTarget {url}, Target.attachToTarget {targetId, flatten: true},
    /// then Page.enable and Runtime.enable on the session.
    let newPage (b: Browser) : Task<result<Page, BrowserError>> =
        notYet "Browser.newPage"

    /// The browser's product and version, `Chrome/131.0.6778.85` say (Browser.getVersion).
    let version (b: Browser) : Task<result<string, BrowserError>> =
        notYet "Browser.version"

/// Functions on a page. Calls that look for an element wait for it to be there and visible, up
/// to the page's timeout.
module Page =
    /// Navigates and waits for the `load` event.
    ///
    /// TODO(impl): wait for Page.loadEventFired registered before Page.navigate {url}; an
    /// `errorText` in navigate's result is a failed navigation.
    let goto (p: Page) (url: string) : Task<result<unit, BrowserError>> =
        notYet "Page.goto"

    /// Runs a JavaScript expression and reads its value with zjson's `Deserialize`. A promise is
    /// awaited.
    ///
    /// TODO(impl): Runtime.evaluate {expression, returnByValue: true, awaitPromise: true};
    /// `exceptionDetails` is a `Script` error; `result.value` is read with Deserialize.
    let evaluate (p: Page) (expression: string) : Task<result<'a, BrowserError>> =
        notYet "Page.evaluate"

    /// Waits until an element matches the CSS selector and is visible.
    ///
    /// TODO(impl): one injected function, polled with requestAnimationFrame and awaited through
    /// Runtime.evaluate's awaitPromise, rather than a round trip per poll.
    let waitFor (p: Page) (selector: string) : Task<result<unit, BrowserError>> =
        notYet "Page.waitFor"

    /// Clicks the middle of the element.
    ///
    /// TODO(impl): waitFor, scrollIntoView, getBoundingClientRect, then Input.dispatchMouseEvent
    /// mouseMoved, mousePressed, mouseReleased (button left, clickCount 1).
    let click (p: Page) (selector: string) : Task<result<unit, BrowserError>> =
        notYet "Page.click"

    /// Replaces the value of an input or textarea, as a user typing it would.
    ///
    /// TODO(impl): waitFor, focus, select all, Input.insertText {text}.
    let fill (p: Page) (selector: string) (text: string) : Task<result<unit, BrowserError>> =
        notYet "Page.fill"

    /// Presses a key on the focused element: `Enter`, `Tab`, `ArrowDown`, a character.
    ///
    /// TODO(impl): Input.dispatchKeyEvent keyDown and keyUp, with key, code and
    /// windowsVirtualKeyCode from a small table.
    let press (p: Page) (key: string) : Task<result<unit, BrowserError>> =
        notYet "Page.press"

    /// The page's HTML (`document.documentElement.outerHTML`).
    let content (p: Page) : Task<result<string, BrowserError>> =
        notYet "Page.content"

    /// The visible part of the page as PNG.
    ///
    /// TODO(impl): Page.captureScreenshot {format: "png"}; `data` is Base64.
    let screenshot (p: Page) : Task<result<array<u8>, BrowserError>> =
        notYet "Page.screenshot"

    /// The page printed to PDF (headless only).
    ///
    /// TODO(impl): Page.printToPDF {printBackground: true}; `data` is Base64.
    let pdf (p: Page) : Task<result<array<u8>, BrowserError>> =
        notYet "Page.pdf"

    /// Closes the tab (Target.closeTarget).
    let close (p: Page) : Task<result<unit, BrowserError>> =
        notYet "Page.close"
