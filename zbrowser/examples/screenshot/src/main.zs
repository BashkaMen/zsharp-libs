// Opens a page, reads its title and saves a screenshot: `zs run -- https://example.com`.
open Zbrowser

let title (page: Page) : Task<result<string, BrowserError>> = Page.evaluate page "document.title"

let shoot (url: string) (b: Browser) : Task<result<string, BrowserError>> = task {
    let! page = Browser.newPage b
    let page = page?
    let! loaded = Page.goto page url
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
    let url =
        match Environment.args () with
        | [ u ] -> u
        | _ -> "https://example.com"
    match Task.run (Browser.launch { headless = true } (shoot url)) with
    | Ok name ->
        println $"{name} -> page.png"
        0
    | Error e ->
        eprintln $"zbrowser: {e}"
        1
