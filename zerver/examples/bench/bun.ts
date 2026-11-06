Bun.serve({
  port: 8081,
  fetch(req) {
    if (new URL(req.url).pathname === "/json") return Response.json({ hello: "world!" });
    return new Response("hello world!");
  },
});
