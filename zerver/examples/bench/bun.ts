import { sample, items } from "./item.js";

Bun.serve({
  port: 8081,
  fetch(req) {
    const path = new URL(req.url).pathname;
    if (path === "/json") return Response.json({ hello: "world!" });
    if (path === "/object") return Response.json(sample);
    if (path === "/list") return Response.json(items);
    return new Response("hello world!");
  },
});
