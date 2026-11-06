import http from "node:http";

http
  .createServer((req, res) => {
    if (req.url === "/json") {
      const body = JSON.stringify({ hello: "world!" });
      res.writeHead(200, { "content-type": "application/json", "content-length": Buffer.byteLength(body) });
      res.end(body);
      return;
    }
    res.writeHead(200, { "content-length": 12 });
    res.end("hello world!");
  })
  .listen(8082);
