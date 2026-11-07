import http from "node:http";
import { sample, items } from "./item.js";

const json = (res, value) => {
  const body = JSON.stringify(value);
  res.writeHead(200, { "content-type": "application/json", "content-length": Buffer.byteLength(body) });
  res.end(body);
};

http
  .createServer((req, res) => {
    if (req.url === "/json") return json(res, { hello: "world!" });
    if (req.url === "/object") return json(res, sample);
    if (req.url === "/list") return json(res, items);
    res.writeHead(200, { "content-length": 12 });
    res.end("hello world!");
  })
  .listen(8082);
