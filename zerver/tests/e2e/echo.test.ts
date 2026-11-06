import { test } from "node:test";
import assert from "node:assert/strict";
import net from "node:net";

const base = process.env.ZERVER_URL ?? "http://127.0.0.1:8080";

function rawStatusLine(request: string): Promise<string> {
  const { hostname, port } = new URL(base);
  return new Promise((resolve, reject) => {
    const socket = net.connect(Number(port), hostname, () => socket.write(request));
    let data = "";
    socket.setTimeout(2000, () => {
      socket.destroy();
      reject(new Error(`no status line within 2s, got: ${JSON.stringify(data)}`));
    });
    socket.on("data", chunk => {
      data += chunk.toString("latin1");
      const end = data.indexOf("\r\n");
      if (end < 0)
        return;
      socket.destroy();
      resolve(data.slice(0, end));
    });
    socket.on("error", reject);
  });
}

test("GET / returns 200", async () => {
  const response = await fetch(`${base}/`);

  assert.equal(response.status, 200);
});

test("POST echoes the text body back", async () => {
  const response = await fetch(`${base}/echo`, { method: "POST", body: "hello, zerver" });

  assert.equal(response.status, 200);
  assert.equal(await response.text(), "hello, zerver");
});

test("POST echoes a binary body byte for byte", async () => {
  const body = new Uint8Array(256).map((_, i) => i);

  const response = await fetch(`${base}/echo`, { method: "POST", body });

  assert.deepEqual(new Uint8Array(await response.arrayBuffer()), body);
});

test("POST with a 1 MB body echoes it whole across many socket reads", async () => {
  const body = new Uint8Array(1024 * 1024).map((_, i) => i % 251);

  const response = await fetch(`${base}/echo`, { method: "POST", body });

  assert.equal(response.headers.get("content-length"), String(body.length));
  assert.deepEqual(new Uint8Array(await response.arrayBuffer()), body);
});

test("a malformed request line gets 400", async () => {
  const status = await rawStatusLine("NOT A VALID REQUEST LINE\r\n\r\n");

  assert.match(status, /^HTTP\/1\.[01] 400 /);
});
