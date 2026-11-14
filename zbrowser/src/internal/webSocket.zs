// A WebSocket client (RFC 6455), only as much as CDP needs: `ws://` to localhost, text frames,
// ping/pong and close. Nothing here is part of the public API.
//
// TODO(impl):
// - connect: parse `ws://host:port/path`, Tcp.connect, send the upgrade request with a random
//   16-byte Sec-WebSocket-Key (Random/Entropy + Base64.encode), read the head up to "\r\n\r\n",
//   expect "101". Checking Sec-WebSocket-Accept needs SHA-1, which std doesn't have; Chrome on
//   localhost can be trusted to skip it.
// - sendText: one FIN frame, opcode 0x1, masked (a client must mask), 7/16/64-bit length.
// - receive: read frames into `buf`, join continuation frames (opcode 0x0), answer ping (0x9)
//   with pong (0xA), give `None` on close (0x8). Chrome's frames aren't masked.

/// An open connection: the TCP stream, and bytes read past the last whole frame.
type Socket =
    { stream: TcpStream
      pending: Buffer<u8> }

/// Connects and upgrades to WebSocket. `url` is Chrome's `webSocketDebuggerUrl`.
let connect (url: string) : Task<result<Socket, string>> = task {
    return Error $"WebSocket.connect {url}: not implemented"
}

/// Sends one text message.
let sendText (s: Socket) (text: string) : Task<result<unit, string>> = task {
    return Error "WebSocket.sendText: not implemented"
}

/// The next text message; `None` once the other side has closed.
let receive (s: Socket) : Task<result<option<string>, string>> = task {
    return Error "WebSocket.receive: not implemented"
}

/// Sends a close frame and closes the stream.
let close (s: Socket) : Task<unit> = task {
    return ()
}
