# zerver

HTTP server for Z#.

## Benchmarks

`examples/bench`: the same three routes in zerver, Bun, Node, ASP.NET Core minimal API and Oxpecker (F#).
`wrk -t4 -c64 -d10s`, Apple M5 Pro (18 cores), server and `wrk` on the same machine.
Requests per second.

| route | zerver + zjson | zerver + std Json | Bun | Node | .NET | Oxpecker |
|---|---|---|---|---|---|---|
| `/json` (18 B) | 153k | 153k | **222k** | 158k | 128k | 130k |
| `/object` (1.4 KB) | 152k | 153k | **203k** | 138k | 128k | 128k |
| `/list` (141 KB) | **41.1k** | 5.1k | 15.5k | 13.3k | 36.8k | 37.4k |

zerver on one thread (`ZS_WORKERS=1`):

| route | zjson | std Json |
|---|---|---|
| `/json` | 168k | 171k |
| `/object` | 120k | 84k |
| `/list` | 5.1k | 1.6k |

Routes under `/std/` write with `Json.print`; the rest with zjson. To run:

```sh
cd examples/bench
zs build --release && ./build/bench   # :8080
bun bun.ts                            # :8081
node node.mjs                         # :8082
cd dotnet && dotnet run -c Release    # :8083
cd ../oxpecker && dotnet run -c Release    # :8084
wrk -t4 -c64 -d10s http://127.0.0.1:8080/list
```
