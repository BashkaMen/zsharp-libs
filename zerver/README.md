# zerver

HTTP server for Z#.

## Benchmarks

`examples/bench`: the same three routes in zerver, Bun, Node, ASP.NET Core minimal API and Oxpecker (F#).
`wrk -t4 -c64 -d10s`, Apple M5 Pro (18 cores), server and `wrk` on the same machine.
Requests per second:

| route | zerver + zjson | zerver + std Json | Bun | Node | .NET | Oxpecker |
|---|---|---|---|---|---|---|
| `/json` (18 B) | 154k | 155k | **224k** | 178k | 130k | 132k |
| `/object` (1.4 KB) | 155k | 154k | **204k** | 156k | 129k | 130k |
| `/list` (141 KB) | **40.6k** | 5.2k | 15.8k | 12.0k | 38.6k | 38.7k |

Memory, RSS in MB: idle is after start and one request; under load is the peak of `ps -o rss`, sampled
every 0.1 s through the 10 s run, a fresh process for each route.

| | zerver + zjson | zerver + std Json | Bun | Node | .NET | Oxpecker |
|---|---|---|---|---|---|---|
| idle | **3.5** | 3.6 | 21.5 | 53.6 | 83.3 | 82.3 |
| `/json` | 6.2 | **5.8** | 43.5 | 82.7 | 273 | 281 |
| `/object` | **6.0** | **6.0** | 33.0 | 83.7 | 276 | 281 |
| `/list` | 17.9 | **17.6** | 28.0 | 139 | 280 | 300 |

zerver on one thread (`ZS_WORKERS=1`), requests per second and peak RSS in MB:

| route | zjson | std Json | RSS, zjson | RSS, std Json |
|---|---|---|---|---|
| `/json` | 200k | 200k | 3.9 | 3.9 |
| `/object` | 147k | 93k | 4.0 | 4.0 |
| `/list` | 5.2k | 1.6k | 5.5 | 5.1 |

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
