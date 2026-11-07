# zjson

JSON serialization for Z#: `Serialize` / `Deserialize` traits with derives, writing straight to
bytes and reading straight from bytes, without a `Json` tree.

## Benchmarks

`zs bench --release tests/bench.zs --runs 30`, Apple M5 Pro, zs 0.7.0. Median of 30 runs.

| case | std (`Json.print` / `Json.parse + fromJson`) | zjson | speedup |
|---|---|---|---|
| write, 1000 users | 1.26 ms | 333 µs | 3.8× |
| write, 100 posts × 2000 chars | 782 µs | 175 µs | 4.5× |
| read, 1000 users | 1.20 ms | 661 µs | 1.8× |
| read, 100 posts × 2000 chars | 658 µs | 480 µs | 1.4× |

Floats with up to 15 significant digits in 1e-4…1e15 are written without `snprintf`; other
floats fall back to `string x`, which on macOS takes a process-wide locale lock.
