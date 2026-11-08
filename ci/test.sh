#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."

for pkg in */; do
  compgen -G "$pkg/tests/*.zs" >/dev/null || continue
  echo "::group::zs test $pkg"
  (cd "$pkg" && zs test)
  echo "::endgroup::"
done

for example in */examples/*/; do
  echo "::group::zs build $example"
  (cd "$example" && zs build --release)
  echo "::endgroup::"
done

echo "::group::zerver e2e"
./zerver/examples/echo/build/echo &
server=$!
trap 'kill $server' EXIT
for _ in $(seq 50); do
  curl -fs -o /dev/null http://127.0.0.1:8080/ && break
  sleep 0.1
done
(cd zerver && node --test 'tests/e2e/*.test.ts')
echo "::endgroup::"
