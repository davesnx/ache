#!/bin/sh
set -eu

usage() {
  echo "usage: H1SPEC=/path/to/http_test.ts $0 REPORT_FILE" >&2
  echo "  RUNTIME=eio|lwt|miou (default eio)" >&2
  echo "  NODE=node binary >= 22.18 (default node); runs h1spec with deno_shim.mjs" >&2
  echo "  DENO=deno binary; when set, runs h1spec with Deno instead of Node" >&2
  exit 2
}

[ "$#" -eq 1 ] || usage
report=$1
runtime=${RUNTIME:-eio}
node=${NODE:-node}
deno=${DENO:-}
h1spec_rev=f0a5650a20c575fbea0f7179a3a9cfa50f20ba6e

case $runtime in
  eio | lwt | miou) ;;
  *) echo "RUNTIME must be eio, lwt or miou, got: $runtime" >&2; exit 2 ;;
esac

if [ -z "${H1SPEC:-}" ] || [ ! -f "$H1SPEC" ]; then
  cat >&2 <<EOF
H1SPEC is not set or is not a file. Get the runner with:
  git clone https://github.com/uNetworking/h1spec
  git -C h1spec checkout $h1spec_rev
  export H1SPEC=\$PWD/h1spec/http_test.ts
EOF
  exit 2
fi

here=$(cd "$(dirname "$0")" && pwd)
root=$(dirname "$here")
expected="$here/expected-failures.txt"
server="$root/_build/default/conformance/h1spec_$runtime.exe"

work=$(mktemp -d)
pid=
cleanup() {
  if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
    kill -TERM "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 130' INT TERM

if [ -n "$deno" ]; then
  if ! command -v "$deno" >/dev/null 2>&1; then
    echo "Deno not found: $deno" >&2
    exit 2
  fi
else
  if ! command -v "$node" >/dev/null 2>&1; then
    echo "Node not found: $node. Install Node >= 22.18, or set NODE or DENO." >&2
    exit 2
  fi
  echo 'const probe: number = 1;' >"$work/probe.ts"
  if ! "$node" --disable-warning=ExperimentalWarning "$work/probe.ts" 2>"$work/probe.err"; then
    echo "$node $("$node" --version) cannot run TypeScript; Node >= 22.18 is needed:" >&2
    cat "$work/probe.err" >&2
    exit 2
  fi
  if ! "$node" "$here/deno_shim_test.mjs" >"$work/shim.out" 2>&1; then
    cat "$work/shim.out" >&2
    echo "deno_shim.mjs self-test failed" >&2
    exit 1
  fi
  tail -n 1 "$work/shim.out"
fi

(cd "$root" && dune build "./conformance/h1spec_$runtime.exe")

"$server" -p 0 >"$work/server.out" 2>"$work/server.err" &
pid=$!

port=
i=0
while [ -z "$port" ]; do
  if ! kill -0 "$pid" 2>/dev/null; then
    echo "server exited before listening:" >&2
    cat "$work/server.err" >&2
    exit 1
  fi
  port=$(sed -n 's/.* listening on 127\.0\.0\.1:\([0-9][0-9]*\)$/\1/p' "$work/server.out")
  i=$((i + 1))
  if [ -z "$port" ] && [ "$i" -gt 100 ]; then
    echo "server did not listen within 10 s" >&2
    exit 1
  fi
  [ -n "$port" ] || sleep 0.1
done

status=0
if [ -n "$deno" ]; then
  "$deno" run --allow-net "$H1SPEC" 127.0.0.1 "$port" >"$report" 2>&1 || status=$?
else
  "$node" --disable-warning=ExperimentalWarning --import "file://$here/deno_shim.mjs" \
    "$H1SPEC" 127.0.0.1 "$port" >"$report" 2>&1 || status=$?
fi

kill -TERM "$pid"
if wait "$pid"; then pid=; else
  echo "server exited with status $? on SIGTERM" >&2
  pid=
  exit 1
fi

summary=$(grep -E '^[0-9]+ out of [0-9]+ tests passed\.$' "$report" || true)
if [ -z "$summary" ]; then
  echo "h1spec (exit $status) did not print a summary; report: $report" >&2
  exit 1
fi

sed -n -e 's/^✅ \([^:]*\):.*/PASS \1/p' -e 's/^❌ \([^:]*\):.*/FAIL \1/p' "$report" >"$work/results"
grep -v -e '^[[:space:]]*#' -e '^[[:space:]]*$' "$expected" | sed 's/[[:space:]]*|.*//' | sort -u >"$work/expected"
sed 's/^[A-Z]* //' "$work/results" >"$work/names"

failures=0
total=$(wc -l <"$work/results" | tr -d ' ')
reported=$(echo "$summary" | sed 's/^[0-9]* out of \([0-9]*\) .*/\1/')
if [ "$total" -ne "$reported" ]; then
  echo "parsed $total results but h1spec ran $reported tests; report: $report" >&2
  failures=1
fi

while read -r mark name; do
  if grep -Fxq "$name" "$work/expected"; then
    if [ "$mark" = PASS ]; then
      echo "UNEXPECTED PASS: $name (remove it from expected-failures.txt)"
      failures=1
    else
      echo "expected failure: $name"
    fi
  elif [ "$mark" = FAIL ]; then
    echo "FAIL: $name"
    failures=1
  fi
done <"$work/results"

while read -r name; do
  if ! grep -Fxq "$name" "$work/names"; then
    echo "UNKNOWN CHECK in expected-failures.txt: $name"
    failures=1
  fi
done <"$work/expected"

echo "$runtime: $summary"
exit "$failures"
