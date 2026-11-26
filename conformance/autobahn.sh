#!/bin/sh
# Runs the Autobahn WebSocket test suite (fuzzingclient) against ws_echo_$RUNTIME.exe
# and compares the result with autobahn-expected.txt.
set -eu

usage() {
  echo "usage: $0" >&2
  echo "  RUNTIME=eio|lwt|miou (default eio)" >&2
  echo "  NODE=node binary (default node); compares the report" >&2
  echo "  AUTOBAHN_IMAGE=image (default: crossbario/autobahn-testsuite pinned by digest)" >&2
  exit 2
}

[ "$#" -eq 0 ] || usage
runtime=${RUNTIME:-eio}
node=${NODE:-node}
image=${AUTOBAHN_IMAGE:-crossbario/autobahn-testsuite@sha256:519915fb568b04c9383f70a1c405ae3ff44ab9e35835b085239c258b6fac3074}

case $runtime in
  eio | lwt | miou) ;;
  *) echo "RUNTIME must be eio, lwt or miou, got: $runtime" >&2; exit 2 ;;
esac

for tool in docker "$node"; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "$tool not found" >&2
    exit 2
  fi
done

here=$(cd "$(dirname "$0")" && pwd)
root=$(dirname "$here")
expected="$here/autobahn-expected.txt"
server="$root/_build/default/conformance/ws_echo_$runtime.exe"
report="$root/_build/autobahn/$runtime"
container=ache-autobahn-$runtime-$$

work=$(mktemp -d)
pid=
cleanup() {
  docker kill "$container" >/dev/null 2>&1 || true
  if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
    kill -TERM "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 130' INT TERM

(cd "$root" && dune build --root . "./conformance/ws_echo_$runtime.exe")

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

cat >"$work/fuzzingclient.json" <<EOF
{
  "outdir": "/reports",
  "servers": [ { "agent": "ache-$runtime", "url": "ws://127.0.0.1:$port/ws" } ],
  "cases": [ "*" ],
  "exclude-cases": [ "12.*", "13.*" ],
  "exclude-agent-cases": {}
}
EOF

mkdir -p "$report"
rm -f "$report/index.json"
# --network host: the server listens on 127.0.0.1 of the host (Linux only).
docker run --rm --name "$container" --network host \
  --user "$(id -u):$(id -g)" \
  --volume "$work:/config:ro" \
  --volume "$report:/reports" \
  "$image" \
  wstest -m fuzzingclient -s /config/fuzzingclient.json >"$work/wstest.out" 2>&1 || {
  status=$?
  tail -n 20 "$work/wstest.out" >&2
  echo "wstest exited with status $status" >&2
  exit 1
}

if ! kill -0 "$pid" 2>/dev/null; then
  echo "server exited during the run:" >&2
  cat "$work/server.err" >&2
  exit 1
fi
if [ -s "$work/server.err" ]; then
  echo "server stderr:"
  cat "$work/server.err"
fi

echo "report: $report/index.html"
"$node" "$here/autobahn_compare.mjs" "$report/index.json" "$expected"
