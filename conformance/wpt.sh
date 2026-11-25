#!/bin/sh
# Runs conformance/wpt/ache.any.js in a browser with web-platform-tests,
# against ws_echo_$RUNTIME.exe. Manual: not run in CI.
set -eu

usage() {
  echo "usage: WPT_ROOT=/path/to/wpt $0 [wpt run options]" >&2
  echo "  RUNTIME=eio|lwt|miou (default eio)" >&2
  echo "  WPT_BROWSER=chromium, or a product of wpt run (default chromium)" >&2
  echo "  ACHE_PORT=server port on 127.0.0.1 (default 8090)" >&2
  exit 2
}

runtime=${RUNTIME:-eio}
browser=${WPT_BROWSER:-chromium}
port=${ACHE_PORT:-8090}

case $runtime in
  eio | lwt | miou) ;;
  *) echo "RUNTIME must be eio, lwt or miou, got: $runtime" >&2; usage ;;
esac

if [ -z "${WPT_ROOT:-}" ] || [ ! -x "$WPT_ROOT/wpt" ]; then
  cat >&2 <<EOF
WPT_ROOT is not a web-platform-tests checkout. Get one with:
  git clone --depth 1 https://github.com/web-platform-tests/wpt
  export WPT_ROOT=\$PWD/wpt
EOF
  usage
fi

here=$(cd "$(dirname "$0")" && pwd)
root=$(dirname "$here")
server="$root/_build/default/conformance/ws_echo_$runtime.exe"
test_dir="$WPT_ROOT/ache-conformance"

if [ -e "$test_dir" ]; then
  echo "$test_dir exists; remove it or let a running wpt.sh finish" >&2
  exit 2
fi

work=$(mktemp -d)
pid=
cleanup() {
  if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
    kill -TERM "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  fi
  rm -f "$test_dir/ache.any.js"
  rmdir "$test_dir" 2>/dev/null || true
  rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 130' INT TERM

(cd "$root" && dune build --root . "./conformance/ws_echo_$runtime.exe")

mkdir "$test_dir"
sed "s/127\.0\.0\.1:8090/127.0.0.1:$port/" "$here/wpt/ache.any.js" >"$test_dir/ache.any.js"

"$server" -p "$port" >"$work/server.out" 2>"$work/server.err" &
pid=$!

i=0
until grep -q " listening on 127\.0\.0\.1:$port\$" "$work/server.out"; do
  if ! kill -0 "$pid" 2>/dev/null; then
    echo "server exited before listening:" >&2
    cat "$work/server.err" >&2
    exit 1
  fi
  i=$((i + 1))
  if [ "$i" -gt 100 ]; then
    echo "server did not listen within 10 s" >&2
    exit 1
  fi
  sleep 0.1
done

# wpt run's chromium product needs the WPT hosts in /etc/hosts. Its chrome
# product maps them in the browser (--host-resolver-rules) instead, and runs
# any Chromium build given with --binary.
if [ "$browser" = chromium ]; then
  binary=$(command -v chromium || command -v chromium-browser || true)
  if [ -z "$binary" ]; then
    echo "chromium not found on PATH" >&2
    exit 2
  fi
  set -- --binary "$binary" "$@"
  browser=chrome
fi

# --test-types: the default includes accessibility tests, whose requirements
# need PyGObject built from source.
status=0
(cd "$WPT_ROOT" &&
  ./wpt run --test-types testharness "$@" "$browser" ache-conformance/ache.any.js) ||
  status=$?
if [ -s "$work/server.err" ]; then
  echo "server stderr:"
  cat "$work/server.err"
fi
exit "$status"
