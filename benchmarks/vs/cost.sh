#!/usr/bin/env bash
# Cost of one ache server per request: CPU instructions and GC words.
#
#   benchmarks/vs/cost.sh SERVER [OPTION...] PATH CONN
#   benchmarks/vs/cost.sh idle RANGE N
#
# SERVER is lwt, eio or miou (server_ache_<SERVER>.exe). PATH is a route of
# vs.ml, such as /fixed/5 or /close/12288. CONN is keep-alive or close; close
# adds "Connection: close" to each request, so wrk opens a connection per
# request.
#
# Options:
#   --timeouts on|off  the servers' default timeouts (on), or all off
#   --profile P        request fields besides Host: host (none, the default),
#                      curl (2 fields) or browser (11 fields, 600-byte Cookie)
#   --body N           send a body of N bytes; the method becomes POST
#   --method M         request method (GET, or POST with --body)
#   --chunked N        send the body with chunked coding, in chunks of N bytes
#   --pipeline N       write N requests at once on each connection
#   --tls              serve HTTPS (server_ache_SERVER_tls.exe, eio and miou)
#   --ws N             WebSocket echo of N-byte binary messages, with
#                      ws_load.exe instead of wrk (PATH /ws, CONN keep-alive);
#                      a request is then one echoed message
#   --ws-depth N       messages in flight per WebSocket connection (1)
#
# Without options, the request is wrk's own (GET with only Host). The other
# request options give wrk a fixed request through wrk.lua. The options that
# differ from the defaults are added to the output line after conn=.
#
# The script builds the server with --profile=release, starts it pinned to
# SERVER_CPU, warms it up with wrk for 2 s, then runs
# wrk -t4 -c64 -d$DURATION pinned to WRK_CPUS. Over DURATION - 2 seconds of
# that run, `perf stat -p` counts the server's user and kernel instructions,
# and two SIGUSR1 snapshots (vs.ml prints a request count and the GC counters
# on stderr) give the requests and GC words of the same window. It prints one
# line of key=value fields:
#
#   req_s           wrk's requests per second over the whole run
#   user_instr      user instructions per request (instructions:u)
#   kernel_instr    kernel instructions per request (instructions:k)
#   minor_words     minor words allocated per request
#   promoted_words  words promoted to the major heap per request
#   requests        requests the server started in the window
#
# req/s is noisy on a shared host; compare instructions and words.
#
# `idle RANGE N` samples /proc/stat for 5 s and prints the N idlest CPUs of
# RANGE (a taskset list, such as 384-511) as a comma list.
#
# Environment:
#   SERVER_CPU  CPU for the server (required)
#   WRK_CPUS    CPU list for wrk (required), such as 400-403
#   PORT        port on 127.0.0.1 (18300)
#   NOFILE      open file limit of the server (65536)
#   DURATION    seconds of the measured wrk run (10); the window is 2 s less
#   DUNE        dune command (dune); run under `opam exec --` or an opam env
#   NO_BUILD=1  use the server already built in _build/default
#   PERF_RECORD file: also record the window into this perf.data, with call
#               branch stacks (perf record -e instructions:u -c 20000
#               -j any_call,u); read it with
#               perf script -F ip,sym,brstacksym -i file
#
# Needs wrk, perf (perf_event_paranoid <= 1 for instructions:k), taskset and
# curl.

set -u

usage() {
  sed -n '2,/^$/s/^# \{0,1\}//p' "$0" >&2
  exit 2
}

expand_cpus() {
  local part lo hi
  local IFS=,
  for part in $1; do
    case $part in
      *-*)
        lo=${part%-*}
        hi=${part#*-}
        seq "$lo" "$hi"
        ;;
      *) echo "$part" ;;
    esac
  done
}

# Prints "cpu busy total" for each CPU of the list in $1.
cpu_times() {
  local wanted
  wanted=" $(expand_cpus "$1" | tr '\n' ' ') "
  awk -v wanted="$wanted" '
    /^cpu[0-9]/ {
      n = substr($1, 4)
      if (index(wanted, " " n " ") == 0) next
      total = 0
      for (i = 2; i <= NF; i++) total += $i
      print n, total - $5 - $6, total
    }' /proc/stat
}

idle_cpus() {
  local range=$1 count=$2 before after
  before=$(cpu_times "$range")
  sleep 5
  after=$(cpu_times "$range")
  join <(echo "$before" | sort -k1,1) <(echo "$after" | sort -k1,1) \
    | awk '{ t = $5 - $3; b = $4 - $2; print (t > 0 ? b / t : 1), $1 }' \
    | sort -g | head -n "$count" | awk '{ print $2 }' | sort -n | paste -sd,
}

if [ "${1:-}" = idle ]; then
  [ $# -eq 3 ] || usage
  idle_cpus "$2" "$3"
  exit
fi

[ $# -ge 3 ] || usage
server=$1
shift
timeouts=on
profile=host
method=
body=
chunk=
pipeline=1
tls=
ws=
ws_depth=1
number() { case ${1:-} in '' | *[!0-9]*) usage ;; esac }
while [ $# -gt 2 ]; do
  case $1 in
    --timeouts) timeouts=${2:-} ;;
    --profile) profile=${2:-} ;;
    --method) method=${2:-} ;;
    --body) number "${2:-}" && body=$2 ;;
    --chunked) number "${2:-}" && chunk=$2 ;;
    --pipeline) number "${2:-}" && pipeline=$2 ;;
    --ws) number "${2:-}" && ws=$2 ;;
    --ws-depth) number "${2:-}" && ws_depth=$2 ;;
    --tls)
      tls=1
      shift
      continue
      ;;
    *) usage ;;
  esac
  shift 2
done
[ $# -eq 2 ] || usage
path=$1
conn=$2
case $server in lwt | eio | miou) ;; *) usage ;; esac
case $timeouts in on | off) ;; *) usage ;; esac
case $conn in keep-alive | close) ;; *) usage ;; esac
case $path in /*) ;; *) usage ;; esac
case $profile in host | curl | browser) ;; *) usage ;; esac
[ -z "$method" ] && method=${body:+POST}
[ -n "$chunk" ] && [ -z "$body" ] && usage
[ "$pipeline" -ge 1 ] || usage
[ -n "$tls" ] && [ "$server" = lwt ] && usage
if [ -n "$ws" ] && { [ "$conn" != keep-alive ] || [ -n "$tls" ]; }; then
  usage
fi

# The options that differ from the defaults, for the output line.
options=
[ "$profile" != host ] && options+=" profile=$profile"
[ -n "$method" ] && options+=" method=$method"
[ -n "$body" ] && options+=" body=$body"
[ -n "$chunk" ] && options+=" chunked=$chunk"
[ "$pipeline" != 1 ] && options+=" pipeline=$pipeline"
[ -n "$tls" ] && options+=" tls=on"
[ -n "$ws" ] && options+=" ws=$ws"
[ "$ws_depth" != 1 ] && options+=" ws_depth=$ws_depth"

: "${SERVER_CPU:?SERVER_CPU is required (one CPU; see cost.sh idle)}"
: "${WRK_CPUS:?WRK_CPUS is required (a CPU list; see cost.sh idle)}"
port=${PORT:-18300}
duration=${DURATION:-10}
window=$((duration - 2))
[ "$window" -ge 1 ] || {
  echo "cost.sh: DURATION must be at least 3" >&2
  exit 2
}

root=$(cd "$(dirname "$0")/../.." && pwd)
vs=$root/_build/default/benchmarks/vs
exe=$vs/server_ache_$server${tls:+_tls}.exe
targets=("benchmarks/vs/${exe##*/}")
[ -n "$ws" ] && targets+=(benchmarks/vs/ws_load.exe)
if [ "${NO_BUILD:-}" != 1 ]; then
  (cd "$root" && ${DUNE:-dune} build --root . --profile release \
    "${targets[@]}") || exit 1
fi
[ -x "$exe" ] || {
  echo "cost.sh: $exe is not built" >&2
  exit 1
}

# ocaml-tls rejects an IP address in SNI (RFC 6066 §3), which wrk sends for
# 127.0.0.1.
if [ -n "$tls" ]; then
  origin=https://localhost:$port
else
  origin=http://127.0.0.1:$port
fi
url=$origin$path
if curl -sk -o /dev/null --max-time 1 "$origin/"; then
  echo "cost.sh: something already listens on port $port" >&2
  exit 1
fi

tmp=$(mktemp -d)
server_pid=
wrk_pid=
# shellcheck disable=SC2317 # called by the EXIT trap
cleanup() {
  [ -n "$wrk_pid" ] && kill "$wrk_pid" 2>/dev/null
  if [ -n "$server_pid" ]; then
    kill "$server_pid" 2>/dev/null
    wait "$server_pid" 2>/dev/null
  fi
  rm -rf "$tmp"
}
trap cleanup EXIT
trap 'exit 130' INT TERM

# miou 0.10.2 makes two tables per domain of RLIMIT_NOFILE bytes each: with a
# limit of 2^31 that is a 4 GiB major heap, and the major GC then almost never
# runs, which hides its cost.
(
  ulimit -n "${NOFILE:-65536}" || exit 1
  exec taskset -c "$SERVER_CPU" "$exe" --port "$port" --timeouts "$timeouts"
) 2>"$tmp/server.err" &
server_pid=$!

ready=
for _ in $(seq 100); do
  if curl -sk -o /dev/null --max-time 1 "$origin/fixed/0"; then
    ready=1
    break
  fi
  kill -0 "$server_pid" 2>/dev/null || break
  sleep 0.1
done
[ -n "$ready" ] || {
  echo "cost.sh: the server did not start" >&2
  cat "$tmp/server.err" >&2
  exit 1
}

fields=()
case $profile in
  host) ;;
  curl) fields=("User-Agent: curl/8.5.0" "Accept: */*") ;;
  browser)
    cookie="_ga=GA1.1.1234567890.1700000000; _gid=GA1.1.987654321.1700000000"
    cookie+="; session_id=8f14e45fceea167a5a36dedd4bea2543f1b2c3d4e5f60718293a4b5c6d7e8f90"
    cookie+="; csrftoken=Zx9Yw8Vu7Ts6Rq5Po4Nm3Lk2Ji1Hg0FeDcBaZx9Yw8Vu7Ts6Rq5Po4Nm3Lk2Ji1"
    cookie+="; theme=dark; locale=en-US; ab_test=variant_b"
    cookie+="; _fbp=fb.1.1700000000000.1234567890"
    cookie+="; __stripe_mid=abcd1234-ef56-7890-abcd-ef1234567890abcdef"
    cookie+="; consent=analytics:1,marketing:0,functional:1"
    cookie+="; last_seen=2026-11-27T14:00:00Z"
    fields=(
      "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
      "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8"
      "Accept-Language: en-US,en;q=0.9,fr;q=0.8"
      "Accept-Encoding: gzip, deflate, br, zstd"
      "Cookie: $cookie"
      "Referer: http://127.0.0.1/index.html"
      "Sec-Fetch-Dest: document"
      "Sec-Fetch-Mode: navigate"
      "Sec-Fetch-Site: same-origin"
      "Upgrade-Insecure-Requests: 1"
      "Cache-Control: max-age=0"
    )
    ;;
esac
[ "$conn" = close ] && fields+=("Connection: close")

# The request of the wrk.lua script: wrk sends these bytes as they are, and
# counts the requests in them for pipelining.
write_request() {
  local field size left
  printf '%s %s HTTP/1.1\r\nHost: %s\r\n' "${method:-GET}" "$path" \
    "${origin#*://}"
  for field in "${fields[@]}"; do printf '%s\r\n' "$field"; done
  if [ -z "$body" ]; then
    printf '\r\n'
  elif [ -z "$chunk" ]; then
    printf 'Content-Length: %d\r\n\r\n' "$body"
    head -c "$body" /dev/zero | tr '\0' x
  else
    printf 'Transfer-Encoding: chunked\r\n\r\n'
    left=$body
    while [ "$left" -gt 0 ]; do
      size=$((left < chunk ? left : chunk))
      printf '%x\r\n' "$size"
      head -c "$size" /dev/zero | tr '\0' x
      printf '\r\n'
      left=$((left - size))
    done
    printf '0\r\n\r\n'
  fi
}

scripted=
if [ "$profile" != host ] || [ -n "$method" ] || [ "$pipeline" != 1 ]; then
  scripted=1
  for ((i = 0; i < pipeline; i++)); do write_request; done >"$tmp/request"
fi
run_wrk() {
  if [ -n "$ws" ]; then
    taskset -c "$WRK_CPUS" "$vs/ws_load.exe" --port "$port" --path "$path" \
      --size "$ws" --depth "$ws_depth" --duration "$1"
  elif [ -n "$scripted" ]; then
    taskset -c "$WRK_CPUS" wrk -t4 -c64 -d"$1" \
      -s "$root/benchmarks/vs/wrk.lua" "$url" -- "$tmp/request"
  elif [ "$conn" = close ]; then
    taskset -c "$WRK_CPUS" wrk -t4 -c64 -d"$1" -H "Connection: close" "$url"
  else
    taskset -c "$WRK_CPUS" wrk -t4 -c64 -d"$1" "$url"
  fi
}

# Waits for the counters line number $1 and prints it.
counters() {
  for _ in $(seq 50); do
    line=$(grep '^vs-counters ' "$tmp/server.err" | sed -n "$1p")
    [ -n "$line" ] && {
      echo "$line"
      return
    }
    sleep 0.1
  done
  echo "cost.sh: no counters from the server" >&2
  exit 1
}

field() { echo "$1" | tr ' ' '\n' | sed -n "s/^$2=//p"; }
delta() { echo $(($(field "$second" "$1") - $(field "$first" "$1"))); }

run_wrk 2 >"$tmp/warmup.out" 2>&1
run_wrk "$duration" >"$tmp/wrk.out" 2>&1 &
wrk_pid=$!
sleep 1
kill -USR1 "$server_pid"
record_pid=
if [ -n "${PERF_RECORD:-}" ]; then
  perf record -q -o "$PERF_RECORD" -e instructions:u -c 20000 -j any_call,u \
    -p "$server_pid" -- sleep "$window" 2>"$tmp/record.err" &
  record_pid=$!
fi
perf stat -x, -o "$tmp/perf.out" -e instructions:u,instructions:k \
  -p "$server_pid" -- sleep "$window"
kill -USR1 "$server_pid"
[ -n "$record_pid" ] && wait "$record_pid"
wait "$wrk_pid"
wrk_pid=

first=$(counters 1) || exit 1
second=$(counters 2) || exit 1
user=$(awk -F, '$3 ~ /^instructions:u/ { print $1 }' "$tmp/perf.out")
kernel=$(awk -F, '$3 ~ /^instructions:k/ { print $1 }' "$tmp/perf.out")
req_s=$(awk '/^Requests\/sec:/ { printf "%.0f", $2 }' "$tmp/wrk.out")

requests=$(delta requests)
[ "$requests" -gt 0 ] || {
  echo "cost.sh: no request in the window" >&2
  cat "$tmp/wrk.out" >&2
  exit 1
}

awk -v server="$server" -v timeouts="$timeouts" -v path="$path" \
  -v conn="$conn$options" -v req_s="$req_s" -v user="$user" -v kernel="$kernel" \
  -v n="$requests" -v minor="$(delta minor_words)" \
  -v promoted="$(delta promoted_words)" \
  -v minor_gcs="$(delta minor_collections)" \
  -v major_gcs="$(delta major_collections)" \
  'BEGIN {
    printf "server=%s timeouts=%s path=%s conn=%s req_s=%s", server, timeouts, path, conn, req_s
    printf " user_instr=%.0f kernel_instr=%.0f", user / n, kernel / n
    printf " minor_words=%.1f promoted_words=%.2f", minor / n, promoted / n
    printf " requests=%d minor_gcs=%d major_gcs=%d\n", n, minor_gcs, major_gcs
  }'

# wrk errors and anything else the server wrote to stderr.
grep -E 'Socket errors|Non-2xx' "$tmp/wrk.out" >&2
grep -v '^vs-counters ' "$tmp/server.err" >&2
exit 0
