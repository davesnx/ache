#!/usr/bin/env bash
# Cost of one ache server per request: CPU instructions and GC words.
#
#   benchmarks/vs/cost.sh SERVER [--timeouts on|off] PATH CONN
#   benchmarks/vs/cost.sh idle RANGE N
#
# SERVER is lwt, eio or miou (server_ache_<SERVER>.exe). PATH is a route of
# vs.ml, such as /fixed/5 or /close/12288. CONN is keep-alive or close; close
# adds "Connection: close" to each request, so wrk opens a connection per
# request.
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
#   DURATION    seconds of the measured wrk run (10); the window is 2 s less
#   DUNE        dune command (dune); run under `opam exec --` or an opam env
#   NO_BUILD=1  use the server already built in _build/default
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
if [ "$1" = --timeouts ]; then
  timeouts=${2:-}
  shift 2
fi
[ $# -eq 2 ] || usage
path=$1
conn=$2
case $server in lwt | eio | miou) ;; *) usage ;; esac
case $timeouts in on | off) ;; *) usage ;; esac
case $conn in keep-alive | close) ;; *) usage ;; esac
case $path in /*) ;; *) usage ;; esac

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
exe=$root/_build/default/benchmarks/vs/server_ache_$server.exe
if [ "${NO_BUILD:-}" != 1 ]; then
  (cd "$root" && ${DUNE:-dune} build --root . --profile release \
    "benchmarks/vs/server_ache_$server.exe") || exit 1
fi
[ -x "$exe" ] || {
  echo "cost.sh: $exe is not built" >&2
  exit 1
}

url=http://127.0.0.1:$port$path
if curl -s -o /dev/null --max-time 1 "http://127.0.0.1:$port/"; then
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

taskset -c "$SERVER_CPU" "$exe" --port "$port" --timeouts "$timeouts" \
  2>"$tmp/server.err" &
server_pid=$!

ready=
for _ in $(seq 100); do
  if curl -s -o /dev/null --max-time 1 "http://127.0.0.1:$port/fixed/0"; then
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

header=()
[ "$conn" = close ] && header=(-H "Connection: close")
run_wrk() {
  taskset -c "$WRK_CPUS" wrk -t4 -c64 -d"$1" "${header[@]}" "$url"
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
perf stat -x, -o "$tmp/perf.out" -e instructions:u,instructions:k \
  -p "$server_pid" -- sleep "$window"
kill -USR1 "$server_pid"
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
  -v conn="$conn" -v req_s="$req_s" -v user="$user" -v kernel="$kernel" \
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
