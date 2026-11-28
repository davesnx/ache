#!/usr/bin/env bash
# Cost and throughput of one ache server per request, over 1, 2, 4, 8 domains.
#
#   benchmarks/vs/scale.sh SERVER [--timeouts on|off] PATH CONN [DOMAINS...]
#
# SERVER is eio or miou (server_ache_<SERVER>.exe, which serves with
# Server.run ~domains). PATH is a route of vs.ml, such as /fixed/5. CONN is
# keep-alive or close; close adds "Connection: close" to each request.
# DOMAINS defaults to 1 2 4 8.
#
# For each domain count N, the script starts the server with --domains N,
# pinned to the first N CPUs of SERVER_CPUS, warms it up with wrk for 2 s,
# then runs wrk -t min(4N, |WRK_CPUS|) -c 64N for DURATION seconds pinned to
# WRK_CPUS. Over DURATION - 2 seconds of that run, `perf stat -p` counts the
# instructions of every server thread, /proc gives the CPU time of the
# server and of wrk, and two SIGUSR1
# snapshots (vs.ml) give the requests and GC words of the same window. One
# line per N:
#
#   domains         N
#   req_s           wrk's requests per second over the whole run
#   req_s_domain    req_s / N
#   server_util     server CPU time / (window * N): 1.0 is saturated
#   wrk_util        wrk CPU time / (window * wrk threads): near 1.0, the
#                   client is the limit
#   user_instr      user instructions per request, all threads
#   kernel_instr    kernel instructions per request
#   minor_words     minor words per request (Gc.quick_stat, all domains)
#   promoted_words  promoted words per request
#   minor_gcs       minor collections per 1000 requests (each one stops every
#                   domain)
#   major_gcs       major cycles per million requests
#   spread          requests of each domain in the window, in % of the total
#
# Compare instructions and words; req/s and utilisation are noisy on a shared
# host. OCAMLRUNPARAM reaches the server, for example s=4M for a larger minor
# heap.
#
# Environment:
#   SERVER_CPUS  CPU list for the server, at least max(DOMAINS) CPUs (required)
#   WRK_CPUS     CPU list for wrk (required)
#   PORT         port on 127.0.0.1 (18300)
#   DURATION     seconds of the measured wrk run (10); the window is 2 s less
#   DUNE         dune command (dune); run under `opam exec --` or an opam env
#   NO_BUILD=1   use the server already built in _build/default
#
# Needs wrk, perf (perf_event_paranoid <= 1 for instructions:k), taskset and
# curl. `benchmarks/vs/cost.sh idle RANGE N` picks idle CPUs.

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

[ $# -ge 3 ] || usage
server=$1
shift
timeouts=on
if [ "$1" = --timeouts ]; then
  timeouts=${2:-}
  shift 2
fi
[ $# -ge 2 ] || usage
path=$1
conn=$2
shift 2
domain_counts=("$@")
[ ${#domain_counts[@]} -gt 0 ] || domain_counts=(1 2 4 8)
case $server in eio | miou) ;; *) usage ;; esac
case $timeouts in on | off) ;; *) usage ;; esac
case $conn in keep-alive | close) ;; *) usage ;; esac
case $path in /*) ;; *) usage ;; esac

: "${SERVER_CPUS:?SERVER_CPUS is required (a CPU list; see cost.sh idle)}"
: "${WRK_CPUS:?WRK_CPUS is required (a CPU list; see cost.sh idle)}"
mapfile -t server_cpus < <(expand_cpus "$SERVER_CPUS")
wrk_cpu_count=$(expand_cpus "$WRK_CPUS" | wc -l)
for n in "${domain_counts[@]}"; do
  if [ "$n" -lt 1 ] || [ "$n" -gt ${#server_cpus[@]} ]; then
    echo "scale.sh: $n domains need $n CPUs in SERVER_CPUS" >&2
    exit 2
  fi
done
port=${PORT:-18300}
duration=${DURATION:-10}
window=$((duration - 2))
[ "$window" -ge 1 ] || {
  echo "scale.sh: DURATION must be at least 3" >&2
  exit 2
}

root=$(cd "$(dirname "$0")/../.." && pwd)
exe=$root/_build/default/benchmarks/vs/server_ache_$server.exe
if [ "${NO_BUILD:-}" != 1 ]; then
  (cd "$root" && ${DUNE:-dune} build --root . --profile release \
    "benchmarks/vs/server_ache_$server.exe") || exit 1
fi
[ -x "$exe" ] || {
  echo "scale.sh: $exe is not built" >&2
  exit 1
}

# Miou_unix allocates and clears two bytes per possible file descriptor in
# each domain; with a huge RLIMIT_NOFILE (2^31 in some containers) a domain
# takes seconds and gigabytes to start.
nofile=$(ulimit -n)
if [ "$nofile" = unlimited ] || [ "$nofile" -gt 65536 ]; then
  ulimit -n 65536
fi

url=http://127.0.0.1:$port$path
tmp=$(mktemp -d)
server_pid=
wrk_pid=
# shellcheck disable=SC2317 # called by the EXIT trap and between runs
stop_server() {
  [ -n "$wrk_pid" ] && kill "$wrk_pid" 2>/dev/null
  wrk_pid=
  if [ -n "$server_pid" ]; then
    kill "$server_pid" 2>/dev/null
    wait "$server_pid" 2>/dev/null
  fi
  server_pid=
}
# shellcheck disable=SC2317
cleanup() {
  stop_server
  rm -rf "$tmp"
}
trap cleanup EXIT
trap 'exit 130' INT TERM

header=()
[ "$conn" = close ] && header=(-H "Connection: close")

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
  echo "scale.sh: no counters from the server" >&2
  exit 1
}

# utime + stime of process $1 in clock ticks.
cpu_ticks() { awk '{ print $14 + $15 }' "/proc/$1/stat"; }

run() {
  local n=$1 cpus threads conns first second user kernel req_s
  local wrk_before wrk_after server_before server_after
  cpus=$(printf '%s\n' "${server_cpus[@]:0:n}" | paste -sd,)
  threads=$((4 * n < wrk_cpu_count ? 4 * n : wrk_cpu_count))
  conns=$((64 * n))
  if curl -s -o /dev/null --max-time 1 "http://127.0.0.1:$port/"; then
    echo "scale.sh: something already listens on port $port" >&2
    exit 1
  fi
  : >"$tmp/server.err"
  taskset -c "$cpus" "$exe" --port "$port" --timeouts "$timeouts" \
    --domains "$n" 2>"$tmp/server.err" &
  server_pid=$!
  local ready=
  for _ in $(seq 100); do
    if curl -s -o /dev/null --max-time 1 "http://127.0.0.1:$port/fixed/0"; then
      ready=1
      break
    fi
    kill -0 "$server_pid" 2>/dev/null || break
    sleep 0.1
  done
  [ -n "$ready" ] || {
    echo "scale.sh: the server did not start" >&2
    cat "$tmp/server.err" >&2
    exit 1
  }
  taskset -c "$WRK_CPUS" wrk -t"$threads" -c"$conns" -d2 "${header[@]}" \
    "$url" >"$tmp/warmup.out" 2>&1
  taskset -c "$WRK_CPUS" wrk -t"$threads" -c"$conns" -d"$duration" \
    "${header[@]}" "$url" >"$tmp/wrk.out" 2>&1 &
  wrk_pid=$!
  sleep 1
  wrk_before=$(cpu_ticks "$wrk_pid")
  server_before=$(cpu_ticks "$server_pid")
  kill -USR1 "$server_pid"
  perf stat -x, -o "$tmp/perf.out" -e instructions:u,instructions:k \
    -p "$server_pid" -- sleep "$window" || exit 1
  kill -USR1 "$server_pid"
  wrk_after=$(cpu_ticks "$wrk_pid")
  server_after=$(cpu_ticks "$server_pid")
  wait "$wrk_pid"
  wrk_pid=

  first=$(counters 1) || exit 1
  second=$(counters 2) || exit 1
  user=$(awk -F, '$3 ~ /^instructions:u/ { print $1 }' "$tmp/perf.out")
  kernel=$(awk -F, '$3 ~ /^instructions:k/ { print $1 }' "$tmp/perf.out")
  req_s=$(awk '/^Requests\/sec:/ { printf "%.0f", $2 }' "$tmp/wrk.out")
  awk -v n="$n" -v req_s="$req_s" -v user="$user" -v kernel="$kernel" \
    -v server_ticks="$((server_after - server_before))" -v window="$window" -v threads="$threads" \
    -v wrk_ticks="$((wrk_after - wrk_before))" -v hz="$(getconf CLK_TCK)" \
    -v first="$first" -v second="$second" \
    'function get(line, key,   i, f, kv) {
       split(line, f, " ")
       for (i in f) {
         split(f[i], kv, "=")
         if (kv[1] == key) return kv[2]
       }
     }
     BEGIN {
       reqs = get(second, "requests") - get(first, "requests")
       if (reqs <= 0) { print "scale.sh: no request in the window" > "/dev/stderr"; exit 1 }
       split(get(first, "per_domain"), a, "/")
       m = split(get(second, "per_domain"), b, "/")
       spread = ""
       for (i = 1; i <= m; i++)
         spread = spread (i > 1 ? "/" : "") sprintf("%.0f", 100 * (b[i] - a[i]) / reqs)
       printf "domains=%d req_s=%s req_s_domain=%.0f", n, req_s, req_s / n
       printf " server_util=%.2f wrk_util=%.2f", server_ticks / hz / window / n, wrk_ticks / hz / window / threads
       printf " user_instr=%.0f kernel_instr=%.0f", user / reqs, kernel / reqs
       printf " minor_words=%.1f promoted_words=%.2f", (get(second, "minor_words") - get(first, "minor_words")) / reqs, (get(second, "promoted_words") - get(first, "promoted_words")) / reqs
       printf " minor_gcs=%.2f", 1000 * (get(second, "minor_collections") - get(first, "minor_collections")) / reqs
       printf " major_gcs=%.1f", 1e6 * (get(second, "major_collections") - get(first, "major_collections")) / reqs
       printf " requests=%d spread=%s\n", reqs, spread
     }' || exit 1
  grep -E 'Socket errors|Non-2xx' "$tmp/wrk.out" >&2
  stop_server
  grep -v '^vs-counters ' "$tmp/server.err" >&2
}

echo "server=$server timeouts=$timeouts path=$path conn=$conn"
for n in "${domain_counts[@]}"; do
  run "$n"
done
exit 0
