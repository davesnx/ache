-- wrk.init builds the request with wrk.format after it calls init; returning
-- the raw bytes from wrk.format keeps the request static (no Lua call per
-- request) while allowing pipelined and chunked requests.
function init(args)
  local file = assert(io.open(args[1], "rb"))
  local raw = file:read("*a")
  file:close()
  wrk.format = function() return raw end
end

function done(summary, latency, requests)
  local e = summary.errors
  io.write(string.format(
    '{"requests":%d,"duration_us":%d,"bytes":%d,' ..
    '"errors":{"connect":%d,"read":%d,"write":%d,"status":%d,"timeout":%d},' ..
    '"latency_us":{"p50":%d,"p90":%d},' ..
    '"latency_unreliable_us":{"min":%d,"mean":%.1f,"stdev":%.1f,"max":%d,' ..
    '"p99":%d,"p999":%d}}\n',
    summary.requests, summary.duration, summary.bytes,
    e.connect, e.read, e.write, e.status, e.timeout,
    latency:percentile(50), latency:percentile(90),
    latency.min, latency.mean, latency.stdev, latency.max,
    latency:percentile(99), latency:percentile(99.9)))
end
