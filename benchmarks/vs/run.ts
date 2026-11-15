import { spawn, spawnSync, type ChildProcess } from "node:child_process";
import { createHash } from "node:crypto";
import * as fs from "node:fs";
import * as net from "node:net";
import * as os from "node:os";
import * as path from "node:path";
import { parseArgs } from "node:util";

type Runtime = "lwt" | "eio" | "miou";
type Lib = "ache" | "httpun" | "httpaf" | "httpcats";
type EioBackend = "linux" | "posix";

interface Server {
  id: string;
  lib: Lib;
  runtime: Runtime;
  exe: string;
}

interface Buffers {
  read: number;
  requestBody: number;
  response: number;
  responseBody: number;
}

interface Cell {
  id: string;
  axis: string;
  label: string;
  method: "GET" | "POST";
  target: string;
  fields: number;
  body: number;
  chunked: boolean;
  connections: number;
  pipeline: number;
  close: boolean;
  buffers: Buffers;
  eioBackend: EioBackend | null;
  runtimes: Runtime[];
  ocamlrunparam: string | null;
  note: string | null;
}

interface Settings {
  profile: string;
  reps: number;
  duration: number;
  warmup: number;
  threads: number;
  timeout: number;
  port: number;
  serverCpu: number | null;
  clientCpus: number[] | null;
  only: string;
  servers: string;
  lwtMaxConnections: number;
  maxSiblingBusy: number;
  retries: number;
  core: boolean;
  coreReps: number;
  coreTime: number;
}

interface WrkResult {
  requests: number;
  duration_us: number;
  bytes: number;
  errors: { connect: number; read: number; write: number; status: number; timeout: number };
  latency_us: { p50: number; p90: number };
  latency_unreliable_us: { min: number; mean: number; stdev: number; max: number; p99: number; p999: number };
}

interface Metrics {
  rps: number;
  response_mbps: number;
  request_mbps: number;
  p50_us: number;
  p90_us: number;
  cpu_s: number;
  cpu_pct: number;
  cpu_us_per_req: number;
  minor_words_per_req: number;
  major_collections: number;
  rss_peak_kb: number;
  involuntary_switches: number;
  open_fds: number;
  sibling_busy_pct: number;
  l3_busy_cpus: number;
}

interface Wire {
  status: number;
  head: string;
  raw_bytes: number;
  body_bytes: number;
  sha256: string;
  closed: boolean;
  close_headers: number;
  adds_close_header: boolean | null;
  pipeline_probe: { requests: number; replies: number[] }[] | null;
}

interface Run {
  cell: string;
  server: string;
  rep: number;
  started: string;
  ok: boolean;
  skipped: boolean;
  reason: string | null;
  wire: Wire | null;
  wrk: WrkResult | null;
  metrics: Metrics | null;
  discarded: { rps: number; sibling_busy_pct: number }[];
  sibling_flagged: boolean;
  server_stderr: string;
}

interface Cpus {
  server: number;
  clients: number[];
  note: string;
}

interface Environment {
  host: string;
  cpu_model: string;
  cpus: number;
  kernel: string;
  node: string;
  wrk: string;
  ocaml: string;
  findlib_destdir: string;
  packages: Record<string, string>;
  git_commit: string;
  git_dirty: boolean;
  binaries_built: string;
  newest_source: string;
  binaries_stale: boolean;
  governor: string;
  clk_tck: number;
  cpu_choice: string;
  loadavg_start: number[];
  loadavg_end: number[] | null;
}

interface Results {
  format: 1;
  started: string;
  finished: string | null;
  settings: Settings;
  environment: Environment;
  servers: Server[];
  cells: Cell[];
  cell_cpus: Record<string, Cpus>;
  core: unknown;
  runs: Run[];
}

const here = path.dirname(path.resolve(process.argv[1] ?? "benchmarks/vs/run.ts"));
const root = path.resolve(here, "..", "..");
const buildDir = path.join(root, "_build", "default", "benchmarks", "vs");
const luaScript = path.join(here, "wrk.lua");

const SERVERS: Server[] = [
  { id: "ache-lwt", lib: "ache", runtime: "lwt", exe: "server_ache_lwt.exe" },
  { id: "httpun-lwt", lib: "httpun", runtime: "lwt", exe: "server_httpun_lwt.exe" },
  { id: "httpaf-lwt", lib: "httpaf", runtime: "lwt", exe: "server_httpaf_lwt.exe" },
  { id: "ache-eio", lib: "ache", runtime: "eio", exe: "server_ache_eio.exe" },
  { id: "httpun-eio", lib: "httpun", runtime: "eio", exe: "server_httpun_eio.exe" },
  { id: "httpcats-miou", lib: "httpcats", runtime: "miou", exe: "server_httpcats_miou.exe" },
];

const DEFAULT_BUFFERS: Buffers = { read: 4096, requestBody: 4096, response: 1024, responseBody: 4096 };
const KiB = 1024;
const MiB = 1024 * KiB;

const BASE: Cell = {
  id: "base",
  axis: "base",
  label: "GET /fixed/12, 64 connections",
  method: "GET",
  target: "/fixed/12",
  fields: 1,
  body: 0,
  chunked: false,
  connections: 64,
  pipeline: 1,
  close: false,
  buffers: DEFAULT_BUFFERS,
  eioBackend: null,
  runtimes: ["lwt", "eio", "miou"],
  ocamlrunparam: null,
  note: null,
};

function cell(id: string, axis: string, label: string, over: Partial<Cell>): Cell {
  return { ...BASE, id, axis, label, ...over };
}

function uniformBuffers(size: number): Buffers {
  return { read: size, requestBody: size, response: size, responseBody: size };
}

function matrix(): Cell[] {
  const cells: Cell[] = [BASE];
  for (const c of [1, 16, 256, 1024]) cells.push(cell(`c${c}`, "load", `${c} connections`, { connections: c }));
  cells.push(cell("pipeline16", "load", "pipelining depth 16", { pipeline: 16 }));
  cells.push(
    cell("close", "load", "Connection: close on each request, GET /close/12", {
      target: "/close/12",
      close: true,
      note: "The handler sets connection: close in the response for every server, so that wrk closes after each response. Without it, wrk reuses a connection that httpaf and httpun close without that header, and counts a read error per request; the wire-differences section says which servers add the header by themselves.",
    }),
  );
  for (const [name, size] of [["0", 0], ["1k", KiB], ["16k", 16 * KiB], ["256k", 256 * KiB], ["4m", 4 * MiB]] as const)
    cells.push(cell(`fixed-${name}`, "response size", `GET /fixed/${size}`, { target: `/fixed/${size}` }));
  cells.push(cell("stream-1kx16", "streaming", "chunked, 1 KiB x 16", { target: `/stream/${KiB}/16` }));
  cells.push(cell("stream-16kx16", "streaming", "chunked, 16 KiB x 16", { target: `/stream/${16 * KiB}/16` }));
  cells.push(cell("stream-64kx64", "streaming", "chunked, 64 KiB x 64", { target: `/stream/${64 * KiB}/64` }));
  cells.push(cell("streamcl-16kx16", "streaming", "content-length, 16 KiB x 16", { target: `/streamcl/${16 * KiB}/16` }));
  const post = (id: string, label: string, target: string, body: number, chunked: boolean): Cell =>
    cell(id, "request body", label, { method: "POST", target, body, chunked });
  cells.push(post("sink-1k", "POST /sink 1 KiB", "/sink", KiB, false));
  cells.push(post("sink-64k", "POST /sink 64 KiB", "/sink", 64 * KiB, false));
  cells.push(post("sink-4m", "POST /sink 4 MiB", "/sink", 4 * MiB, false));
  cells.push(post("sink-64k-chunked", "POST /sink 64 KiB chunked", "/sink", 64 * KiB, true));
  cells.push(post("echo-64k", "POST /echo 64 KiB", "/echo", 64 * KiB, false));
  for (const n of [8, 32, 96]) cells.push(cell(`fields-${n}`, "request head", `${n} header fields`, { fields: n }));
  const longTarget = "/fixed/12?" + "a".repeat(4 * KiB - "/fixed/12?".length);
  cells.push(
    cell("target-4k", "request head", "4 KiB request target", {
      target: longTarget,
      note: "httpaf and httpun keep the whole request head in their read buffer (4096 bytes by default); this head is 4.1 KiB. target-4k-read16k is the same request with a 16 KiB read buffer.",
    }),
  );
  cells.push(
    cell("target-4k-read16k", "request head", "4 KiB request target, read buffer 16 KiB", {
      target: longTarget,
      buffers: { ...DEFAULT_BUFFERS, read: 16 * KiB },
    }),
  );
  const settingTargets: [string, Partial<Cell>][] = [
    ["base", {}],
    ["fixed-256k", { target: `/fixed/${256 * KiB}` }],
    ["sink-4m", { method: "POST", target: "/sink", body: 4 * MiB }],
  ];
  for (const [name, size] of [["1k", KiB], ["64k", 64 * KiB], ["256k", 256 * KiB]] as const)
    for (const [target, over] of settingTargets)
      cells.push(
        cell(`buf-${name}-${target}`, "buffers", `read, response and response-body buffers ${name}, ${target}`, {
          ...over,
          buffers: uniformBuffers(size),
        }),
      );
  for (const [target, over] of [
    ["base", {}],
    ["fields-32", { fields: 32 }],
    ["sink-64k", { method: "POST", target: "/sink", body: 64 * KiB }],
    ["stream-16kx16", { target: `/stream/${16 * KiB}/16` }],
  ] as [string, Partial<Cell>][])
    cells.push(cell(`gc-s4M-${target}`, "gc settings", `OCAMLRUNPARAM=s=4M, ${target}`, { ...over, ocamlrunparam: "s=4M" }));
  cells.push(cell("eio-posix-base", "eio backend", "EIO_BACKEND=posix, base", { eioBackend: "posix", runtimes: ["eio"] }));
  cells.push(
    cell("eio-posix-fixed-256k", "eio backend", `EIO_BACKEND=posix, GET /fixed/${256 * KiB}`, {
      eioBackend: "posix",
      runtimes: ["eio"],
      target: `/fixed/${256 * KiB}`,
    }),
  );
  return cells;
}

function expectedBody(c: Cell): Buffer {
  const parts = c.target.split("?")[0]!.split("/");
  if (parts[1] === "fixed" || parts[1] === "close") return Buffer.alloc(Number(parts[2]), "x");
  if (parts[1] === "stream" || parts[1] === "streamcl") return Buffer.alloc(Number(parts[2]) * Number(parts[3]), "x");
  if (parts[1] === "sink") return Buffer.from(String(c.body));
  if (parts[1] === "echo") return Buffer.alloc(c.body, "x");
  throw new Error(`no expected body for ${c.target}`);
}

function chunkedBody(size: number, chunk: number): Buffer {
  const parts: Buffer[] = [];
  for (let left = size; left > 0; left -= chunk) {
    const n = Math.min(chunk, left);
    parts.push(Buffer.from(`${n.toString(16)}\r\n`), Buffer.alloc(n, "x"), Buffer.from("\r\n"));
  }
  parts.push(Buffer.from("0\r\n\r\n"));
  return Buffer.concat(parts);
}

function requestBytes(c: Cell, port: number): Buffer {
  const lines = [`${c.method} ${c.target} HTTP/1.1`, `Host: 127.0.0.1:${port}`];
  for (let i = 1; i < c.fields; i++)
    lines.push(`X-Bench-${String(i).padStart(2, "0")}: value-${String(i).padStart(2, "0")}-abcdefghij`);
  if (c.close) lines.push("Connection: close");
  let body: Buffer = Buffer.alloc(0);
  if (c.method === "POST") {
    if (c.chunked) {
      lines.push("Transfer-Encoding: chunked");
      body = chunkedBody(c.body, 4 * KiB);
    } else {
      lines.push(`Content-Length: ${c.body}`);
      body = Buffer.alloc(c.body, "x");
    }
  }
  return Buffer.concat([Buffer.from(lines.join("\r\n") + "\r\n\r\n", "latin1"), body]);
}

const children = new Set<ChildProcess>();
const tempDirs = new Set<string>();

function killAll(): void {
  for (const dir of tempDirs) fs.rmSync(dir, { recursive: true, force: true });
  tempDirs.clear();
  for (const child of children) {
    try {
      child.kill("SIGKILL");
    } catch {}
  }
  children.clear();
}

process.on("exit", killAll);
for (const signal of ["SIGINT", "SIGTERM", "SIGHUP"] as const)
  process.on(signal, () => {
    killAll();
    process.stderr.write(`\n${signal}: stopped, servers killed\n`);
    process.exit(130);
  });
process.on("uncaughtException", (e) => {
  killAll();
  process.stderr.write(`${e.stack ?? String(e)}\n`);
  process.exit(1);
});

function sh(cmd: string, args: string[]): string {
  const r = spawnSync(cmd, args, { encoding: "utf8" });
  return r.status === 0 ? r.stdout.trim() : "";
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function shuffle<T>(items: T[]): T[] {
  const a = items.slice();
  for (let i = a.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [a[i], a[j]] = [a[j]!, a[i]!];
  }
  return a;
}

function median(xs: number[]): number {
  const a = xs.slice().sort((x, y) => x - y);
  const n = a.length;
  if (n === 0) return NaN;
  return n % 2 === 1 ? a[(n - 1) / 2]! : (a[n / 2 - 1]! + a[n / 2]!) / 2;
}

function parseCpuList(s: string): number[] {
  const out: number[] = [];
  for (const part of s.trim().split(",")) {
    if (part === "") continue;
    const [a, b] = part.split("-").map(Number);
    if (a === undefined || Number.isNaN(a)) throw new Error(`bad CPU list: ${s}`);
    for (let i = a; i <= (b ?? a); i++) out.push(i);
  }
  return out;
}

function cpuTimes(): Map<number, { idle: number; total: number }> {
  const m = new Map<number, { idle: number; total: number }>();
  for (const line of fs.readFileSync("/proc/stat", "utf8").split("\n")) {
    const match = /^cpu(\d+) (.*)$/.exec(line);
    if (!match) continue;
    const v = match[2]!.trim().split(/\s+/).map(Number);
    const idle = v[3]! + v[4]!;
    const total = v.slice(0, 8).reduce((x, y) => x + y, 0);
    m.set(Number(match[1]), { idle, total });
  }
  return m;
}

function cpuGroup(cpu: number, file: string): number[] {
  const f = `/sys/devices/system/cpu/cpu${cpu}/${file}`;
  return fs.existsSync(f) ? parseCpuList(fs.readFileSync(f, "utf8")) : [cpu];
}

function busy(a: Map<number, { idle: number; total: number }>, b: Map<number, { idle: number; total: number }>, cpus: number[]): number {
  let sum = 0;
  for (const cpu of cpus) {
    const t0 = a.get(cpu);
    const t1 = b.get(cpu);
    if (!t0 || !t1 || t1.total === t0.total) continue;
    sum += 1 - (t1.idle - t0.idle) / (t1.total - t0.total);
  }
  return sum;
}

async function pickCpus(clients: number): Promise<{ server: number; clients: number[]; note: string }> {
  const a = cpuTimes();
  await sleep(3000);
  const b = cpuTimes();
  const idle = new Map<number, number>();
  for (const [cpu, t1] of b) {
    const t0 = a.get(cpu);
    if (!t0) continue;
    const dt = t1.total - t0.total;
    idle.set(cpu, dt > 0 ? (t1.idle - t0.idle) / dt : 0);
  }
  const groups = new Map<string, { cpu: number; idle: number }[]>();
  const seen = new Set<number>();
  for (const cpu of [...idle.keys()].sort((x, y) => x - y)) {
    if (seen.has(cpu)) continue;
    const siblings = cpuGroup(cpu, "topology/thread_siblings_list");
    for (const s of siblings) seen.add(s);
    const key = cpuGroup(cpu, "cache/index3/shared_cpu_list").join(",");
    const cores = groups.get(key) ?? [];
    cores.push({ cpu: Math.min(...siblings), idle: Math.min(...siblings.map((s) => idle.get(s) ?? 0)) });
    groups.set(key, cores);
  }
  let best: { score: number; cores: { cpu: number; idle: number }[] } | null = null;
  for (const cores of groups.values()) {
    cores.sort((x, y) => y.idle - x.idle);
    const top = cores.slice(0, clients + 1);
    if (top.length < clients + 1) continue;
    const score = top.reduce((sum, c) => sum + c.idle, 0);
    if (!best || score > best.score) best = { score, cores: top };
  }
  if (!best) throw new Error(`no L3 cache group has ${clients + 1} cores; pass --server-cpu and --client-cpus`);
  const [server, ...rest] = best.cores;
  const note = best.cores.map((c) => `${c.cpu}:${(c.idle * 100).toFixed(0)}%idle`).join(" ");
  return { server: server!.cpu, clients: rest.map((c) => c.cpu), note };
}

interface Reply {
  status: number;
  head: string;
  body: Buffer;
  raw: Buffer;
}

function parseReply(buf: Buffer, ended: boolean): { reply: Reply; used: number } | null {
  const end = buf.indexOf("\r\n\r\n");
  if (end < 0) return null;
  const head = buf.subarray(0, end).toString("latin1");
  const lines = head.split("\r\n");
  const status = Number(lines[0]!.split(" ")[1]);
  const fields = new Map<string, string>();
  for (const line of lines.slice(1)) {
    const i = line.indexOf(":");
    fields.set(line.slice(0, i).trim().toLowerCase(), line.slice(i + 1).trim());
  }
  const start = end + 4;
  const te = fields.get("transfer-encoding");
  const cl = fields.get("content-length");
  if (te !== undefined && /chunked\s*$/i.test(te)) {
    const parts: Buffer[] = [];
    let at = start;
    for (;;) {
      const eol = buf.indexOf("\r\n", at);
      if (eol < 0) return null;
      const size = parseInt(buf.subarray(at, eol).toString("latin1"), 16);
      if (Number.isNaN(size)) throw new Error("bad chunk size");
      if (size === 0) {
        const trailerEnd = buf.indexOf("\r\n", eol + 2);
        if (trailerEnd < 0) return null;
        if (trailerEnd !== eol + 2) throw new Error("trailers are not expected");
        const used = trailerEnd + 2;
        return { reply: { status, head, body: Buffer.concat(parts), raw: buf.subarray(0, used) }, used };
      }
      if (buf.length < eol + 2 + size + 2) return null;
      parts.push(buf.subarray(eol + 2, eol + 2 + size));
      at = eol + 2 + size + 2;
    }
  }
  if (cl !== undefined) {
    const used = start + Number(cl);
    if (buf.length < used) return null;
    return { reply: { status, head, body: buf.subarray(start, used), raw: buf.subarray(0, used) }, used };
  }
  if (!ended) return null;
  return { reply: { status, head, body: buf.subarray(start), raw: buf }, used: buf.length };
}

async function exchange(
  port: number,
  requests: Buffer[],
  timeoutMs: number,
  lingerMs: number,
  together = false,
): Promise<{ replies: Reply[]; closed: boolean }> {
  return new Promise((resolve, reject) => {
    const socket = net.connect(port, "127.0.0.1");
    let buf = Buffer.alloc(0);
    let ended = false;
    const replies: Reply[] = [];
    const timer = setTimeout(() => {
      socket.destroy();
      reject(new Error(`timeout after ${replies.length} of ${requests.length} replies`));
    }, timeoutMs);
    const finish = (closed: boolean): void => {
      clearTimeout(timer);
      socket.destroy();
      resolve({ replies, closed });
    };
    const step = (): void => {
      for (;;) {
        if (replies.length === requests.length) {
          if (ended || lingerMs === 0) return finish(ended);
          setTimeout(() => finish(ended), lingerMs);
          return;
        }
        let parsed: { reply: Reply; used: number } | null;
        try {
          parsed = parseReply(buf, ended);
        } catch (e) {
          clearTimeout(timer);
          socket.destroy();
          return reject(e instanceof Error ? e : new Error(String(e)));
        }
        if (!parsed) {
          if (ended) {
            clearTimeout(timer);
            return reject(new Error(`connection closed after ${replies.length} of ${requests.length} replies`));
          }
          return;
        }
        replies.push(parsed.reply);
        buf = buf.subarray(parsed.used);
        const next = requests[replies.length];
        if (next && !together) socket.write(next);
      }
    };
    socket.on("connect", () => socket.write(together ? Buffer.concat(requests) : requests[0]!));
    socket.on("data", (d: Buffer) => {
      buf = Buffer.concat([buf, d]);
      step();
    });
    socket.on("end", () => {
      ended = true;
      step();
    });
    socket.on("error", (e) => {
      clearTimeout(timer);
      if (replies.length === requests.length) resolve({ replies, closed: true });
      else reject(e);
    });
  });
}

function portOpen(port: number): Promise<boolean> {
  return new Promise((resolve) => {
    const s = net.connect(port, "127.0.0.1");
    s.on("connect", () => {
      s.destroy();
      resolve(true);
    });
    s.on("error", () => resolve(false));
  });
}

async function portFree(port: number, waitMs: number): Promise<boolean> {
  for (let waited = 0; ; waited += 50) {
    if (!(await portOpen(port))) return true;
    if (waited >= waitMs) return false;
    await sleep(50);
  }
}

interface Started {
  child: ChildProcess;
  exited: Promise<number | null>;
  stderr: () => string;
}

async function startServer(s: Server, c: Cell, settings: Settings, cpus: Cpus): Promise<Started> {
  const args = [
    "-c",
    String(cpus.server),
    path.join(buildDir, s.exe),
    "--port",
    String(settings.port),
    "--read-buffer-size",
    String(c.buffers.read),
    "--request-body-buffer-size",
    String(c.buffers.requestBody),
    "--response-buffer-size",
    String(c.buffers.response),
    "--response-body-buffer-size",
    String(c.buffers.responseBody),
  ];
  const env = { ...process.env };
  delete env["EIO_BACKEND"];
  if (s.runtime === "eio" && c.eioBackend) env["EIO_BACKEND"] = c.eioBackend;
  delete env["OCAMLRUNPARAM"];
  if (c.ocamlrunparam !== null) env["OCAMLRUNPARAM"] = c.ocamlrunparam;
  if (!(await portFree(settings.port, 10000))) throw new Error(`port ${settings.port} is in use before ${s.id} starts`);
  const child = spawn("taskset", args, { env, stdio: ["ignore", "ignore", "pipe"] });
  children.add(child);
  let stderr = "";
  child.stderr?.on("data", (d: Buffer) => {
    if (stderr.length < 4096) stderr += d.toString();
  });
  const exited = new Promise<number | null>((resolve) =>
    child.on("exit", (code) => {
      children.delete(child);
      resolve(code);
    }),
  );
  let alive = true;
  void exited.then(() => {
    alive = false;
  });
  for (let i = 0; i < 200; i++) {
    if (!alive) throw new Error(`${s.id} exited at start: ${stderr.trim()}`);
    if (await portOpen(settings.port)) return { child, exited, stderr: () => stderr };
    await sleep(25);
  }
  child.kill("SIGKILL");
  throw new Error(`${s.id} did not listen on port ${settings.port}`);
}

async function stopServer(st: Started, port: number): Promise<number> {
  if (st.child.exitCode === null && st.child.signalCode === null) {
    st.child.kill("SIGTERM");
    const t = setTimeout(() => st.child.kill("SIGKILL"), 2000);
    await st.exited;
    clearTimeout(t);
  }
  const t0 = Date.now();
  if (!(await portFree(port, 10000))) throw new Error(`port ${port} still accepts 10 s after the server exited`);
  return Date.now() - t0;
}

function procStat(pid: number): { cpuTicks: number; rssPeakKb: number; involuntary: number } {
  const stat = fs.readFileSync(`/proc/${pid}/stat`, "utf8");
  const fields = stat.slice(stat.lastIndexOf(")") + 2).split(" ");
  const cpuTicks = Number(fields[11]) + Number(fields[12]);
  const status = fs.readFileSync(`/proc/${pid}/status`, "utf8");
  const hwm = /VmHWM:\s+(\d+) kB/.exec(status);
  const nv = /nonvoluntary_ctxt_switches:\s+(\d+)/.exec(status);
  return { cpuTicks, rssPeakKb: hwm ? Number(hwm[1]) : NaN, involuntary: nv ? Number(nv[1]) : NaN };
}

interface GcStat {
  minor_words: number;
  major_collections: number;
}

async function gcStat(port: number): Promise<GcStat> {
  const req = Buffer.from(`GET /gc HTTP/1.1\r\nHost: 127.0.0.1:${port}\r\n\r\n`);
  const { replies } = await exchange(port, [req], 5000, 0);
  const parsed = JSON.parse(replies[0]!.body.toString()) as GcStat;
  return parsed;
}

async function runWrk(
  settings: Settings,
  cpus: Cpus,
  connections: number,
  duration: number,
  requestFile: string,
): Promise<WrkResult> {
  const threads = Math.min(settings.threads, connections);
  const args = [
    "-c",
    cpus.clients.join(","),
    "wrk",
    "-t",
    String(threads),
    "-c",
    String(connections),
    "-d",
    `${duration}s`,
    "--timeout",
    `${settings.timeout}s`,
    "-s",
    luaScript,
    `http://127.0.0.1:${settings.port}/`,
    "--",
    requestFile,
  ];
  const child = spawn("taskset", args, { stdio: ["ignore", "pipe", "pipe"] });
  children.add(child);
  let out = "";
  let err = "";
  child.stdout?.on("data", (d: Buffer) => (out += d.toString()));
  child.stderr?.on("data", (d: Buffer) => (err += d.toString()));
  const code = await new Promise<number | null>((resolve) =>
    child.on("exit", (c) => {
      children.delete(child);
      resolve(c);
    }),
  );
  const line = out
    .split("\n")
    .reverse()
    .find((l) => l.startsWith("{"));
  if (code !== 0 || !line) throw new Error(`wrk failed (exit ${code}): ${err.trim() || out.trim()}`);
  return JSON.parse(line) as WrkResult;
}

function wrkFailure(w: WrkResult): string | null {
  const e = w.errors;
  const bad = Object.entries(e).filter(([, v]) => v > 0);
  if (bad.length > 0) return `wrk errors: ${bad.map(([k, v]) => `${k} ${v}`).join(", ")} (of ${w.requests} responses)`;
  if (w.requests === 0) return "wrk got no response";
  return null;
}

async function gate(c: Cell, port: number): Promise<Wire> {
  const req = requestBytes(c, port);
  const expected = expectedBody(c);
  const requests = c.close ? [req] : [req, req];
  const { replies, closed } = await exchange(port, requests, 10000, 200);
  for (const r of replies) {
    if (r.status !== 200) throw new Error(`status ${r.status}, head ${JSON.stringify(r.head)}`);
    if (!r.body.equals(expected))
      throw new Error(`body of ${r.body.length} bytes, expected ${expected.length} bytes`);
  }
  if (c.close && !closed) throw new Error("connection still open after a Connection: close request");
  if (!c.close && closed) throw new Error("connection closed after a keep-alive request");
  const first = replies[0]!;
  const closeHeaders = first.head.split("\r\n").filter((l) => /^connection:\s*close\s*$/i.test(l)).length;
  if (c.close && closeHeaders !== 1) throw new Error(`${closeHeaders} connection: close fields in the response, expected 1`);
  let addsClose: boolean | null = null;
  if (c.close) {
    const probe = requestBytes({ ...c, target: "/fixed/12" }, port);
    const { replies: plain } = await exchange(port, [probe], 10000, 0);
    addsClose = /\r\nconnection:\s*close/i.test(plain[0]!.head);
  }
  return {
    status: first.status,
    head: first.head,
    raw_bytes: first.raw.length,
    body_bytes: first.body.length,
    sha256: createHash("sha256").update(first.raw).digest("hex"),
    closed,
    close_headers: closeHeaders,
    adds_close_header: addsClose,
    pipeline_probe: null,
  };
}

function countReplies(port: number, data: Buffer, want: number, timeoutMs: number): Promise<number> {
  return new Promise((resolve) => {
    const socket = net.connect(port, "127.0.0.1");
    let buf = Buffer.alloc(0);
    let count = 0;
    const done = (): void => {
      clearTimeout(timer);
      socket.destroy();
      resolve(count);
    };
    const timer = setTimeout(done, timeoutMs);
    socket.on("connect", () => socket.write(data));
    socket.on("data", (d: Buffer) => {
      buf = Buffer.concat([buf, d]);
      for (;;) {
        let parsed: { reply: Reply; used: number } | null = null;
        try {
          parsed = parseReply(buf, false);
        } catch {
          return done();
        }
        if (!parsed) break;
        count++;
        buf = buf.subarray(parsed.used);
      }
      if (count >= want) done();
    });
    socket.on("end", done);
    socket.on("error", done);
  });
}

async function pipelineProbe(c: Cell, port: number): Promise<{ requests: number; replies: number[] }[]> {
  const req = requestBytes(c, port);
  const out: { requests: number; replies: number[] }[] = [];
  for (const n of [2, c.pipeline]) {
    const replies: number[] = [];
    for (let i = 0; i < 3; i++)
      replies.push(await countReplies(port, Buffer.concat(Array.from({ length: n }, () => req)), n, 2000));
    out.push({ requests: n, replies });
  }
  return out;
}

async function runOne(c: Cell, s: Server, rep: number, settings: Settings, cpus: Cpus, tmp: string): Promise<Run> {
  const run: Run = {
    cell: c.id,
    server: s.id,
    rep,
    started: new Date().toISOString(),
    ok: false,
    skipped: false,
    reason: null,
    wire: null,
    wrk: null,
    metrics: null,
    discarded: [],
    sibling_flagged: false,
    server_stderr: "",
  };
  const single = requestBytes(c, settings.port);
  const requestFile = path.join(tmp, `${c.id}.req`);
  fs.writeFileSync(requestFile, Buffer.concat(Array.from({ length: c.pipeline }, () => single)));
  let st: Started | null = null;
  let stage = "start";
  try {
    st = await startServer(s, c, settings, cpus);
    stage = "gate";
    const pid = st.child.pid!;
    run.wire = await gate(c, settings.port);
    if (c.pipeline > 1) {
      const probe = await pipelineProbe(c, settings.port);
      run.wire.pipeline_probe = probe;
      if (probe.some((p) => p.replies.some((n) => n < p.requests)))
        throw new Error(
          `requests sent in one socket write get fewer replies: ${probe.map((p) => `${p.replies.join(", ")} replies to ${p.requests}`).join("; ")} (3 tries each, 2 s wait)`,
        );
    }
    stage = "warm-up";
    if (settings.warmup > 0) await runWrk(settings, cpus, c.connections, settings.warmup, requestFile);
    stage = "GET /gc before the measured run";
    const gc0 = await gcStat(settings.port);
    stage = "measured run";
    const p0 = procStat(pid);
    const s0 = cpuTimes();
    const w = await runWrk(settings, cpus, c.connections, settings.duration, requestFile);
    const s1 = cpuTimes();
    const p1 = procStat(pid);
    const openFds = fs.readdirSync(`/proc/${pid}/fd`).length;
    const siblings = cpuGroup(cpus.server, "topology/thread_siblings_list").filter((x) => x !== cpus.server);
    const l3 = cpuGroup(cpus.server, "cache/index3/shared_cpu_list").filter(
      (x) => x !== cpus.server && !cpus.clients.includes(x),
    );
    stage = "GET /gc after the measured run";
    const gc1 = await gcStat(settings.port).catch(() => null);
    run.wrk = w;
    const seconds = w.duration_us / 1e6;
    const cpu = (p1.cpuTicks - p0.cpuTicks) / clkTck;
    run.metrics = {
      rps: w.requests / seconds,
      response_mbps: w.bytes / seconds / 1e6,
      request_mbps: (w.requests * single.length) / seconds / 1e6,
      p50_us: w.latency_us.p50,
      p90_us: w.latency_us.p90,
      cpu_s: cpu,
      cpu_pct: (100 * cpu) / seconds,
      cpu_us_per_req: w.requests > 0 ? (cpu * 1e6) / w.requests : NaN,
      minor_words_per_req: gc1 && w.requests > 0 ? (gc1.minor_words - gc0.minor_words) / w.requests : NaN,
      major_collections: gc1 ? gc1.major_collections - gc0.major_collections : NaN,
      rss_peak_kb: p1.rssPeakKb,
      involuntary_switches: p1.involuntary - p0.involuntary,
      open_fds: openFds,
      sibling_busy_pct: 100 * busy(s0, s1, siblings),
      l3_busy_cpus: busy(s0, s1, l3),
    };
    const wrkFailed = wrkFailure(w);
    const failure =
      (wrkFailed && c.close && run.wire && !/\r\nconnection: *close/i.test(run.wire.head)
        ? `${wrkFailed}; the response has no Connection: close`
        : wrkFailed) ??
      (gc1 ? null : "the server stopped answering after the measured run; its wrk numbers are kept here");
    run.ok = failure === null;
    run.reason = failure;
  } catch (e) {
    run.ok = false;
    run.reason = `${stage}: ${e instanceof Error ? e.message : String(e)}`;
    if (stage === "gate" && c.note) run.reason += ` (cell note: ${c.note})`;
  } finally {
    if (st) {
      try {
        await stopServer(st, settings.port);
      } catch (e) {
        run.ok = false;
        run.reason = `${run.reason ?? ""}; ${e instanceof Error ? e.message : String(e)}`;
      }
      run.server_stderr = st.stderr().trim();
      if (run.server_stderr && !run.ok) run.reason = `${run.reason ?? ""}; server stderr: ${run.server_stderr.slice(0, 300)}`;
    }
  }
  return run;
}

let clkTck = 100;

function newestMtime(dir: string, keep: (file: string) => boolean): { file: string; ms: number } {
  let best = { file: "", ms: 0 };
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (entry.name.startsWith(".") || entry.name === "_build" || entry.name === "_opam" || entry.name === "node_modules") continue;
      const sub = newestMtime(full, keep);
      if (sub.ms > best.ms) best = sub;
    } else if (keep(entry.name)) {
      const ms = fs.statSync(full).mtimeMs;
      if (ms > best.ms) best = { file: full, ms };
    }
  }
  return best;
}

function environment(settings: Settings, cpuNote: string, servers: Server[]): Environment {
  const built = Math.min(...[...servers.map((s) => s.exe), "core_bench.exe"].map((exe) => {
    const f = path.join(buildDir, exe);
    return fs.existsSync(f) ? fs.statSync(f).mtimeMs : Infinity;
  }));
  const source = newestMtime(root, (f) => /\.(ml|mli)$|^dune(-project)?$/.test(f));
  const cpuinfo = fs.readFileSync("/proc/cpuinfo", "utf8");
  const model = /model name\s*:\s*(.*)/.exec(cpuinfo)?.[1] ?? "unknown";
  const packages: Record<string, string> = {};
  const destdir = sh("ocamlfind", ["printconf", "destdir"]);
  const switchPackages = path.join(destdir, "..", ".opam-switch", "packages");
  const installed = fs.existsSync(switchPackages) ? fs.readdirSync(switchPackages) : [];
  for (const p of ["httpaf", "httpaf-lwt-unix", "httpun", "httpun-lwt-unix", "httpun-eio", "httpcats", "h1", "miou", "gluten", "lwt", "eio", "eio_main", "parseff", "faraday", "angstrom", "bigstringaf"])
    packages[p] = installed.find((d) => d.startsWith(`${p}.`))?.slice(p.length + 1) ?? "unknown";
  const governorFile = `/sys/devices/system/cpu/cpu${settings.serverCpu ?? 0}/cpufreq/scaling_governor`;
  return {
    host: os.hostname(),
    cpu_model: model,
    cpus: os.cpus().length,
    kernel: `${os.type()} ${os.release()}`,
    node: process.version,
    wrk: sh("sh", ["-c", "wrk -v 2>&1 | head -1"]),
    ocaml: sh("ocamlfind", ["ocamlopt", "-version"]) || "unknown",
    findlib_destdir: destdir,
    packages,
    git_commit: sh("git", ["-C", root, "rev-parse", "HEAD"]),
    git_dirty: sh("git", ["-C", root, "status", "--porcelain"]) !== "",
    binaries_built: Number.isFinite(built) ? new Date(built).toISOString() : "missing",
    newest_source: `${path.relative(root, source.file)} ${new Date(source.ms).toISOString()}`,
    binaries_stale: !Number.isFinite(built) || built < source.ms,
    governor: fs.existsSync(governorFile) ? fs.readFileSync(governorFile, "utf8").trim() : "unknown",
    clk_tck: clkTck,
    cpu_choice: cpuNote,
    loadavg_start: os.loadavg(),
    loadavg_end: null,
  };
}

function runCore(settings: Settings, cpus: Cpus): unknown {
  const exe = path.join(buildDir, "core_bench.exe");
  process.stderr.write(`core: ${settings.coreReps} reps x ${settings.coreTime} s per scenario and library\n`);
  const r = spawnSync(
    "taskset",
    ["-c", String(cpus.server), exe, "--json", "--reps", String(settings.coreReps), "--time", String(settings.coreTime)],
    { encoding: "utf8", maxBuffer: 64 * MiB },
  );
  if (r.status !== 0) return { error: `core_bench exit ${r.status}: ${r.stderr.trim()}` };
  return JSON.parse(r.stdout) as unknown;
}

const PROFILES: Record<string, { reps: number; duration: number; warmup: number; coreReps: number; coreTime: number; maxSiblingBusy: number; retries: number }> = {
  quick: { reps: 1, duration: 2, warmup: 0, coreReps: 3, coreTime: 0.1, maxSiblingBusy: 20, retries: 0 },
  full: { reps: 5, duration: 5, warmup: 1, coreReps: 7, coreTime: 0.3, maxSiblingBusy: 20, retries: 3 },
};

function save(file: string, results: Results): void {
  fs.writeFileSync(file, JSON.stringify(results, null, 1) + "\n");
}

async function main(argv: string[]): Promise<void> {
  const { values, positionals } = parseArgs({
    args: argv,
    allowPositionals: true,
    options: {
      profile: { type: "string", default: "quick" },
      reps: { type: "string" },
      duration: { type: "string" },
      warmup: { type: "string" },
      threads: { type: "string", default: "4" },
      timeout: { type: "string", default: "10" },
      port: { type: "string", default: "18080" },
      "server-cpu": { type: "string" },
      "client-cpus": { type: "string" },
      only: { type: "string", default: "" },
      servers: { type: "string", default: "" },
      "lwt-max-connections": { type: "string", default: "512" },
      "max-sibling-busy": { type: "string" },
      retries: { type: "string" },
      "no-core": { type: "boolean", default: false },
      out: { type: "string" },
      help: { type: "boolean", default: false },
    },
  });
  const command = positionals[0] ?? "run";
  if (values.help) {
    process.stdout.write(usage);
    return;
  }
  if (command === "report") {
    const file = positionals[1];
    if (!file) throw new Error("usage: run.ts report <results.json>");
    const md = report(JSON.parse(fs.readFileSync(file, "utf8")) as Results);
    const out = values.out ?? file.replace(/\.json$/, "") + ".md";
    fs.writeFileSync(out, md);
    process.stderr.write(`wrote ${out}\n`);
    return;
  }
  if (command === "list") {
    for (const c of matrix()) process.stdout.write(`${c.id.padEnd(24)} ${c.axis.padEnd(14)} ${c.label}\n`);
    return;
  }
  if (command !== "run") throw new Error(`unknown command ${command}\n${usage}`);

  const profile = PROFILES[values.profile];
  if (!profile) throw new Error(`unknown profile ${values.profile}; use quick or full`);
  const int = (v: string | undefined, fallback: number, name: string): number => {
    const n = v === undefined ? fallback : Number(v);
    if (!Number.isInteger(n) || n < 0) throw new Error(`--${name} needs a non-negative integer, got ${v}`);
    return n;
  };
  const threads = int(values.threads, 4, "threads");
  const serverCpu = values["server-cpu"] !== undefined ? int(values["server-cpu"], 0, "server-cpu") : null;
  const clientCpus = values["client-cpus"] !== undefined ? parseCpuList(values["client-cpus"]) : null;
  if ((serverCpu === null) !== (clientCpus === null)) throw new Error("pass both --server-cpu and --client-cpus, or neither");
  if (serverCpu !== null && clientCpus!.includes(serverCpu)) throw new Error("the server CPU is also a client CPU");
  const cpuNote =
    serverCpu === null
      ? "picked before each cell (and before core) from 3 s of /proc/stat: the most idle cores of one L3 cache group"
      : "given by flags";
  const choose = async (): Promise<Cpus> => {
    if (serverCpu !== null) return { server: serverCpu, clients: clientCpus!, note: "given by flags" };
    const picked = await pickCpus(threads);
    return { server: picked.server, clients: picked.clients, note: picked.note };
  };
  const settings: Settings = {
    profile: values.profile,
    reps: int(values.reps, profile.reps, "reps"),
    duration: int(values.duration, profile.duration, "duration"),
    warmup: int(values.warmup, profile.warmup, "warmup"),
    threads,
    timeout: int(values.timeout, 10, "timeout"),
    port: int(values.port, 18080, "port"),
    serverCpu,
    clientCpus,
    only: values.only,
    servers: values.servers,
    lwtMaxConnections: int(values["lwt-max-connections"], 512, "lwt-max-connections"),
    maxSiblingBusy: int(values["max-sibling-busy"], profile.maxSiblingBusy, "max-sibling-busy"),
    retries: int(values.retries, profile.retries, "retries"),
    core: !values["no-core"],
    coreReps: profile.coreReps,
    coreTime: profile.coreTime,
  };
  if (settings.duration < 1) throw new Error("--duration must be at least 1 s");
  const cellRe = new RegExp(settings.only);
  const serverRe = new RegExp(settings.servers);
  const cells = matrix().filter((c) => cellRe.test(c.id));
  const servers = SERVERS.filter((s) => serverRe.test(s.id));
  if (cells.length === 0 || servers.length === 0) throw new Error("the filters select no cell or no server");
  for (const s of servers)
    if (!fs.existsSync(path.join(buildDir, s.exe)))
      throw new Error(`${s.exe} is not built; run make bench-deps, then build with --profile=release`);
  clkTck = Number(sh("getconf", ["CLK_TCK"])) || 100;

  const env = environment(settings, cpuNote, servers);
  if (env.binaries_stale)
    process.stderr.write(`warning: binaries (${env.binaries_built}) are older than ${env.newest_source}; the driver does not rebuild\n`);
  const started = new Date();
  const stamp = started.toISOString().replace(/[:.]/g, "-");
  const resultsDir = path.join(here, "results");
  fs.mkdirSync(resultsDir, { recursive: true });
  const file = values.out ?? path.join(resultsDir, `${settings.profile}-${stamp}.json`);
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "ache-bench-"));
  tempDirs.add(tmp);
  const results: Results = {
    format: 1,
    started: started.toISOString(),
    finished: null,
    settings,
    environment: env,
    servers,
    cells,
    cell_cpus: {},
    core: null,
    runs: [],
  };

  const plan: { c: Cell; s: Server; rep: number }[] = [];
  for (let rep = 1; rep <= settings.reps; rep++)
    for (const c of cells) for (const s of shuffle(servers)) plan.push({ c, s, rep });
  const active = plan.filter(({ c, s }) => skipReason(c, s, settings) === null).length;
  const perRun = settings.duration + settings.warmup + 2;
  const picks = serverCpu === null ? cells.length * 3.5 : 0;
  process.stderr.write(
    `${plan.length} runs (${active} active), about ${Math.ceil((active * perRun + picks) / 60)} min, at most ${Math.ceil((active * perRun * (1 + settings.retries) + picks) / 60)} min with retries\n${file}\n`,
  );
  if (settings.core) {
    const cpus = await choose();
    process.stderr.write(`core on CPU ${cpus.server}\n`);
    results.core = runCore(settings, cpus);
    save(file, results);
  }
  let i = 0;
  for (const { c, s, rep } of plan) {
    i++;
    const skip = skipReason(c, s, settings);
    let run: Run;
    if (skip !== null) {
      run = { cell: c.id, server: s.id, rep, started: new Date().toISOString(), ok: false, skipped: true, reason: skip, wire: null, wrk: null, metrics: null, discarded: [], sibling_flagged: false, server_stderr: "" };
    } else {
      let cpus = results.cell_cpus[c.id];
      if (!cpus) {
        cpus = await choose();
        results.cell_cpus[c.id] = cpus;
        process.stderr.write(`${c.id}: server CPU ${cpus.server}, wrk CPUs ${cpus.clients.join(",")}\n`);
      }
      const discarded: { rps: number; sibling_busy_pct: number }[] = [];
      for (;;) {
        run = await runOne(c, s, rep, settings, cpus, tmp);
        const m = run.metrics;
        if (!m || m.sibling_busy_pct <= settings.maxSiblingBusy || discarded.length >= settings.retries) break;
        discarded.push({ rps: m.rps, sibling_busy_pct: m.sibling_busy_pct });
        process.stderr.write(`  ${c.id} ${s.id}: sibling CPU ${m.sibling_busy_pct.toFixed(0)}% busy, run again\n`);
      }
      run.discarded = discarded;
      run.sibling_flagged = run.metrics !== null && run.metrics.sibling_busy_pct > settings.maxSiblingBusy;
      const m = run.metrics;
      process.stderr.write(
        `[${i}/${plan.length}] ${c.id} ${s.id} rep ${rep}: ` +
          (m ? `${m.rps.toFixed(0)} req/s, cpu ${m.cpu_pct.toFixed(0)}%, sibling ${m.sibling_busy_pct.toFixed(0)}%${run.sibling_flagged ? " (flagged)" : ""}` : "no metrics") +
          (run.ok ? "" : ` FAILED: ${run.reason}`) +
          "\n",
      );
    }
    results.runs.push(run);
    save(file, results);
  }
  results.finished = new Date().toISOString();
  env.loadavg_end = os.loadavg();
  save(file, results);
  fs.rmSync(tmp, { recursive: true, force: true });
  tempDirs.delete(tmp);
  const md = report(results);
  const mdFile = file.replace(/\.json$/, "") + ".md";
  fs.writeFileSync(mdFile, md);
  process.stderr.write(`wrote ${file}\nwrote ${mdFile}\n`);
}

function skipReason(c: Cell, s: Server, settings: Settings): string | null {
  if (!c.runtimes.includes(s.runtime)) return `cell applies to ${c.runtimes.join(", ")} only`;
  if (s.runtime === "lwt" && c.connections > settings.lwtMaxConnections)
    return `Lwt uses its select engine here (no libev), which fails above 1024 file descriptors; cap ${settings.lwtMaxConnections} connections`;
  return null;
}

function fmt(n: number, digits = 0): string {
  if (!Number.isFinite(n)) return "-";
  return n.toLocaleString("en-US", { maximumFractionDigits: digits, minimumFractionDigits: digits });
}

function escapeHead(head: string): string {
  return head.replace(/\r\n/g, "\\r\\n ");
}

function report(r: Results): string {
  const out: string[] = [];
  const e = r.environment;
  const s = r.settings;
  const discarded = r.runs.reduce((n, x) => n + x.discarded.length, 0);
  const flagged = r.runs.filter((x) => x.sibling_flagged).length;
  out.push(`# ache benchmark: ${s.profile} profile, ${r.started}`, "");
  out.push(
    `- CPU: ${e.cpu_model} (${e.cpus} logical CPUs), governor ${e.governor}`,
    `- Kernel: ${e.kernel}; OCaml ${e.ocaml} (${e.findlib_destdir}); Node ${e.node}; ${e.wrk}`,
    `- Packages: ${Object.entries(e.packages).map(([k, v]) => `${k} ${v}`).join(", ")}`,
    `- ache commit: ${e.git_commit}${e.git_dirty ? " (uncommitted changes)" : ""}; binaries built ${e.binaries_built}; newest source ${e.newest_source}${e.binaries_stale ? " — **binaries are older than the sources**" : ""}`,
    `- CPUs: ${e.cpu_choice}; the CPUs of each cell are in its detail section`,
    `- Load average at start ${e.loadavg_start.map((x) => x.toFixed(0)).join(" ")}, at end ${e.loadavg_end?.map((x) => x.toFixed(0)).join(" ") ?? "-"}`,
    `- ${s.reps} repetitions x ${s.duration} s, warm-up ${s.warmup} s, ${s.threads} wrk threads, finished ${r.finished ?? "no (partial)"}`,
    `- A run is done again, at most ${s.retries} times, when the server core's SMT sibling was more than ${s.maxSiblingBusy} % busy during the measured window: ${discarded} runs were done again. A run kept above that limit is flagged: ${flagged} runs.`,
    "",
    "**Ratio**: in every table, the ratio is the speed of that server or library divided by the speed of ache on the same runtime (requests/s over ache's requests/s; ache's ns over the library's ns). Above 1.00 means faster than ache.",
    "",
    "Values are medians over the passed runs. † marks a median server CPU below 90 % of one core: the server was not the limit. [p/n, f flagged] shows passed runs out of all runs and flagged runs, when not all runs are clean.",
    "",
  );
  const wordsCaveat =
    "Minor words do not count Bigstrings (allocated with malloc) or fiber and effect stacks; peak RSS and major collections show part of that cost.";

  const okRuns = (cell: string, server: string): Run[] =>
    r.runs.filter((x) => x.cell === cell && x.server === server && x.ok && x.metrics);
  const med = (runs: Run[], f: (m: Metrics) => number): number => median(runs.map((x) => f(x.metrics!)));
  const ache = (server: Server): string => `ache-${server.runtime}`;
  const us = (v: number): string => (v > 0 ? fmt(v / 1000, 2) : "-");

  const core = r.core as { results?: { scenario: string; lib: string; ns_median: number; ns_min: number; words_median: number }[]; error?: string } | null;
  if (core) {
    out.push("## In-process (core)", "");
    if (core.error) out.push(`core_bench failed: ${core.error}`, "");
    else if (core.results) {
      const libs = [...new Set(core.results.map((x) => x.lib))];
      const scen = [...new Set(core.results.map((x) => x.scenario))];
      out.push("ns per message, median (min), and the ratio (ache ns / library ns); minor words per message.", "");
      out.push(`| scenario | ${libs.map((l) => `${l} ns`).join(" | ")} | ${libs.map((l) => `${l} words`).join(" | ")} |`);
      out.push(`|---|${libs.map(() => "---:").join("|")}|${libs.map(() => "---:").join("|")}|`);
      for (const sc of scen) {
        const row = (lib: string) => core.results!.find((x) => x.scenario === sc && x.lib === lib);
        const base = row("ache");
        out.push(
          `| ${sc} | ${libs
            .map((l) => {
              const x = row(l);
              if (!x) return "-";
              const ratio = base && l !== "ache" ? ` (${(base.ns_median / x.ns_median).toFixed(2)})` : "";
              return `${fmt(x.ns_median)} (${fmt(x.ns_min)})${ratio}`;
            })
            .join(" | ")} | ${libs.map((l) => fmt(row(l)?.words_median ?? NaN, 1)).join(" | ")} |`,
        );
      }
      out.push("", wordsCaveat, "");
    }
  }

  const axes = [...new Set(r.cells.map((c) => c.axis))];
  out.push("## Summary: requests/s", "");
  for (const axis of axes) {
    const cells = r.cells.filter((c) => c.axis === axis || (c.id === "base" && axis !== "base"));
    out.push(`### ${axis}`, "");
    out.push(`| cell | ${r.servers.map((x) => x.id).join(" | ")} |`);
    out.push(`|---|${r.servers.map(() => "---:").join("|")}|`);
    for (const c of cells) {
      const cols = r.servers.map((srv) => {
        const all = r.runs.filter((x) => x.cell === c.id && x.server === srv.id);
        if (all.length === 0) return "-";
        if (all.every((x) => x.skipped)) return "skip";
        const runs = okRuns(c.id, srv.id);
        if (runs.length === 0) return "FAIL";
        const rps = med(runs, (m) => m.rps);
        const cpu = med(runs, (m) => m.cpu_pct);
        const baseRuns = okRuns(c.id, ache(srv));
        const ratio = srv.lib !== "ache" && baseRuns.length > 0 ? ` (${(rps / med(baseRuns, (m) => m.rps)).toFixed(2)})` : "";
        const nFlagged = all.filter((x) => x.sibling_flagged).length;
        const counts = runs.length < all.length || nFlagged > 0 ? ` [${runs.length}/${all.length}${nFlagged > 0 ? `, ${nFlagged} flagged` : ""}]` : "";
        return `${fmt(rps)}${ratio}${cpu < 90 ? " †" : ""}${counts}`;
      });
      out.push(`| ${c.id}: ${c.label} | ${cols.join(" | ")} |`);
    }
    out.push("");
  }

  out.push("## Details", "");
  out.push(
    "req/s median [min–max]; response and request MB/s; latency p50 and p90 from wrk (closed loop; wrk 4.1 corrects for coordinated omission by adding samples after a slow response, so its p99 and max are not reported, and a 0 percentile shows as -); server CPU % of one core; CPU µs and minor words per request; major collections in the measured window; peak RSS; open file descriptors of the server at the end of the measured run; involuntary context switches of the server (preempted); busy % of the server core's SMT sibling and busy CPUs among the other CPUs that share its L3 cache (other tenants of the machine), medians; flagged runs.",
    "",
    wordsCaveat,
    "",
  );
  for (const c of r.cells) {
    out.push(`### ${c.id}: ${c.label}`, "");
    const cpus = (r.cell_cpus ?? {})[c.id];
    out.push(
      `${c.note ? `${c.note}\n\n` : ""}${c.connections} connections${c.pipeline > 1 ? `, pipelining ${c.pipeline}` : ""}${c.close ? ", Connection: close" : ""}, ${c.fields} header fields, buffers read ${c.buffers.read} / response ${c.buffers.response} / response body ${c.buffers.responseBody}${c.eioBackend ? `, EIO_BACKEND=${c.eioBackend}` : ""}, OCAMLRUNPARAM ${c.ocamlrunparam ?? "unset"}${cpus ? `; server CPU ${cpus.server}, wrk CPUs ${cpus.clients.join(",")} (${cpus.note})` : ""}`,
      "",
    );
    out.push("| server | ok | req/s | resp MB/s | req MB/s | p50 ms | p90 ms | CPU % | CPU µs/req | words/req | major GCs | RSS MB | open fds | preempted | sibling % | L3 busy | flagged |");
    out.push("|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|");
    for (const srv of r.servers) {
      const all = r.runs.filter((x) => x.cell === c.id && x.server === srv.id);
      if (all.length === 0 || all.every((x) => x.skipped)) continue;
      const runs = okRuns(c.id, srv.id);
      const ok = `${runs.length}/${all.length}`;
      const nFlagged = all.filter((x) => x.sibling_flagged).length;
      if (runs.length === 0) {
        out.push(`| ${srv.id} | ${ok} |${" |".repeat(14)} ${nFlagged} |`);
        continue;
      }
      const rps = runs.map((x) => x.metrics!.rps);
      out.push(
        `| ${srv.id} | ${ok} | ${fmt(median(rps))} [${fmt(Math.min(...rps))}–${fmt(Math.max(...rps))}] | ${fmt(med(runs, (m) => m.response_mbps), 1)} | ${fmt(med(runs, (m) => m.request_mbps), 1)} | ${us(med(runs, (m) => m.p50_us))} | ${us(med(runs, (m) => m.p90_us))} | ${fmt(med(runs, (m) => m.cpu_pct))} | ${fmt(med(runs, (m) => m.cpu_us_per_req), 2)} | ${fmt(med(runs, (m) => m.minor_words_per_req), 0)} | ${fmt(med(runs, (m) => m.major_collections))} | ${fmt(med(runs, (m) => m.rss_peak_kb) / 1024, 1)} | ${fmt(med(runs, (m) => m.open_fds))} | ${fmt(med(runs, (m) => m.involuntary_switches))} | ${fmt(med(runs, (m) => m.sibling_busy_pct))} | ${fmt(med(runs, (m) => m.l3_busy_cpus), 1)} | ${nFlagged} |`,
      );
    }
    out.push("");
  }

  out.push("## Failed and skipped runs", "");
  const failed = r.runs.filter((x) => !x.ok);
  if (failed.length === 0) out.push("None.", "");
  else {
    out.push("| cell | server | rep | kind | req/s | reason |", "|---|---|---:|---|---:|---|");
    for (const x of failed)
      out.push(
        `| ${x.cell} | ${x.server} | ${x.rep} | ${x.skipped ? "skipped" : "failed"} | ${x.metrics ? fmt(x.metrics.rps) : ""} | ${(x.reason ?? "").replace(/\|/g, "\\|").replace(/\n/g, " ")} |`,
      );
    out.push("");
  }

  out.push("## Server stderr", "");
  const noisy = r.runs.filter((x) => x.server_stderr);
  if (noisy.length === 0) out.push("No server wrote to stderr.", "");
  else {
    out.push("Runs whose server wrote to stderr. Lwt servers log exceptions that reach Lwt.async_exception_hook and go on; Eio servers log exceptions from a connection fiber and go on.", "");
    out.push("| cell | server | rep | ok | first line | lines |", "|---|---|---:|---|---|---:|");
    for (const x of noisy) {
      const lines = x.server_stderr.split("\n");
      out.push(`| ${x.cell} | ${x.server} | ${x.rep} | ${x.ok ? "yes" : "no"} | ${lines[0]!.replace(/\|/g, "\\|").slice(0, 160)} | ${lines.length}${x.server_stderr.length >= 4096 ? "+" : ""} |`);
    }
    out.push("");
  }

  out.push("## Wire differences", "");
  out.push("The gate sends the cell's request before each run and records the first response. A cell is listed when the servers' response bytes differ.", "");
  let any = false;
  for (const c of r.cells) {
    const groups = new Map<string, { servers: Set<string>; wire: Wire }>();
    for (const x of r.runs) {
      if (x.cell !== c.id || !x.wire) continue;
      const g = groups.get(x.wire.sha256) ?? { servers: new Set<string>(), wire: x.wire };
      g.servers.add(x.server);
      groups.set(x.wire.sha256, g);
    }
    if (groups.size <= 1) continue;
    any = true;
    out.push(`### ${c.id}`, "");
    for (const g of groups.values())
      out.push(`- ${[...g.servers].join(", ")}: ${g.wire.raw_bytes} bytes, body ${g.wire.body_bytes} bytes, head \`${escapeHead(g.wire.head)}\``);
    out.push("");
  }
  if (!any) out.push("No cell: every server sent the same bytes in every cell where it passed the gate.", "");
  const adds = new Map<string, boolean>();
  for (const x of r.runs) if (x.wire && x.wire.adds_close_header !== null) adds.set(x.server, x.wire.adds_close_header);
  if (adds.size > 0) {
    const yes = [...adds].filter(([, v]) => v).map(([k]) => k);
    const no = [...adds].filter(([, v]) => !v).map(([k]) => k);
    out.push(
      `- Connection: close in the request, handler without a Connection field (GET /fixed/12): ${yes.length ? `${yes.join(", ")} add connection: close to the response` : "no server adds connection: close"}${no.length ? `; ${no.join(", ")} close the connection without it` : ""}.`,
    );
  }
  const probes = new Map<string, { requests: number; replies: number[] }[]>();
  for (const x of r.runs) if (x.wire?.pipeline_probe) probes.set(x.server, x.wire.pipeline_probe);
  if (probes.size > 0)
    out.push(
      `- Requests sent in one socket write (3 tries each, replies counted for 2 s): ${[...probes]
        .map(([k, v]) => `${k} ${v.map((p) => `${p.replies.join("/")} of ${p.requests}`).join(", ")}`)
        .join("; ")}.`,
    );
  out.push("");
  return out.join("\n");
}

const usage = `usage: node benchmarks/vs/run.ts [run|report <file.json>|list] [flags]

run flags:
  --profile quick|full        repetitions, duration and warm-up preset (quick)
  --reps N --duration S --warmup S   override the profile (whole seconds)
  --threads N                 wrk threads, one per client CPU (4)
  --server-cpu N              CPU for the server (default: picked before
                              each cell, the most idle cores of one L3
                              cache group for server and wrk)
  --client-cpus LIST          CPUs for wrk, e.g. 4,5,6,7 (picked)
  --port N                    server port on 127.0.0.1 (18080)
  --timeout S                 wrk request timeout (10)
  --only REGEX                cells whose id matches
  --servers REGEX             servers whose id matches
  --lwt-max-connections N     skip Lwt servers above N connections (512)
  --max-sibling-busy PCT      run again when the server core's SMT sibling was
                              busier than PCT %; a run kept above it is
                              flagged (20)
  --retries N                 at most N extra runs for that rule (quick 0,
                              full 3)
  --no-core                   do not run core_bench
  --out FILE                  results file (benchmarks/vs/results/<profile>-<time>.json)
`;

main(process.argv.slice(2)).catch((e: unknown) => {
  killAll();
  process.stderr.write(`${e instanceof Error ? e.message : String(e)}\n`);
  process.exit(1);
});
