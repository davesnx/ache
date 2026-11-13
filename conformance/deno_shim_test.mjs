import "./deno_shim.mjs";
import * as net from "node:net";
import { spawn } from "node:child_process";
import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";

const self = fileURLToPath(import.meta.url);
const shim = new URL("./deno_shim.mjs", import.meta.url).href;
const [mode, ...rest] = process.argv.slice(2);

function listen(onSocket) {
  return new Promise((resolve) => {
    const sockets = new Set();
    const server = net.createServer((s) => {
      sockets.add(s);
      s.on("error", () => {});
      onSocket(s);
    });
    server.listen(0, "127.0.0.1", () =>
      resolve({
        port: server.address().port,
        close: () => {
          for (const s of sockets) s.destroy();
          server.close();
        },
      }),
    );
  });
}

function child(args, waitMs) {
  return new Promise((resolve) => {
    const p = spawn(process.execPath, ["--import", shim, self, ...args], {
      stdio: ["ignore", "pipe", "inherit"],
    });
    let out = "";
    p.stdout.on("data", (d) => (out += d));
    const timer = setTimeout(() => p.kill("SIGKILL"), waitMs);
    p.on("exit", (code, signal) => {
      clearTimeout(timer);
      resolve({ code, signal, out });
    });
  });
}

const settled = (promise, ms) =>
  Promise.race([
    promise.then(
      (v) => ({ state: "resolved", v }),
      (e) => ({ state: "rejected", e }),
    ),
    new Promise((r) => setTimeout(() => r({ state: "pending" }), ms)),
  ]);

const tests = [
  ["read returns the bytes received, at most buffer.length", async () => {
    const srv = await listen((s) => s.write("0123456789"));
    const conn = await Deno.connect({ hostname: "127.0.0.1", port: srv.port });
    await new Promise((r) => setTimeout(r, 50));
    const a = new Uint8Array(4);
    assert.equal(await conn.read(a), 4);
    assert.equal(new TextDecoder().decode(a), "0123");
    const b = new Uint8Array(1024);
    assert.equal(await conn.read(b), 6);
    assert.equal(new TextDecoder().decode(b.subarray(0, 6)), "456789");
    conn.close();
    srv.close();
  }],
  ["read resolves null at EOF, after the data", async () => {
    const srv = await listen((s) => s.end("bye"));
    const conn = await Deno.connect({ hostname: "127.0.0.1", port: srv.port });
    const b = new Uint8Array(1024);
    assert.equal(await conn.read(b), 3);
    assert.equal(await conn.read(b), null);
    assert.equal(await conn.read(b), null);
    conn.close();
    srv.close();
  }],
  ["write resolves to the byte count and the peer gets the bytes", async () => {
    let got = "";
    let done;
    const received = new Promise((r) => (done = r));
    const srv = await listen((s) =>
      s.on("data", (d) => {
        got += d;
        if (got.length >= 5) done();
      }),
    );
    const conn = await Deno.connect({ hostname: "127.0.0.1", port: srv.port });
    assert.equal(await conn.write(new TextEncoder().encode("hello")), 5);
    await received;
    assert.equal(got, "hello");
    conn.close();
    srv.close();
  }],
  ["read with no data stays pending; close rejects it; second close throws", async () => {
    const srv = await listen(() => {});
    const conn = await Deno.connect({ hostname: "127.0.0.1", port: srv.port });
    const read = conn.read(new Uint8Array(1024));
    assert.equal((await settled(read, 600)).state, "pending");
    conn.close();
    const r = await settled(read, 100);
    assert.equal(r.state, "rejected");
    assert.equal(r.e.name, "BadResource");
    assert.throws(() => conn.close(), { name: "BadResource" });
    srv.close();
  }],
  ["connection refused rejects connect with ConnectionRefused", async () => {
    const srv = await listen(() => {});
    const port = srv.port;
    srv.close();
    await assert.rejects(Deno.connect({ hostname: "127.0.0.1", port }), {
      name: "ConnectionRefused",
    });
  }],
  ["reset rejects the pending read with ConnectionReset", async () => {
    const srv = await listen((s) => setTimeout(() => s.resetAndDestroy(), 50));
    const conn = await Deno.connect({ hostname: "127.0.0.1", port: srv.port });
    await assert.rejects(conn.read(new Uint8Array(1024)), {
      name: "ConnectionReset",
    });
    srv.close();
  }],
  ["reset before read rejects the next read with ConnectionReset", async () => {
    const srv = await listen((s) => setTimeout(() => s.resetAndDestroy(), 20));
    const conn = await Deno.connect({ hostname: "127.0.0.1", port: srv.port });
    await new Promise((r) => setTimeout(r, 150));
    await assert.rejects(conn.read(new Uint8Array(1024)), {
      name: "ConnectionReset",
    });
    srv.close();
  }],
  ["Deno.args are the script arguments and Deno.exit sets the exit code", async () => {
    const r = await child(["child-args", "a b", "7"], 5000);
    assert.equal(r.code, 7);
    assert.deepEqual(JSON.parse(r.out), ["child-args", "a b", "7"]);
    const z = await child(["child-args", "x", "0"], 5000);
    assert.equal(z.code, 0);
  }],
  ["an open idle connection does not keep the process alive", async () => {
    const srv = await listen(() => {});
    const r = await child(["child-idle", String(srv.port)], 5000);
    srv.close();
    assert.equal(r.signal, null);
    assert.equal(r.code, 0);
  }],
  ["a pending read keeps the process alive", async () => {
    const srv = await listen(() => {});
    const r = await child(["child-pending", String(srv.port)], 1000);
    srv.close();
    assert.equal(r.signal, "SIGKILL");
  }],
];

async function main() {
  let failed = 0;
  for (const [name, fn] of tests) {
    try {
      await fn();
      console.log(`ok - ${name}`);
    } catch (e) {
      failed++;
      console.log(`not ok - ${name}\n  ${e && e.stack ? e.stack : e}`);
    }
  }
  console.log(`${tests.length - failed}/${tests.length} shim checks passed`);
  process.exit(failed === 0 ? 0 : 1);
}

if (mode === "child-args") {
  console.log(JSON.stringify(Deno.args));
  Deno.exit(Number(rest[1]));
} else if (mode === "child-idle") {
  const conn = await Deno.connect({ hostname: "127.0.0.1", port: Number(rest[0]) });
  await conn.write(new TextEncoder().encode("x"));
} else if (mode === "child-pending") {
  const conn = await Deno.connect({ hostname: "127.0.0.1", port: Number(rest[0]) });
  conn.read(new Uint8Array(16)).catch(() => {});
} else {
  await main();
}
