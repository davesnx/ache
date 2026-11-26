// META: title=ache HTTP and WebSocket server interoperability
// META: global=window
// META: timeout=long

// Runs against conformance/ws_echo_$RUNTIME.exe, started by conformance/wpt.sh,
// which replaces the port below with the server's port.

"use strict";

const acheHost = "127.0.0.1:8090";
const acheBase = `http://${acheHost}`;

promise_test(async () => {
  const response = await fetch(`${acheBase}/status`);
  assert_equals(response.status, 200);
  assert_equals(response.headers.get("content-type"), "application/json");
  assert_equals(response.headers.get("content-length"), "15");
  assert_object_equals(await response.json(), { status: "ok" });
}, "GET returns a status, headers and a JSON body");

promise_test(async () => {
  const value = "héllø wörld ✓ 😀";
  const response = await fetch(`${acheBase}/echo`, {
    method: "POST",
    headers: { "Content-Type": "text/plain;charset=UTF-8" },
    body: value,
  });
  assert_equals(response.status, 200);
  assert_equals(response.headers.get("x-ache-method"), "POST");
  assert_equals(
    response.headers.get("content-length"),
    String(new TextEncoder().encode(value).length),
  );
  assert_equals(await response.text(), value);
}, "A UTF-8 POST body comes back unchanged");

promise_test(async () => {
  const response = await fetch(`${acheBase}/status`, { method: "HEAD" });
  assert_equals(response.status, 200);
  assert_equals(response.headers.get("content-length"), "15");
  assert_equals(await response.text(), "");
}, "HEAD has a content-length and no body");

promise_test(async () => {
  const response = await fetch(`${acheBase}/redirect`);
  assert_true(response.redirected, "redirected");
  assert_equals(response.url, `${acheBase}/status`);
  assert_equals(response.status, 200);
  assert_object_equals(await response.json(), { status: "ok" });
}, "fetch follows a 302 redirect");

promise_test(async () => {
  const response = await fetch(`${acheBase}/status`, { mode: "cors" });
  assert_equals(response.type, "cors");
  assert_equals(response.status, 200);
}, "A simple cross-origin request is allowed");

promise_test(async () => {
  const id = `id-${Date.now()}-${Math.random().toString(16).slice(2)}`;
  const response = await fetch(`${acheBase}/cors?${id}`, {
    method: "PUT",
    headers: { "X-Ache-Test": "preflighted" },
  });
  assert_equals(response.status, 200);
  assert_object_equals(await response.json(), {
    preflight: { method: "PUT", headers: "x-ache-test" },
    "x-ache-test": "preflighted",
  });
}, "A preflighted cross-origin request sends OPTIONS first, then the request");

promise_test(async () => {
  const count = 20;
  const responses = await Promise.all(
    Array.from({ length: count }, (_, i) =>
      fetch(`${acheBase}/echo?${i}`, { method: "POST", body: `request ${i}` })
    ),
  );
  const bodies = await Promise.all(responses.map((r) => r.text()));
  for (let i = 0; i < count; i++) {
    assert_equals(responses[i].status, 200);
    assert_equals(bodies[i], `request ${i}`);
  }
}, "Concurrent fetches get their own responses");

function open(path, protocols = []) {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket(`ws://${acheHost}${path}`, protocols);
    ws.binaryType = "arraybuffer";
    ws.onopen = () => resolve(ws);
    ws.onerror = () => reject(new Error("WebSocket error"));
  });
}

function next(ws, event) {
  return new Promise((resolve, reject) => {
    ws.addEventListener(event, resolve, { once: true });
    if (event !== "close") {
      ws.addEventListener("close", () => reject(new Error("closed")), {
        once: true,
      });
    }
  });
}

promise_test(async (t) => {
  const ws = await open("/ws");
  t.add_cleanup(() => ws.close());
  const message = next(ws, "message");
  ws.send("héllø ✓");
  assert_equals((await message).data, "héllø ✓");
}, "A WebSocket text message is echoed as text");

promise_test(async (t) => {
  const ws = await open("/ws");
  t.add_cleanup(() => ws.close());
  const bytes = new Uint8Array([0, 1, 2, 127, 128, 254, 255]);
  const message = next(ws, "message");
  ws.send(bytes.buffer);
  const data = (await message).data;
  assert_true(data instanceof ArrayBuffer, "ArrayBuffer");
  assert_array_equals(new Uint8Array(data), bytes);
}, "A WebSocket binary message is echoed as binary");

promise_test(async () => {
  const ws = await open("/ws/close");
  const closed = next(ws, "close");
  ws.send("goodbye from the browser");
  const event = await closed;
  assert_equals(event.code, 4001);
  assert_equals(event.reason, "goodbye from the browser");
}, "The browser sees the code and reason of a close the server starts");

promise_test(async (t) => {
  const ws = await open("/ws", ["chat"]);
  t.add_cleanup(() => ws.close());
  assert_equals(ws.protocol, "chat");
  const message = next(ws, "message");
  ws.send("hello chat");
  assert_equals((await message).data, "hello chat");
}, "A WebSocket that offers the chat subprotocol gets it");
