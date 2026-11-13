import * as net from "node:net";

const errorNames = {
  ECONNREFUSED: "ConnectionRefused",
  ECONNRESET: "ConnectionReset",
  ECONNABORTED: "ConnectionAborted",
  EPIPE: "BrokenPipe",
  ETIMEDOUT: "TimedOut",
  EADDRNOTAVAIL: "AddrNotAvailable",
};

function denoError(err) {
  const name = errorNames[err.code];
  if (name) err.name = name;
  return err;
}

function badResource() {
  const err = new Error("Bad resource ID");
  err.name = "BadResource";
  return err;
}

class Conn {
  #socket;
  #chunks = [];
  #eof = false;
  #error = null;
  #closed = false;
  #pending = null;
  #ops = 0;

  constructor(socket) {
    this.#socket = socket;
    socket.unref();
    socket.on("data", (chunk) => {
      this.#chunks.push(chunk);
      this.#settle();
    });
    socket.on("end", () => {
      this.#eof = true;
      this.#settle();
    });
    socket.on("error", (err) => {
      this.#error = denoError(err);
      this.#settle();
    });
  }

  #begin() {
    if (this.#ops++ === 0) this.#socket.ref();
  }

  #end() {
    if (--this.#ops === 0) this.#socket.unref();
  }

  #take(buffer) {
    let n = 0;
    while (n < buffer.length && this.#chunks.length > 0) {
      const chunk = this.#chunks[0];
      const k = Math.min(chunk.length, buffer.length - n);
      buffer.set(chunk.subarray(0, k), n);
      n += k;
      if (k === chunk.length) this.#chunks.shift();
      else this.#chunks[0] = chunk.subarray(k);
    }
    return n;
  }

  #settle() {
    const pending = this.#pending;
    if (pending === null) return;
    if (this.#chunks.length > 0) {
      this.#pending = null;
      pending.resolve(this.#take(pending.buffer));
    } else if (this.#error !== null) {
      this.#pending = null;
      pending.reject(this.#error);
    } else if (this.#eof) {
      this.#pending = null;
      pending.resolve(null);
    }
  }

  async read(buffer) {
    if (this.#closed) throw badResource();
    if (buffer.length === 0) return 0;
    if (this.#pending !== null) throw new Error("concurrent read");
    this.#begin();
    try {
      return await new Promise((resolve, reject) => {
        this.#pending = { buffer, resolve, reject };
        this.#settle();
      });
    } finally {
      this.#end();
    }
  }

  async write(buffer) {
    if (this.#closed) throw badResource();
    if (this.#error !== null) throw this.#error;
    this.#begin();
    try {
      await new Promise((resolve, reject) => {
        this.#socket.write(buffer, (err) =>
          err ? reject(denoError(err)) : resolve(),
        );
      });
      return buffer.length;
    } finally {
      this.#end();
    }
  }

  close() {
    if (this.#closed) throw badResource();
    this.#closed = true;
    this.#socket.destroy();
    const pending = this.#pending;
    if (pending !== null) {
      this.#pending = null;
      pending.reject(badResource());
    }
  }
}

function connect({ hostname = "127.0.0.1", port }) {
  return new Promise((resolve, reject) => {
    const socket = net.connect({ host: hostname, port });
    const onError = (err) => reject(denoError(err));
    socket.once("error", onError);
    socket.once("connect", () => {
      socket.off("error", onError);
      resolve(new Conn(socket));
    });
  });
}

globalThis.Deno = {
  args: process.argv.slice(2),
  exit: (code = 0) => process.exit(code),
  connect,
};
