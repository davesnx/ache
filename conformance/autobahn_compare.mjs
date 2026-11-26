// node autobahn_compare.mjs INDEX_JSON EXPECTED_TXT
//
// Compares an Autobahn fuzzingclient report (index.json) with the expected
// results. Each case has a behavior and a behaviorClose; the worse of the two
// counts. OK, INFORMATIONAL and UNIMPLEMENTED pass. A case not listed in the
// expected file must pass; a listed case may be as bad as its line says.
// Exits 1 when a case is worse than expected or a listed case did not run.

import { readFileSync } from "node:fs";

const rank = {
  OK: 0,
  INFORMATIONAL: 0,
  UNIMPLEMENTED: 0,
  "NON-STRICT": 1,
  FAILED: 2,
};

const [indexPath, expectedPath] = process.argv.slice(2);
if (!indexPath || !expectedPath) {
  console.error("usage: node autobahn_compare.mjs INDEX_JSON EXPECTED_TXT");
  process.exit(2);
}

const index = JSON.parse(readFileSync(indexPath, "utf8"));
const agents = Object.keys(index);
if (agents.length !== 1) {
  console.error(`${indexPath}: expected one agent, got ${agents.length}`);
  process.exit(2);
}
const cases = index[agents[0]];

const expected = new Map();
readFileSync(expectedPath, "utf8")
  .split("\n")
  .forEach((line, i) => {
    const text = line.replace(/#.*/, "").trim();
    if (text === "") return;
    const [id, behavior, ...rest] = text.split(/\s+/);
    if (!(behavior in rank) || rest.length > 0) {
      console.error(`${expectedPath}:${i + 1}: expected "case-id behavior # reason"`);
      process.exit(2);
    }
    expected.set(id, behavior);
  });

const worse = (a, b) => (rank[a] >= rank[b] ? a : b);
const byId = (a, b) => {
  const x = a.split(".").map(Number);
  const y = b.split(".").map(Number);
  for (let i = 0; i < Math.max(x.length, y.length); i++) {
    if ((x[i] ?? -1) !== (y[i] ?? -1)) return (x[i] ?? -1) - (y[i] ?? -1);
  }
  return 0;
};

const totals = {};
let failures = 0;
for (const id of Object.keys(cases).sort(byId)) {
  const { behavior, behaviorClose } = cases[id];
  totals[behavior] = (totals[behavior] ?? 0) + 1;
  for (const b of [behavior, behaviorClose]) {
    if (!(b in rank)) {
      console.log(`UNKNOWN BEHAVIOR: ${id} ${b}`);
      failures++;
    }
  }
  const actual = worse(behavior, behaviorClose);
  const allowed = expected.get(id) ?? "OK";
  const shown = `${id} behavior=${behavior} behaviorClose=${behaviorClose}`;
  if (rank[actual] > rank[allowed]) {
    console.log(`WORSE THAN EXPECTED (${allowed}): ${shown}`);
    failures++;
  } else if (expected.has(id) && rank[actual] < rank[allowed]) {
    console.log(`BETTER THAN EXPECTED (${allowed}): ${shown}; update the expected file`);
  } else if (rank[actual] > 0) {
    console.log(`expected: ${shown}`);
  }
}

for (const id of [...expected.keys()].sort(byId)) {
  if (!(id in cases)) {
    console.log(`NOT IN REPORT: ${id}`);
    failures++;
  }
}

const total = Object.keys(cases).length;
const summary = Object.entries(totals)
  .sort(([a], [b]) => a.localeCompare(b))
  .map(([b, n]) => `${b} ${n}`)
  .join(", ");
console.log(`${agents[0]}: ${total} cases: ${summary}`);
process.exit(failures === 0 ? 0 : 1);
