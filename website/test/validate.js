// Compares the JavaScript port (js/model.js) to the R values exported by code_poverty/website_data.R.
// Usage (from website/): node test/validate.js
const fs = require("fs");
const path = require("path");
const M = require("../js/model.js");
const data = JSON.parse(fs.readFileSync(path.join(__dirname, "../data/countries.json")));
const val = JSON.parse(fs.readFileSync(path.join(__dirname, "../data/validation.json")));
M.init(data.meta);
const byCode = Object.fromEntries(data.countries.map(c => [c.code, c]));
const scen = {
  average: [{ type: "rate", rate: 0.03 }, 2030], very_optimistic: [{ type: "rate", rate: 0.07 }, 2030],
  trend: [{ type: "trend" }, 2030], reg: [{ type: "reg" }, 2030], now: [{ type: "rate", rate: 0.03 }, 2022],
  y_2025: [{ type: "rate", rate: 0 }, 2025], Y3_35: [{ type: "rate", rate: 0.03 }, 2035], Y7_35: [{ type: "rate", rate: 0.07 }, 2035],
};
const EQ_LINE = 2.46, EQ_THR = 6.85 * 2.46 / 2.15;
let failures = 0;
const relTol = 1e-4;
console.log("test".padEnd(42), "n", "max abs diff", "max rel diff", "worst");
for (const t of val.tests) {
  const [scenario, year] = scen[t.g];
  let maxAbs = 0, maxRel = 0, worst = "", n = 0;
  val.codes.forEach((code, k) => {
    const r = t.values[k];
    const c = byCode[code];
    const d = M.distribution(c, scenario, year, t.src === "s");
    let j;
    if (t.ind === "rate") j = 100 * M.povertyRate(d, t.line);
    else if (t.ind === "gap") j = 100 * M.povertyGap(d, t.line) / d.mean;
    else if (t.ind === "tax") j = M.antipovertyTax(d, t.line, t.thr);
    else if (t.ind === "cap") { j = M.antipovertyCap(d, t.line).cap; if (j === Infinity) j = 1e300; }
    else if (t.ind === "floor") j = M.incomeFloor(d, t.thr, t.rate);
    if (r === null || r === "NA" || r === undefined) { if (!Number.isNaN(j)) { maxAbs = Infinity; worst = code + " (R NA, JS " + j + ")"; } return; }
    n++;
    const abs = Math.abs(j - r), rel = abs / Math.max(1e-9, Math.abs(r));
    if (abs > maxAbs) { maxAbs = abs; worst = `${code} R=${r} JS=${j}`; }
    if (rel > maxRel) maxRel = rel;
  });
  const label = `${t.src}/${t.g}/${t.ind}/line=${t.line ?? ""}/thr=${t.thr ? +t.thr.toFixed(3) : ""}/rate=${t.rate ?? ""}`;
  const ok = maxRel < relTol || maxAbs < 1e-6;
  if (!ok) failures++;
  console.log((ok ? "OK   " : "FAIL ") + label.padEnd(37), n, maxAbs.toExponential(2), maxRel.toExponential(2), ok ? "" : worst);
}
// World aggregates
function rows(g, ind, p, src = "p") {
  const [scenario, year] = scen[g];
  return data.countries.map(c => M.indicators(c, { line: p.line, threshold: p.thr ?? 8.3, rate: p.rate ?? 10, scenario, year, hfce: src === "s" }));
}
const W = val.world;
const lic = data.countries.filter(c => c.lic).map(c => c.code), ssa = data.countries.filter(c => c.ssa).map(c => c.code);
const checks = {
  RatePovNow: M.aggregate(rows("now", 0, { line: 3 })).rate,
  RatePovTwentyFive: M.aggregate(rows("y_2025", 0, { line: 3 })).rate,
  RatePovThree: M.aggregate(rows("average", 0, { line: 3 })).rate,
  RatePovTrend: M.aggregate(rows("trend", 0, { line: 3 })).rate,
  RatePovSeven: M.aggregate(rows("very_optimistic", 0, { line: 3 })).rate,
  RatePovReg: M.aggregate(rows("reg", 0, { line: 3 })).rate,
  GapNow: M.aggregate(rows("now", 0, { line: 3 })).gap,
  GapThree: M.aggregate(rows("average", 0, { line: 3 })).gap,
  RatePovThreeEq: M.aggregate(rows("average", 0, { line: EQ_LINE })).rate,
  RatePovThreeTF: M.aggregate(rows("Y3_35", 0, { line: 3 })).rate,
  GapThreeTF: M.aggregate(rows("Y3_35", 0, { line: 3 })).gap,
  NbCapEight: M.aggregate(rows("average", 0, { line: 3, thr: 8.3 })).nTax100,
  TaxEightSSA: M.aggregate(rows("average", 0, { line: 3, thr: 8.3 }).filter(r => ssa.includes(r.code))).tax,
  TaxEightLIC: M.aggregate(rows("average", 0, { line: 3, thr: 8.3 }).filter(r => lic.includes(r.code))).tax,
  NbCapEq: M.aggregate(rows("average", 0, { line: EQ_LINE, thr: EQ_THR })).nTax100,
  NbFloor: rows("average", 0, { line: 3, thr: 8.3, rate: 10 }).filter(r => r.floor < 3).length,
  NbFloorSevenHFCE: rows("very_optimistic", 0, { line: 3, thr: 8.3, rate: 10 }, "s").filter(r => r.floor < 3).length,
};
console.log("\nAggregate".padEnd(20), "R".padStart(14), "JS".padStart(14), "abs diff");
for (const [k, j] of Object.entries(checks)) {
  const r = W[k], abs = Math.abs(j - r);
  const ok = abs < 1e-6 * Math.max(1, Math.abs(r));
  if (!ok) failures++;
  console.log((ok ? "OK   " : "FAIL ") + k.padEnd(18), String(+r.toFixed(6)).padStart(14), String(+j.toFixed(6)).padStart(14), abs.toExponential(2));
}
console.log(failures ? `\n${failures} FAILURE(S)` : "\nAll checks passed.");
process.exit(failures ? 1 : 0);
