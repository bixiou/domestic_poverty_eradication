/* Indicators of Fabre (2025), "Shortfall of Domestic Resources to Eradicate Extreme Poverty by 2030".
 * Port of the R functions of code_poverty/preparation.R (compute_poverty_rate, compute_poverty_gap,
 * compute_antipoverty_cap, compute_antipoverty_tax, compute_min_funded, compute_income_floor).
 * Works in the browser (global `PovertyModel`) and in Node (module.exports).
 *
 * Country objects come from data/countries.json:
 *   avg[0..99]: mean welfare of each percentile at survey year ($/day, 2021 PPP)
 *   q[0..98]: upper bound of percentiles 1..99 (the top percentile has no upper bound: its max is its mean)
 *   growth_to_start, growth_start, growth_2022, trend, reg, scaling_factor, pop (2022..2035), pop_year
 */
(function (root, factory) {
  if (typeof module === "object" && module.exports) module.exports = factory();
  else root.PovertyModel = factory();
})(typeof self !== "undefined" ? self : this, function () {
  "use strict";
  const N = 100;
  let defaultPopShare = 0.01;
  let popYears = [];

  function init(meta) {
    if (meta && meta.pop_share != null) defaultPopShare = meta.pop_share;
    if (meta && meta.pop_years) popYears = meta.pop_years;
  }

  function popShares(c) {
    if (c.pop_share) return c.pop_share;
    if (!c._ps) c._ps = new Array(N).fill(defaultPopShare);
    return c._ps;
  }

  /* Growth factor from the survey year to `year`.
   * scenario: {type: "rate", rate: 0.03} | {type: "trend"} | {type: "reg"}.
   * year 2022 uses observed growth (the "now" scenario), independent of the scenario.
   * Mirrors compute_distribution_2030: the factor is growth_to_start * g^(year - growth_start);
   * for "trend", countries without trend (no GDP data) get a factor of 1; for "reg", they are missing (NaN). */
  function growthFactor(c, scenario, year) {
    if (year === "survey") return 1;
    if (year === 2022) return c.growth_2022 == null ? 1 : c.growth_2022;
    const n = year - c.growth_start;
    if (scenario.type === "trend") return c.trend == null ? 1 : c.growth_to_start * Math.pow(1 + c.trend, n);
    if (scenario.type === "reg") return c.reg == null ? NaN : c.growth_to_start * Math.pow(c.reg, n);
    return c.growth_to_start * Math.pow(1 + scenario.rate, n);
  }

  /* Distribution of a country in a given year. hfce: survey rescaled to national accounts
   * (the gap between HFCE and survey mean is added to the top percentile, as in create_p(rescale = TRUE)). */
  function distribution(c, scenario, year, hfce) {
    const ps = popShares(c);
    const f = growthFactor(c, scenario, year);
    const avg = new Float64Array(N), max = new Float64Array(N + 1), min = new Float64Array(N + 1);
    let meanSurvey = 0;
    for (let i = 0; i < N; i++) meanSurvey += c.avg[i] * ps[i];
    for (let i = 0; i < N; i++) avg[i] = c.avg[i];
    if (hfce) avg[N - 1] += 100 * (c.scaling_factor - 1) * meanSurvey;
    let mean = 0;
    for (let i = 0; i < N; i++) { avg[i] *= f; mean += avg[i] * ps[i]; }
    // 1-indexed bounds as in R: max[i], min[i] for i = 1..100
    for (let i = 1; i < N; i++) max[i] = c.q[i - 1] * f;
    max[N] = avg[N - 1];
    min[1] = 0;
    for (let i = 2; i <= N; i++) min[i] = max[i - 1];
    const pop = year === "survey" ? c.pop_year : popAt(c, year);
    return { avg, max, min, ps, mean, factor: f, pop };
  }

  function popAt(c, year) {
    const k = popYears.indexOf(year);
    return k >= 0 ? c.pop[k] : NaN;
  }

  function povertyRate(d, line) { // share of population (0-1)
    let r = 0;
    for (let i = 0; i < N; i++) {
      if (Number.isNaN(d.avg[i])) return NaN;
      if (d.avg[i] < line) r += d.ps[i];
    }
    return r;
  }

  function povertyGap(d, line) { // per capita, $/day
    let g = 0;
    for (let i = 0; i < N; i++) g += Math.max(0, line - d.avg[i]) * d.ps[i];
    return Number.isNaN(d.mean) ? NaN : g;
  }

  function taxableBase(d, threshold) { // per capita, $/day
    let b = 0;
    for (let i = 0; i < N; i++) b += Math.max(0, d.avg[i] - threshold) * d.ps[i];
    return b;
  }

  /* Linear marginal rate above `threshold` closing the poverty gap, in % (compute_antipoverty_tax). */
  function antipovertyTax(d, line, threshold) {
    const gap = 100 * povertyGap(d, line);
    const base = 100 * taxableBase(d, threshold);
    return 100 * gap / Math.max(1e-10, base);
  }

  /* Antipoverty cap (compute_antipoverty_cap), faithful port including its interpolation and conventions.
   * Returns {cap, status}: status "none" (no poverty, cap = Infinity), "insufficient" (expropriating all
   * income above the poverty line is not enough: by convention the cap is then set around the mean), "ok". */
  function antipovertyCap(d, line) {
    const gap = povertyGap(d, line);
    if (Number.isNaN(gap)) return { cap: NaN, status: "na" };
    const ps = d.ps;
    const psAbove = new Float64Array(N + 2); // psAbove[k] = sum of ps for percentiles k..100 (1-indexed), with ps_101 = 0
    for (let k = N; k >= 1; k--) psAbove[k] = psAbove[k + 1] + ps[k - 1];
    let pe = 100, cap = Infinity, funded = 0, fundedOld = 0, lpe = 0, lte = 0, conv = false;
    while (funded < gap && pe > 0) {
      cap = d.min[pe];
      fundedOld = funded;
      lpe = (d.avg[pe - 1] - cap) * ps[pe - 1];
      // R: sum(pop_share_[(pe+1):100]); for pe = 100 this is pop_share_101 (= 0) + pop_share_100
      const above = pe < 100 ? psAbove[pe + 1] : ps[N - 1];
      lte = (d.max[pe] - cap) * above;
      funded = funded + lpe + (pe < 100 ? 1 : 0) * lte;
      pe = pe - 1;
      if (pe < 1 || d.min[pe] < line) { pe = 0; cap = d.mean; conv = true; }
    }
    if (pe === 99) cap = cap + 2 * (d.avg[N - 1] - cap) * Math.pow((funded - gap) / (funded - fundedOld), 2);
    else if (funded > gap) cap = cap + (d.max[pe + 1] - cap) * (lte / (lpe + lte)) * (funded - gap) / (funded - fundedOld);
    return { cap, status: gap === 0 ? "none" : conv ? "insufficient" : "ok" };
  }

  /* Demogrant (income floor) funded by `revenues` per capita (compute_min_funded): the lowest incomes are topped up. */
  function minFunded(d, revenues) {
    if (Number.isNaN(revenues)) return NaN;
    const a = d.avg, ps = d.ps;
    let cost = (a[1] - a[0]) * ps[0];
    let cum = ps[0];
    let i = 2; // 1-indexed as in R
    while (cost < revenues) {
      if (i >= N) { // beyond the data in R (error); natural extension: everyone at the floor
        cum += ps[N - 1];
        return a[N - 1] + (revenues - cost) / cum;
      }
      cum += ps[i - 1];
      cost += (a[i] - a[i - 1]) * cum;
      i++;
    }
    return a[i - 1] - (cost - revenues) / cum;
  }

  /* Income floor financed by a tax of `rate` % above `threshold` (compute_income_floor, revenues recycled nationally). */
  function incomeFloor(d, threshold, rate) {
    return minFunded(d, taxableBase(d, threshold) * rate / 100);
  }

  /* All indicators of a country for given parameters
   * params: {line, threshold, rate, scenario, year, hfce} */
  function indicators(c, params) {
    const d = distribution(c, params.scenario, params.year, params.hfce);
    const gap = povertyGap(d, params.line);
    const capRes = antipovertyCap(d, params.line);
    return {
      code: c.code, name: c.name, pop: d.pop, mean: d.mean,
      rate: 100 * povertyRate(d, params.line),
      gap_pc: gap,
      gap: 100 * gap / d.mean,
      tax: antipovertyTax(d, params.line, params.threshold),
      cap: capRes.cap, cap_status: capRes.status,
      floor: incomeFloor(d, params.threshold, params.rate),
    };
  }

  /* Population-weighted aggregates over a set of country indicator rows (as world_rate / world_gap in update.R). */
  function aggregate(rows) {
    let pr = 0, prw = 0, gN = 0, gD = 0, tN = 0, tW = 0, fN = 0, fW = 0, mN = 0, mW = 0, pop = 0, nTax100 = 0, nInsuf = 0, n = 0;
    for (const r of rows) {
      if (!(r.pop > 0)) continue;
      n++;
      if (!Number.isNaN(r.rate)) { pr += r.rate * r.pop; prw += r.pop; pop += r.pop; }
      if (!Number.isNaN(r.gap_pc)) { gN += r.gap_pc * r.pop; gD += r.mean * r.pop; }
      if (!Number.isNaN(r.tax)) { tN += r.tax * r.pop; tW += r.pop; if (r.tax > 100) nTax100++; }
      if (!Number.isNaN(r.floor)) { fN += r.floor * r.pop; fW += r.pop; }
      if (!Number.isNaN(r.mean)) { mN += r.mean * r.pop; mW += r.pop; }
      if (r.cap_status === "insufficient") nInsuf++;
    }
    return {
      rate: pr / prw, gap: 100 * gN / gD, gap_pc: gN / prw, tax: tN / tW, floor: fN / fW, mean: mN / mW,
      pop, n, nTax100, nInsuf, poor: pr / 100,
    };
  }

  return { init, growthFactor, distribution, povertyRate, povertyGap, taxableBase, antipovertyTax, antipovertyCap, minFunded, incomeFloor, indicators, aggregate, popAt };
});
