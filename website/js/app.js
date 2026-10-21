/* User interface of the website. The computations are in model.js. */
(function () {
  "use strict";
  const M = window.PovertyModel;
  const $ = (id) => document.getElementById(id);
  let DATA = null, BY_CODE = {}, ROWS = [], PARAMS = null;
  const UI = { continuous: false, tab: "map", ind: "floor", country: "COD", log: true, sort: { key: "rate", dir: -1 }, filter: "", group: "all" };
  const STD_LINES = [2.46, 3, 4.2, 8.3, 18.15];
  const YEARS = [2026, 2030, 2035, 2040, 2050, 2100];

  /* ---------- formatting ---------- */
  const css = (v) => getComputedStyle(document.documentElement).getPropertyValue(v).trim();
  const isNum = (x) => typeof x === "number" && Number.isFinite(x);
  // Always "." as decimal separator (independent of the user's locale); thin space as thousands separator from 10 000
  function fmtNum(x, d) {
    if (!isNum(x)) return "–";
    const s = Math.abs(x).toFixed(d), [i, f] = s.split(".");
    const ii = i.length > 4 ? i.replace(/\B(?=(\d{3})+(?!\d))/g, "\u202f") : i;
    return (x < 0 && +s !== 0 ? "-" : "") + ii + (f ? "." + f : "");
  }
  const fmtBreak = (v) => String(+v.toFixed(2)); // e.g. 0.1, 8.3, 18.15
  function fmtPct(x) { if (!isNum(x)) return "–"; if (x === 0) return "0%"; if (x < 0.1) return "<0.1%"; return fmtNum(x, x < 10 ? 1 : 0) + "%"; }
  function fmtGap(x) { if (!isNum(x)) return "–"; if (x === 0) return "0%"; if (x < 0.01) return "<0.01%"; return fmtNum(x, x < 1 ? 2 : x < 10 ? 1 : 0) + "%"; }
  function fmtUsd(x) { if (!isNum(x)) return "–"; return "$" + fmtNum(x, x < 10 ? 2 : x < 100 ? 1 : 0); }
  function fmtPop(x) { if (!isNum(x)) return "–"; if (x === 0) return "0"; if (x >= 1e9) return fmtNum(x / 1e9, 2) + " billion"; if (x >= 1e6) return fmtNum(x / 1e6, x >= 1e7 ? 0 : 1) + " million"; return fmtNum(x / 1e3, 0) + " thousand"; }
  function fmtCap(r) { return r.cap_status === "none" ? "no poverty" : r.cap_status === "insufficient" ? "insufficient" : fmtUsd(r.cap); }
  function fmtTax(r) { return isNum(r.tax) ? (r.tax > 1e4 ? ">10,000%" : fmtPct(r.tax)) : "–"; }
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));

  /* ---------- parameters ---------- */
  function readParams() {
    const gp = $("growth-preset").value;
    let scenario;
    if (gp === "trend") scenario = { type: "trend" };
    else if (gp === "reg") scenario = { type: "reg" };
    else scenario = { type: "rate", rate: gp === "custom" ? (parseFloat($("growth").value) || 0) / 100 : parseFloat(gp) };
    return {
      line: Math.max(0.01, parseFloat($("line").value) || 3),
      threshold: Math.max(0, parseFloat($("thr").value) || 0),
      rate: Math.min(100, Math.max(0, parseFloat($("rate").value) || 0)),
      scenario, growthPreset: gp,
      year: parseInt($("year").value, 10),
      hfce: $("source").value === "hfce",
    };
  }
  function scenarioLabel(p) {
    return projLabel(p);
  }
  function projLabel(p) { // growth scenario used for projections (whatever the horizon year)
    if (p.scenario.type === "trend") return "trend growth (2015–2025)";
    if (p.scenario.type === "reg") return "autoregressive growth";
    return fmtBreak(p.scenario.rate * 100) + "% growth";
  }
  function paramsText(p) {
    return `Poverty line $${fmtNum(p.line, 2)}/day · taxation threshold $${fmtNum(p.threshold, 2)}/day · income-floor tax ${fmtNum(p.rate, p.rate % 1 ? 1 : 0)}% · ${p.year}, ${scenarioLabel(p)} · ${p.hfce ? "surveys rescaled to HFCE" : "survey data"}`;
  }
  function paramsSlug(p) {
    const g = p.scenario.type === "rate" ? `growth${+(p.scenario.rate * 100).toFixed(2)}pct` : p.scenario.type;
    return `line${p.line}_thr${+p.threshold.toFixed(2)}_rate${p.rate}_${g}_${p.year}_${p.hfce ? "hfce" : "survey"}`;
  }

  function syncPreset(presetId, inputId) {
    const sel = $(presetId), v = parseFloat($(inputId).value);
    const match = Array.from(sel.options).find((o) => o.value !== "custom" && Math.abs(parseFloat(o.value) - v) < 0.006);
    sel.value = match ? match.value : "custom";
  }

  /* ---------- URL state ---------- */
  function writeHash() {
    const p = PARAMS;
    const h = new URLSearchParams({ line: p.line, thr: $("thr").value, rate: p.rate, g: p.growthPreset === "custom" ? $("growth").value : p.growthPreset, gc: p.growthPreset === "custom" ? 1 : 0, year: p.year, src: p.hfce ? "hfce" : "survey", tab: UI.tab, ind: UI.ind, c: UI.country });
    history.replaceState(null, "", "#" + h.toString());
  }
  function readHash() {
    const h = new URLSearchParams(location.hash.slice(1));
    if (h.has("line")) { $("line").value = h.get("line"); syncPreset("line-preset", "line"); }
    if (h.has("thr")) { $("thr").value = h.get("thr"); syncPreset("thr-preset", "thr"); }
    if (h.has("rate")) $("rate").value = h.get("rate");
    if (h.has("g")) {
      const g = h.get("g");
      if (h.get("gc") === "1") { $("growth-preset").value = "custom"; $("growth").value = g; }
      else if (Array.from($("growth-preset").options).some((o) => o.value === g)) $("growth-preset").value = g;
    }
    if (h.has("year") && YEARS.includes(+h.get("year"))) $("year").value = h.get("year");
    if (h.get("src") === "hfce") $("source").value = "hfce";
    if (["map", "table", "country", "about"].includes(h.get("tab"))) UI.tab = h.get("tab");
    if (h.has("ind") && Array.from($("map-ind").options).some((o) => o.value === h.get("ind"))) { UI.ind = h.get("ind"); $("map-ind").value = UI.ind; }
    if (h.has("c") && BY_CODE[h.get("c")]) UI.country = h.get("c");
  }

  /* ---------- computation ---------- */
  function compute() {
    PARAMS = readParams();
    ROWS = DATA.countries.map((c) => {
      const r = M.indicators(c, PARAMS);
      r.c = c;
      r.poor = r.rate / 100 * r.pop;
      return r;
    });
  }
  function aggregates() {
    return [
      ["World", M.aggregate(ROWS)],
      ["Sub-Saharan Africa", M.aggregate(ROWS.filter((r) => r.c.ssa))],
      ["Low-income countries", M.aggregate(ROWS.filter((r) => r.c.lic))],
    ];
  }

  /* ---------- headline ---------- */
  function renderHeadline() {
    const w = M.aggregate(ROWS);
    $("h-rate").textContent = fmtPct(w.rate);
    $("h-poor").textContent = fmtPop(w.poor);
    $("h-gap").textContent = fmtGap(w.gap);
    $("h-tax").textContent = `${w.nTax100} of ${w.n}`;
    $("params-summary").textContent = paramsText(PARAMS) + ".";
  }

  /* ---------- map ---------- */
  const IND = {
    rate: { title: "Poverty rate", unit: "% of population", fmt: (r) => fmtPct(r.rate) },
    gap: { title: "Poverty gap", unit: "% of mean consumption", fmt: (r) => fmtGap(r.gap) },
    tax: { title: "Antipoverty tax", unit: "marginal rate above the taxation threshold needed to close the poverty gap", fmt: fmtTax },
    cap: { title: "Antipoverty cap", unit: "$/day: confiscating all income above it would close the poverty gap", fmt: fmtCap },
    floor: { title: "Income floor", unit: "$/day guaranteed with the chosen tax above the taxation threshold", fmt: (r) => fmtUsd(r.floor) },
    mean: { title: "Mean consumption", unit: "$/day per person (2021 PPP)", fmt: (r) => fmtUsd(r.mean) },
  };
  function seqScale() {
    const steps = ["--seq-100", "--seq-200", "--seq-300", "--seq-400", "--seq-500", "--seq-600", "--seq-700"].map(css);
    return steps.map((c, i) => [i / (steps.length - 1), c]);
  }
  const solid = (c) => [[0, c], [1, c]];
  function logTicks(min, max) {
    const cand = [0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100, 300, 1000, 3000];
    return cand.filter((v) => v >= min * 0.999 && v <= max * 1.001);
  }

  /* Discrete colour scales of the paper (plot_world_map in code_poverty, RColorBrewer RdBu); colours from lowest to highest bin. */
  const RDBU8 = ["#053061", "#2166AC", "#4393C3", "#92C5DE", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B"];
  function mapBins(ind, p) {
    if (ind === "tax" || ind === "gap") return { breaks: [0, 0.1, 1, 5, 10, 25, 50, 100, Infinity], left: true, colors: RDBU8, pre: "", suf: "%" };
    if (ind === "rate") return { breaks: [0, 1, 5, 10, 20, 40, 60, 80, 100], left: true, colors: RDBU8, pre: "", suf: "%" };
    if (ind === "cap") {
      let breaks = [0, p.line, p.threshold, 18.15, 40, 80, 400, Infinity]; // paper: 0, 3, 8.3, 18.15, ...; 3 and 8.3 follow the selected line and threshold
      if (!breaks.every((b, i) => i === 0 || b > breaks[i - 1])) breaks = [0, 3, 8.3, 18.15, 40, 80, 400, Infinity];
      return { breaks, left: false, colors: ["#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#4393C3", "#053061"], pre: "$", suf: "" };
    }
    if (ind === "floor") return { breaks: [0, 2, 3, 4.2, 5.5, 8.3, 14, 25, 40, 100, Infinity], left: false, colors: ["#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"], pre: "$", suf: "" };
    return { breaks: [0, 3, 4.2, 8.3, 18.15, 40, 80, Infinity], left: false, colors: ["#B2182B", "#EF8A62", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#67A9CF", "#2166AC"], pre: "$", suf: "" };
  }
  function binOf(v, B) {
    const b = B.breaks, n = b.length - 1;
    for (let i = 0; i < n; i++) if (B.left ? v < b[i + 1] : v <= b[i + 1]) return i;
    return n - 1; // e.g. a poverty rate of exactly 100%
  }
  function binLabel(i, B) {
    const b = B.breaks, f = (v) => B.pre + fmtBreak(v) + B.suf;
    if (b[i + 1] === Infinity) return "> " + f(b[i]);
    if (i === 0 && !B.left) return "≤ " + f(b[1]);
    return `${f(b[i])} to ${f(b[i + 1])}`;
  }
  const mapValue = (ind, r) => ind === "cap" ? (r.cap_status === "insufficient" ? 0 : r.cap_status === "none" ? Infinity : r.cap) : r[ind];

  function renderMap() {
    const ind = UI.ind, p = PARAMS, info = IND[ind];
    $("map-title").textContent = `${info.title}, ${p.year}`;
    $("map-sub").textContent = `${info.unit}. ${paramsText(p)}.`;
    if (UI.continuous) renderMapContinuous(); else renderMapDiscrete();
    const el = $("map");
    if (!el._clickBound) {
      el.on("plotly_click", (ev) => { const code = ev.points && ev.points[0] && ev.points[0].location; if (BY_CODE[code]) { UI.country = code; $("country").value = code; selectTab("country"); } });
      el._clickBound = true;
    }
  }
  function mapLayout() {
    const surface = css("--surface-1"), nodata = css("--nodata");
    return {
      margin: { l: 0, r: 0, t: 0, b: 0 }, paper_bgcolor: surface, plot_bgcolor: surface, separators: ".\u202f",
      geo: { projection: { type: "natural earth" }, showframe: false, showcoastlines: false, showland: true, landcolor: nodata, showcountries: true, countrycolor: css("--map-border"), bgcolor: surface, lataxis: { range: [-58, 85] } },
      font: { family: getComputedStyle(document.body).fontFamily, color: css("--text-secondary") },
      hoverlabel: { bgcolor: surface, bordercolor: css("--border"), font: { color: css("--text-primary") } },
    };
  }
  const MAP_CONFIG = { responsive: true, displaylogo: false, modeBarButtonsToRemove: ["select2d", "lasso2d"] };
  function renderMapDiscrete() {
    const ind = UI.ind, p = PARAMS, info = IND[ind], B = mapBins(ind, p);
    const valid = ROWS.filter((r) => isNum(r.mean) && (isNum(mapValue(ind, r)) || mapValue(ind, r) === Infinity));
    const groups = B.colors.map(() => []);
    valid.forEach((r) => groups[binOf(mapValue(ind, r), B)].push(r));
    const labels = B.colors.map((_, i) => binLabel(i, B) + (ind === "cap" && i === 0 ? " or insufficient" : ind === "cap" && i === B.colors.length - 1 ? " (incl. no poverty)" : ""));
    const hover = (r, i) => `<b>${esc(r.name)}</b><br>${info.title}: ${info.fmt(r)}${ind === "cap" && r.cap_status === "insufficient" ? " (even confiscating all income above the poverty line cannot close the gap)" : ""}<br>Bin: ${esc(labels[i])}<br>Survey: ${r.c.year}, ${r.c.welfare}`;
    const traces = groups.map((g, i) => ({ type: "choropleth", locationmode: "ISO-3", locations: g.map((r) => r.code), z: g.map(() => 1), text: g.map((r) => hover(r, i)), hoverinfo: "text",
      colorscale: solid(B.colors[i]), showscale: false, marker: { line: { color: css("--map-border"), width: 0.4 } } })).filter((t) => t.locations.length);
    Plotly.react("map", traces, mapLayout(), MAP_CONFIG);
    const items = B.colors.map((c, i) => [c, labels[i]]).reverse();
    items.push([css("--nodata"), "No data"]);
    const note = ind === "tax" ? `<p class="legend-note">Above 100%: even confiscating all income above $${fmtNum(p.threshold, 2)}/day cannot close the poverty gap.</p>`
      : ind === "floor" ? `<p class="legend-note">Below $${fmtNum(p.line, 2)}/day: a ${+p.rate.toFixed(2)}% tax above $${fmtNum(p.threshold, 2)}/day cannot eradicate poverty at the selected line.</p>`
      : ind === "cap" ? `<p class="legend-note">Insufficient: even confiscating all income above the poverty line cannot close the gap (shown in the lowest bin).</p>` : "";
    $("map-legend").className = "cat-legend vertical";
    $("map-legend").innerHTML = `<div class="legend-title">${esc(info.title)}${B.suf ? " (%)" : " ($/day)"}</div>` + items.map(([c, t]) => `<div><span class="sw" style="background:${c}"></span>${esc(t)}</div>`).join("") + note;
  }

  function renderMapContinuous() {
    const ind = UI.ind, p = PARAMS, info = IND[ind];
    const textColor = css("--text-secondary"), surface = css("--surface-1");
    const traces = [], legend = [];
    const valid = ROWS.filter((r) => isNum(r.mean));
    const hover = (r) => `<b>${esc(r.name)}</b><br>${info.title}: ${info.fmt(r)}<br>Survey: ${r.c.year}, ${r.c.welfare}`;
    const base = { type: "choropleth", locationmode: "ISO-3", marker: { line: { color: css("--map-border"), width: 0.4 } }, hoverinfo: "text", colorbar: { thickness: 12, len: 0.75, outlinewidth: 0, tickfont: { color: textColor, size: 11 }, title: { text: "", font: { color: textColor } } } };
    let main = [], z = [], cb = {};
    if (ind === "rate") {
      main = valid; z = main.map((r) => r.rate);
      const zmax = Math.max(10, Math.ceil(Math.max(...z) / 10) * 10);
      cb = { zmin: 0, zmax, colorbar: { ...base.colorbar, ticksuffix: "%" } };
    } else if (ind === "tax") {
      main = valid.filter((r) => r.tax <= 100); z = main.map((r) => r.tax);
      cb = { zmin: 0, zmax: 100, colorbar: { ...base.colorbar, ticksuffix: "%" } };
      const over = valid.filter((r) => r.tax > 100);
      traces.push({ ...base, locations: over.map((r) => r.code), z: over.map(() => 1), text: over.map(hover), colorscale: solid(css("--highlight")), showscale: false, name: "> 100%" });
      legend.push([css("--highlight"), `Above 100%: confiscating all income above $${fmtNum(p.threshold, 2)}/day cannot close the gap (${over.length} countries)`]);
    } else {
      let vals;
      if (ind === "cap") {
        main = valid.filter((r) => r.cap_status === "ok"); vals = main.map((r) => r.cap);
        const ins = valid.filter((r) => r.cap_status === "insufficient"), none = valid.filter((r) => r.cap_status === "none");
        traces.push({ ...base, locations: ins.map((r) => r.code), z: ins.map(() => 1), text: ins.map(hover), colorscale: solid(css("--highlight")), showscale: false });
        traces.push({ ...base, locations: none.map((r) => r.code), z: none.map(() => 1), text: none.map(hover), colorscale: solid(css("--none")), showscale: false });
        legend.push([css("--highlight"), `Insufficient: even confiscating all income above the poverty line cannot close the gap (${ins.length} countries)`]);
        legend.push([css("--none"), `No poverty at this line (${none.length} countries)`]);
      } else if (ind === "gap") {
        main = valid; vals = main.map((r) => Math.max(0.01, r.gap));
        legend.push([css("--seq-100"), "Lightest: below 0.01% (including no poverty)"]);
      } else {
        main = valid.filter((r) => isNum(r[ind])); vals = main.map((r) => Math.max(0.01, r[ind]));
      }
      z = vals.map(Math.log10);
      const mn = Math.min(...vals), mx = Math.max(...vals);
      const ticks = logTicks(mn, mx);
      const pre = ind === "gap" ? "" : "$", suf = ind === "gap" ? "%" : "";
      cb = { zmin: Math.log10(mn), zmax: Math.log10(mx), colorbar: { ...base.colorbar, tickvals: ticks.map(Math.log10), ticktext: ticks.map((t) => pre + t + suf), title: { text: "log scale", side: "right", font: { color: textColor, size: 11 } } } };
    }
    traces.unshift({ ...base, ...cb, locations: main.map((r) => r.code), z, text: main.map(hover), colorscale: seqScale(), showscale: true });
    legend.push([css("--nodata"), "No data"]);
    Plotly.react("map", traces, mapLayout(), MAP_CONFIG);
    $("map-legend").className = "cat-legend";
    $("map-legend").innerHTML = legend.map(([c, t]) => `<span><span class="sw" style="background:${c}"></span>${esc(t)}</span>`).join("");
  }

  /* ---------- table ---------- */
  const COLS = [
    { key: "name", label: "Country", fmt: (r) => esc(r.name), csv: (r) => r.name, type: "str" },
    { key: "code", label: "Code", fmt: (r) => r.code, csv: (r) => r.code, type: "str" },
    { key: "year", label: "Survey year", fmt: (r) => r.c ? r.c.year : "", csv: (r) => r.c ? r.c.year : "", val: (r) => r.c.year },
    { key: "pop", label: "Population (millions)", fmt: (r) => fmtNum(r.pop / 1e6, r.pop < 1e7 ? 1 : 0), csv: (r) => r.pop },
    { key: "mean", label: "Mean consumption ($/day)", fmt: (r) => fmtUsd(r.mean), csv: (r) => r.mean },
    { key: "rate", label: "Poverty rate (%)", fmt: (r) => fmtPct(r.rate), csv: (r) => r.rate },
    { key: "poor", label: "Poor (millions)", fmt: (r) => fmtNum(r.poor / 1e6, r.poor < 1e7 ? 1 : 0), csv: (r) => r.poor },
    { key: "gap", label: "Poverty gap (% of mean)", fmt: (r) => fmtGap(r.gap), csv: (r) => r.gap },
    { key: "cap", label: "Antipoverty cap ($/day)", fmt: (r) => r.agg ? "" : (r.cap_status === "insufficient" ? '<span class="flag-dot" aria-hidden="true"></span>insufficient' : fmtCap(r)), csv: (r) => r.agg ? "" : r.cap_status === "ok" ? r.cap : r.cap_status, val: (r) => r.cap_status === "insufficient" ? -1 : r.cap_status === "none" ? Infinity : r.cap },
    { key: "tax", label: "Antipoverty tax (%)", fmt: (r) => (r.tax > 100 && !r.agg ? '<span class="flag-dot" aria-hidden="true"></span>' : "") + fmtTax(r), csv: (r) => r.tax },
    { key: "floor", label: "Income floor ($/day)", fmt: (r) => fmtUsd(r.floor), csv: (r) => r.floor },
  ];
  function tableRows() {
    const f = UI.filter.toLowerCase();
    let rows = ROWS.filter((r) => (UI.group === "all" || (UI.group === "ssa" && r.c.ssa) || (UI.group === "lic" && r.c.lic)) && (!f || r.name.toLowerCase().includes(f) || r.code.toLowerCase().includes(f)));
    const col = COLS.find((c) => c.key === UI.sort.key);
    const val = col.val || ((r) => r[col.key]);
    rows = rows.slice().sort((a, b) => {
      const va = val(a), vb = val(b);
      if (col.type === "str") return UI.sort.dir * String(va).localeCompare(String(vb));
      const na = !isNum(va) && va !== Infinity && va !== -Infinity, nb = !isNum(vb) && vb !== Infinity && vb !== -Infinity;
      if (na || nb) return na - nb; // missing values last
      return UI.sort.dir * (va === vb ? 0 : va < vb ? -1 : 1);
    });
    return rows;
  }
  function aggRows() {
    return aggregates().map(([name, a]) => ({ name, code: "", agg: true, pop: a.pop, mean: a.mean, rate: a.rate, poor: a.poor, gap: a.gap, tax: a.tax, floor: a.floor, cap_status: "" }));
  }
  function renderTable() {
    const t = $("tbl");
    $("tbl-caption").textContent = `Indicators in ${PARAMS.year}. ${paramsText(PARAMS)}.`;
    t.tHead.innerHTML = "<tr>" + COLS.map((c) => {
      const s = UI.sort.key === c.key ? (UI.sort.dir > 0 ? "ascending" : "descending") : "none";
      return `<th scope="col" aria-sort="${s}"><button type="button" data-key="${c.key}">${c.label}</button></th>`;
    }).join("") + "</tr>";
    t.querySelector("tbody.agg").innerHTML = aggRows().map((r) => "<tr>" + COLS.map((c) => `<td>${c.key === "year" ? "" : c.fmt(r)}</td>`).join("") + "</tr>").join("");
    t.querySelector("tbody.rows").innerHTML = tableRows().map((r) => `<tr data-code="${r.code}" tabindex="0">` + COLS.map((c) => `<td>${c.fmt(r)}</td>`).join("") + "</tr>").join("");
  }
  function csvCell(v) {
    if (v === null || v === undefined || (typeof v === "number" && Number.isNaN(v))) return "";
    if (typeof v === "number") return v === Infinity ? "Inf" : Number.isInteger(v) ? String(v) : String(+v.toPrecision(8));
    const s = String(v);
    return /[",\n]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
  }
  function download(name, text) {
    const blob = new Blob([text], { type: "text/csv;charset=utf-8" });
    const a = document.createElement("a");
    a.href = URL.createObjectURL(blob); a.download = name;
    document.body.appendChild(a); a.click(); a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 1000);
  }
  const citation = "# Source: Fabre (2025) Shortfall of Domestic Resources to Eradicate Extreme Poverty by 2030, JDS, https://doi.org/10.1080/00220388.2025.2493807; World Bank PIP Sept. 2026 (2021 PPP)";
  function downloadTable() {
    const rows = aggRows().concat(tableRows());
    const lines = ["# Parameters: " + paramsText(PARAMS), citation,
      "# Cap: 'insufficient' = confiscating all income above the poverty line cannot close the gap; 'none' = no poverty. Aggregates are population-weighted.",
      COLS.map((c) => csvCell(c.label)).join(",")];
    rows.forEach((r) => lines.push(COLS.map((c) => csvCell(c.csv(r))).join(",")));
    download(`poverty_table_${paramsSlug(PARAMS)}.csv`, lines.join("\n") + "\n");
  }

  /* ---------- country ---------- */
  function distYears(c) {
    const ys = new Set([2025, 2030, 2035, PARAMS.year]);
    return Array.from(ys).filter((y) => y > c.year).sort((a, b) => a - b);
  }
  function renderCountry() {
    const c = BY_CODE[UI.country], p = PARAMS;
    const r = ROWS.find((x) => x.code === c.code);
    $("dist-title").textContent = `${c.name}: daily ${c.welfare} per person by percentile`;
    const years = distYears(c);
    const seq = ["--seq-300", "--seq-400", "--seq-500", "--seq-600", "--seq-700"].map(css);
    const textColor = css("--text-secondary"), grid = css("--grid"), surface = css("--surface-1");
    const x = Array.from({ length: 100 }, (_, i) => i + 1);
    const traces = [];
    const surveyD = M.distribution(c, p.scenario, "survey", p.hfce);
    traces.push({ x, y: Array.from(surveyD.avg), name: `${c.year} (survey)`, mode: "lines", line: { color: css("--text-muted"), width: 1.5, dash: "dot" }, hovertemplate: `${c.year} survey: $%{y:.2f}<extra></extra>` });
    const dists = {};
    years.forEach((y, i) => {
      const d = M.distribution(c, p.scenario, y, p.hfce);
      dists[y] = d;
      const col = seq[Math.min(seq.length - 1, Math.round(i * (seq.length - 1) / Math.max(1, years.length - 1)))];
      const label = y === 2025 ? "2025 (observed growth)" : `${y}`;
      traces.push({ x, y: Array.from(d.avg), name: label + (y === p.year ? " — horizon" : ""), mode: "lines", line: { color: col, width: y === p.year ? 2.5 : 1.5 }, hovertemplate: `${y}: $%{y:.2f}<extra></extra>` });
    });
    const hd = dists[p.year] || surveyD;
    // Horizontal lines: selected poverty line, taxation threshold, antipoverty cap, other standard lines (merged when equal)
    const hl = [{ v: p.line, color: css("--highlight"), dash: "dash", width: 1.5, text: `poverty line $${fmtNum(p.line, 2)}` },
      { v: p.threshold, color: textColor, dash: "dash", width: 1, text: `taxation threshold $${fmtNum(p.threshold, 2)}` }];
    if (r.cap_status === "ok" && isNum(r.cap)) hl.push({ v: r.cap, color: css("--series-1"), dash: "dashdot", width: 1, text: `antipoverty cap ${fmtUsd(r.cap)}` });
    STD_LINES.forEach((v) => { if (hl.every((h) => Math.abs(h.v - v) > 0.005)) hl.push({ v, color: css("--border"), dash: "dot", width: 1, text: `$${fmtNum(v, 2)}` }); });
    const all = [surveyD].concat(Object.values(dists));
    let yaxis = { title: { text: "$/day per person (2021 PPP)", font: { size: 12 } }, gridcolor: grid, zeroline: false, tickfont: { size: 11 }, color: textColor, autorange: false };
    let pos, minGap;
    if (UI.log) {
      const vals = all.map((d) => d.avg[0]).concat(all.map((d) => d.avg[99]), hl.map((h) => h.v));
      const lo = Math.min(...vals) * 0.85, hi = Math.max(...vals) * 1.15;
      const ticks = [0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 1e4, 2e4, 5e4, 1e5, 2e5, 5e5, 1e6].filter((t) => t >= lo && t <= hi);
      const step = ticks.length > 12 ? 3 : 1; // fewer ticks for very long horizons
      const tk = ticks.filter((t, i) => i % step === 0);
      Object.assign(yaxis, { type: "log", range: [Math.log10(lo), Math.log10(hi)], tickvals: tk, ticktext: tk.map((t) => "$" + fmtNum(t, t < 1 ? 1 : 0)) });
      pos = (v) => Math.log10(v); minGap = 0.045 * (Math.log10(hi) - Math.log10(lo));
    } else {
      const top = Math.max(hd.avg[98], p.threshold, p.line) * 1.15;
      Object.assign(yaxis, { type: "linear", tickprefix: "$", range: [0, top] });
      pos = (v) => v; minGap = 0.045 * top;
    }
    // one label per line, at the right end; skip a label too close to an already placed one (lines listed by priority)
    const shapes = [], ann = [], placed = [];
    hl.forEach((h) => {
      shapes.push({ type: "line", xref: "paper", x0: 0, x1: 1, y0: h.v, y1: h.v, line: { color: h.color, width: h.width, dash: h.dash } });
      const y = pos(h.v);
      if (y < yaxis.range[0] || y > yaxis.range[1] || placed.some((q) => Math.abs(q - y) < minGap)) return;
      placed.push(y);
      ann.push({ xref: "paper", x: 0.86, y, yref: "y", text: h.text, showarrow: false, xanchor: "right", yanchor: "bottom", font: { size: 11, color: textColor }, bgcolor: surface, opacity: 0.9 });
    });
    const layout = {
      margin: { l: 64, r: 16, t: 10, b: 46 }, paper_bgcolor: surface, plot_bgcolor: surface, separators: ".\u202f",
      xaxis: { title: { text: "Percentile of the distribution", font: { size: 12 } }, range: [0.5, 100.5], gridcolor: grid, zeroline: false, tickfont: { size: 11 }, color: textColor, dtick: 10 },
      yaxis, shapes, annotations: ann, hovermode: "x unified",
      legend: { orientation: "h", y: -0.18, font: { size: 12, color: css("--text-primary") } },
      font: { family: getComputedStyle(document.body).fontFamily, color: textColor },
      hoverlabel: { bgcolor: surface, bordercolor: css("--border"), font: { color: css("--text-primary") } },
    };
    $("dist-sub").textContent = `Observed growth to ${c.growth_start}, then ${projLabel(p)}${p.hfce ? ", survey rescaled to HFCE" : ""}.` + (UI.log ? "" : " Linear scale: the top percentile may be off the chart (zoom out to see it).");
    Plotly.react("dist", traces, layout, { responsive: true, displaylogo: false });

    const ind = [
      [`Poverty rate ($${fmtNum(p.line, 2)})`, fmtPct(r.rate)],
      ["People in poverty", fmtPop(r.poor)],
      ["Poverty gap", fmtGap(r.gap) + " of mean"],
      ["Antipoverty cap", fmtCap(r)],
      [`Antipoverty tax above $${fmtNum(p.threshold, 2)}`, fmtTax(r) + (r.tax > 100 ? ' <span class="flag">insufficient</span>' : "")],
      [`Income floor (${fmtNum(p.rate, p.rate % 1 ? 1 : 0)}% tax)`, fmtUsd(r.floor) + (r.floor < p.line ? ' <span class="flag">below line</span>' : "")],
      [`Mean ${c.welfare}`, fmtUsd(r.mean)],
      [`Population ${p.year}`, fmtPop(r.pop)],
    ];
    $("country-ind").innerHTML = ind.map(([k, v]) => `<dt>${esc(k)}</dt><dd>${v}</dd>`).join("");
    const d = M.distribution(c, p.scenario, p.year, p.hfce);
    $("country-lines").innerHTML = "<thead><tr><th scope='col'>Line ($/day)</th><th scope='col'>Rate</th><th scope='col'>Gap (% mean)</th></tr></thead><tbody>" +
      Array.from(new Set(STD_LINES.concat([p.line]))).sort((a, b) => a - b).map((l) => `<tr><td>${fmtNum(l, 2)}</td><td>${fmtPct(100 * M.povertyRate(d, l))}</td><td>${fmtGap(100 * M.povertyGap(d, l) / d.mean)}</td></tr>`).join("") + "</tbody>";
    const meta = [
      ["Survey year", c.year], ["Welfare measure", c.welfare], ["Coverage", c.level],
      ["Population at survey", fmtPop(c.pop_year)],
      [`Observed growth ${Math.min(c.year, c.growth_start)}–${c.growth_start}`, c.growth_start > c.year ? fmtPct2((c.growth_to_start - 1) * 100) : "none (no GDP data or recent survey)"],
      ["Trend growth 2015–2025", c.trend == null ? "n/a" : fmtPct2(c.trend * 100) + "/yr"],
      ["Autoregressive growth", c.reg == null ? "n/a" : fmtPct2((c.reg - 1) * 100) + "/yr"],
      ["HFCE / survey mean", fmtNum(c.scaling_factor, 2)],
      ["Groups", [c.ssa ? "Sub-Saharan Africa" : "", c.lic ? "Low-income" : ""].filter(Boolean).join(", ") || "–"],
    ];
    $("country-meta").innerHTML = meta.map(([k, v]) => `<dt>${esc(k)}</dt><dd>${esc(v)}</dd>`).join("");
  }
  function fmtPct2(x) { return (x >= 0 ? "+" : "") + fmtNum(x, 1) + "%"; }
  function downloadCountry() {
    const c = BY_CODE[UI.country], p = PARAMS;
    const years = distYears(c);
    const s = M.distribution(c, p.scenario, "survey", p.hfce);
    const ds = years.map((y) => M.distribution(c, p.scenario, y, p.hfce));
    const lines = [`# ${c.name} (${c.code}), ${c.welfare} per capita in $/day (2021 PPP), survey year ${c.year}`,
      `# Projection: observed growth from the survey year to ${c.growth_start} then ${projLabel(p)}${p.hfce ? "; survey rescaled to HFCE" : ""}. Parameters: ${paramsText(p)}`, citation,
      ["percentile", "pop_share", `upper_bound_${c.year}`, `avg_${c.year}`].concat(years.map((y) => `avg_${y}`)).join(",")];
    for (let i = 0; i < 100; i++) lines.push([i + 1, s.ps[i], i < 99 ? c.q[i] : "", s.avg[i]].concat(ds.map((d) => d.avg[i])).map(csvCell).join(","));
    download(`distribution_${c.code}_${paramsSlug(p)}.csv`, lines.join("\n") + "\n");
  }

  /* ---------- tabs & events ---------- */
  const TABS = ["map", "table", "country", "about"];
  function selectTab(t, focus) {
    UI.tab = t;
    TABS.forEach((k) => {
      const b = $("tab-" + k), on = k === t;
      b.setAttribute("aria-selected", on); b.tabIndex = on ? 0 : -1;
      $("panel-" + k).hidden = !on;
    });
    if (focus) $("tab-" + t).focus();
    render();
  }
  function render() {
    renderHeadline();
    if (UI.tab === "map") renderMap();
    else if (UI.tab === "table") renderTable();
    else if (UI.tab === "country") renderCountry();
    writeHash();
  }
  let pending = null;
  function update() { if (pending) cancelAnimationFrame(pending); pending = requestAnimationFrame(() => { pending = null; compute(); render(); }); }

  function bind() {
    const presetPair = (presetId, inputId) => {
      $(presetId).addEventListener("change", () => { const v = $(presetId).value; if (v !== "custom") $(inputId).value = v; else $(inputId).focus(); update(); });
      $(inputId).addEventListener("input", () => { syncPreset(presetId, inputId); update(); });
    };
    presetPair("line-preset", "line");
    presetPair("thr-preset", "thr");
    $("growth-preset").addEventListener("change", () => { syncGrowth(); if ($("growth-preset").value === "custom") $("growth").focus(); update(); });
    $("growth").addEventListener("input", () => { const v = parseFloat($("growth").value), pre = { 3: "0.03", 7: "0.07" }[v]; $("growth-preset").value = pre || "custom"; update(); });
    ["rate", "year", "source"].forEach((id) => $(id).addEventListener(id === "rate" ? "input" : "change", () => { syncGrowth(); update(); }));
    $("map-ind").addEventListener("change", () => { UI.ind = $("map-ind").value; render(); });
    TABS.forEach((k, i) => {
      $("tab-" + k).addEventListener("click", () => selectTab(k));
      $("tab-" + k).addEventListener("keydown", (e) => {
        if (e.key === "ArrowRight" || e.key === "ArrowLeft") { e.preventDefault(); selectTab(TABS[(i + (e.key === "ArrowRight" ? 1 : TABS.length - 1)) % TABS.length], true); }
      });
    });
    $("tbl").tHead.addEventListener("click", (e) => {
      const b = e.target.closest("button[data-key]"); if (!b) return;
      const k = b.dataset.key;
      UI.sort = { key: k, dir: UI.sort.key === k ? -UI.sort.dir : (COLS.find((c) => c.key === k).type === "str" ? 1 : -1) };
      renderTable();
      const nb = $("tbl").tHead.querySelector(`button[data-key="${k}"]`); if (nb) nb.focus();
    });
    const openRow = (e) => { const tr = e.target.closest("tr[data-code]"); if (!tr) return; UI.country = tr.dataset.code; $("country").value = UI.country; selectTab("country"); };
    $("tbl").querySelector("tbody.rows").addEventListener("click", openRow);
    $("tbl").querySelector("tbody.rows").addEventListener("keydown", (e) => { if (e.key === "Enter") openRow(e); });
    $("tbl-filter").addEventListener("input", () => { UI.filter = $("tbl-filter").value; renderTable(); });
    $("tbl-group").addEventListener("change", () => { UI.group = $("tbl-group").value; renderTable(); });
    $("dl-table").addEventListener("click", downloadTable);
    $("dl-country").addEventListener("click", downloadCountry);
    $("country").addEventListener("change", () => { UI.country = $("country").value; render(); });
    const setAxis = (log) => { UI.log = log; $("axis-log").setAttribute("aria-pressed", log); $("axis-lin").setAttribute("aria-pressed", !log); renderCountry(); };
    $("axis-log").addEventListener("click", () => setAxis(true));
    $("axis-lin").addEventListener("click", () => setAxis(false));
    window.addEventListener("hashchange", () => { readHash(); $("country").value = UI.country; syncGrowth(); compute(); selectTab(UI.tab); });
    $("map-continuous").addEventListener("change", () => { UI.continuous = $("map-continuous").checked; render(); });
  }
  function syncGrowth() {
    const gp = $("growth-preset").value, inp = $("growth");
    inp.disabled = gp === "trend" || gp === "reg"; // the rate stays editable with 3% / 7% (typing switches the preset accordingly)
    if (gp !== "custom" && gp !== "trend" && gp !== "reg") inp.value = +(parseFloat(gp) * 100).toFixed(2);
  }

  async function start() {
    $("year").innerHTML = YEARS.map((y) => `<option value="${y}"${y === 2030 ? " selected" : ""}>${y}</option>`).join("");
    try {
      const res = await fetch("data/countries.json");
      if (!res.ok) throw new Error(res.status + " " + res.statusText);
      DATA = await res.json();
    } catch (e) {
      document.querySelector("main").insertAdjacentHTML("afterbegin", `<p role="alert">Could not load data/countries.json (${esc(e.message)}). If you opened index.html directly from disk, serve the folder over HTTP instead (e.g. <code>python3 -m http.server</code>).</p>`);
      return;
    }
    M.init(DATA.meta);
    DATA.countries.forEach((c) => { BY_CODE[c.code] = c; });
    $("country").innerHTML = DATA.countries.slice().sort((a, b) => a.name.localeCompare(b.name)).map((c) => `<option value="${c.code}">${esc(c.name)}</option>`).join("");
    $("data-date").textContent = "Data generated " + DATA.meta.generated;
    readHash();
    $("country").value = UI.country;
    syncGrowth();
    bind();
    compute();
    selectTab(UI.tab);
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start); else start();
})();
