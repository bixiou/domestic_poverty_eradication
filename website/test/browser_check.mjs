// Optional headless check (needs google-chrome): serve website/ with `python3 -m http.server 8765`, then `node test/browser_check.mjs http://localhost:8765/index.html`. Screenshots go to /tmp/shots/.
// Headless check: loads the page, exercises the UI, reports JS errors, takes screenshots.
import { spawn } from "node:child_process";
import fs from "node:fs";
const port = 9333;
const chrome = spawn("google-chrome", ["--headless=new", "--no-sandbox", "--disable-gpu", "--hide-scrollbars", `--remote-debugging-port=${port}`, "--user-data-dir=/tmp/chrome-prof-test", "--window-size=1300,1400", "about:blank"], { stdio: "ignore" });
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
fs.mkdirSync("/tmp/shots", { recursive: true });
await sleep(2500);
const targets = await (await fetch(`http://localhost:${port}/json`)).json();
const ws = new WebSocket(targets.find((t) => t.type === "page").webSocketDebuggerUrl);
await new Promise((r) => ws.addEventListener("open", r));
let id = 0; const pending = {}; const errors = [];
ws.addEventListener("message", (m) => {
  const msg = JSON.parse(m.data);
  if (msg.id && pending[msg.id]) { pending[msg.id](msg); delete pending[msg.id]; }
  if (msg.method === "Runtime.exceptionThrown") errors.push(msg.params.exceptionDetails.exception?.description || msg.params.exceptionDetails.text);
  if (msg.method === "Runtime.consoleAPICalled" && ["error", "warning"].includes(msg.params.type)) errors.push("console." + msg.params.type + ": " + msg.params.args.map((a) => a.value ?? a.description).join(" "));
  if (msg.method === "Log.entryAdded" && msg.params.entry.level === "error") errors.push("log: " + msg.params.entry.text + " " + (msg.params.entry.url || ""));
});
const send = (method, params = {}) => new Promise((r) => { const i = ++id; pending[i] = r; ws.send(JSON.stringify({ id: i, method, params })); });
const ev = async (expr) => (await send("Runtime.evaluate", { expression: expr, awaitPromise: true, returnByValue: true })).result?.result?.value;
const shot = async (name, h = 1400) => { await send("Emulation.setDeviceMetricsOverride", { width: name.includes("mobile") ? 400 : 1300, height: h, deviceScaleFactor: 1, mobile: name.includes("mobile") }); await sleep(800); const r = await send("Page.captureScreenshot", { format: "png" }); fs.writeFileSync(`/tmp/shots/${name}.png`, Buffer.from(r.result.data, "base64")); };
await send("Runtime.enable"); await send("Log.enable"); await send("Page.enable");
const t0 = Date.now();
await send("Page.navigate", { url: process.argv[2] });
for (let i = 0; i < 60; i++) { await sleep(250); if (await ev("!!document.querySelector('#map .main-svg') && document.getElementById('h-rate').textContent !== '–'")) break; }
console.log("map rendered after", Date.now() - t0, "ms; headline:", await ev("document.getElementById('h-rate').textContent + ' / ' + document.getElementById('h-tax').textContent"));
await shot("map");
for (const ind of ["rate", "gap", "cap", "floor", "mean"]) { await ev(`(()=>{const s=document.getElementById('map-ind'); s.value='${ind}'; s.dispatchEvent(new Event('change'));})()`); await sleep(400); }
await shot("map_mean");
await ev(`(()=>{const c=document.getElementById('map-continuous'); c.checked=true; c.dispatchEvent(new Event('change'));})()`); await sleep(300);
for (const ind of ["rate", "gap", "tax", "cap", "floor", "mean"]) { await ev(`(()=>{const s=document.getElementById('map-ind'); s.value='${ind}'; s.dispatchEvent(new Event('change'));})()`); await sleep(300); }
await shot("map_continuous_mean");
await ev(`(()=>{const c=document.getElementById('map-continuous'); c.checked=false; c.dispatchEvent(new Event('change'));})()`); await sleep(300);
for (const ind of ["tax", "floor", "rate", "gap"]) { await ev(`(()=>{const s=document.getElementById('map-ind'); s.value='${ind}'; s.dispatchEvent(new Event('change'));})()`); await sleep(400); await shot("map_" + ind); }
await ev(`(()=>{const s=document.getElementById('map-ind'); s.value='cap'; s.dispatchEvent(new Event('change'));})()`); await sleep(500); await shot("map_cap");
// change parameters
await ev(`(()=>{const s=document.getElementById('growth-preset'); s.value='custom'; s.dispatchEvent(new Event('change')); const g=document.getElementById('growth'); g.value='5'; g.dispatchEvent(new Event('input'));})()`); await sleep(300);
await ev(`(()=>{const s=document.getElementById('line-preset'); s.value='4.2'; s.dispatchEvent(new Event('change'));})()`); await sleep(300);
await ev(`(()=>{const s=document.getElementById('year'); s.value='2050'; s.dispatchEvent(new Event('change'));})()`); await sleep(300);
await ev(`(()=>{const s=document.getElementById('source'); s.value='hfce'; s.dispatchEvent(new Event('change'));})()`); await sleep(300);
console.log("after params:", await ev("document.getElementById('params-summary').textContent + ' | ' + document.getElementById('h-rate').textContent + ' | hash ' + location.hash"));
for (const g of ["trend", "reg", "0.07"]) { await ev(`(()=>{const s=document.getElementById('growth-preset'); s.value='${g}'; s.dispatchEvent(new Event('change'));})()`); await sleep(300); }
await ev(`(()=>{const s=document.getElementById('year'); s.value='2100'; s.dispatchEvent(new Event('change'));})()`); await sleep(300);
console.log("2100:", await ev("document.getElementById('h-rate').textContent"));
await ev("document.getElementById('tab-table').click()"); await sleep(500);
await ev("document.querySelector('#tbl thead button[data-key=tax]').click()"); await sleep(200);
await ev("document.querySelector('#tbl thead button[data-key=cap]').click()"); await sleep(200);
await ev("document.querySelector('#tbl thead button[data-key=name]').click()"); await sleep(200);
console.log("table rows:", await ev("document.querySelectorAll('#tbl tbody.rows tr').length"), "first:", await ev("document.querySelector('#tbl tbody.rows tr td').textContent"));
// downloads (intercept)
await ev(`(()=>{window.__dl=[]; const o=URL.createObjectURL; URL.createObjectURL=(b)=>{window.__dl.push(b); return o(b);};})()`);
await ev("document.getElementById('dl-table').click()");
await ev("document.querySelector('#tbl tbody.rows tr').click()"); await sleep(600);
await ev("document.getElementById('dl-country').click()");
console.log("downloads:", await ev("Promise.all(window.__dl.map(b=>b.text())).then(t=>t.map(x=>x.split('\\n').slice(0,6).join('\\n')).join('\\n-----\\n'))"));
await shot("country_default_linear");
const setSel = (id, v) => ev(`(()=>{const s=document.getElementById('${id}'); s.value='${v}'; s.dispatchEvent(new Event('change'));})()`);
await setSel("unit", "month"); await setSel("currency", "lcu"); await sleep(500);
await shot("country_lcu_month");
console.log("LCU/month panel:", await ev("document.getElementById('country-ind').innerText.replace(/\\n/g,' | ')"));
await ev("document.getElementById('dl-country').click()");
console.log("CSV head:", await ev("window.__dl[window.__dl.length-1].text().then(t=>t.split('\\n').slice(0,6).join('\\n'))"));
await setSel("currency", "usd"); await setSel("unit", "year"); await sleep(300);
await ev("document.getElementById('axis-log').click()"); await sleep(400);
await shot("country_usd_year_log");
await setSel("country", "YEM"); await sleep(400);
console.log("YEM:", await ev("document.getElementById('dist-sub').textContent + ' | lcu disabled=' + document.querySelector('#currency option[value=lcu]').disabled"));
console.log("hash:", await ev("location.hash"));
await setSel("unit", "day"); await setSel("currency", "ppp"); await ev("document.getElementById('axis-lin').click()"); await sleep(400);
await shot("country_lin");
// reset to defaults via hash
await send("Page.navigate", { url: process.argv[2].split("#")[0] + "#tab=country&c=IND" }); await sleep(3000);
await ev("document.getElementById('axis-log').click()"); await sleep(300);
await shot("country_ind");
await send("Page.navigate", { url: process.argv[2].split("#")[0] + "#tab=map" }); await sleep(3000);
await shot("mobile", 1600);
console.log("ERRORS:", errors.length ? errors : "none");
ws.close(); chrome.kill();
process.exit(0);
