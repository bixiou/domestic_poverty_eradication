import io, os, re, sys, requests, pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment
from openpyxl.worksheet.datavalidation import DataValidation
from openpyxl.workbook.defined_name import DefinedName
from openpyxl.worksheet.formula import ArrayFormula
from openpyxl.utils import get_column_letter as L_

out = sys.argv[1]
API = "https://api.worldbank.org/v2"

def get(url):
    r = requests.get(url, timeout=60); r.raise_for_status(); return r.json()

meta = get(f"{API}/country?format=json&per_page=400")[1]
countries = pd.DataFrame([{"iso3": c["id"], "iso2": c["iso2Code"], "country": c["name"], "income": c["incomeLevel"]["id"], "region": c["region"]["id"]}
                          for c in meta if c["region"]["id"] != "NA"])  # drop aggregates

def indicator(code, name):
    rows = get(f"{API}/country/all/indicator/{code}?format=json&per_page=20000&mrnev=1")[1]
    return pd.DataFrame([{"iso3": r["countryiso3code"], name: r["value"], name + "_year": int(r["date"])} for r in rows if r["value"] is not None])

df = countries
for code, name in [("SP.POP.TOTL", "pop"), ("NY.GDP.PCAP.CD", "gdp_nom"), ("NY.GDP.PCAP.PP.CD", "gdp_ppp")]:
    df = df.merge(indicator(code, name), on="iso3", how="left")

# Impute from IMF WEO (DataMapper API, latest year <= 2025) when WB value is missing, or older than 2024 and IMF has a more recent year
df["imf"] = ""
for code, name, scale in [("LP", "pop", 1e6), ("NGDPDPC", "gdp_nom", 1), ("PPPPC", "gdp_ppp", 1)]:
    imf = get(f"https://www.imf.org/external/datamapper/api/v1/{code}")["values"][code]
    for i in df.index[df[name].isna() | (df[name + "_year"] < 2024)]:
        s = {int(y): v for y, v in imf.get(df.at[i, "iso3"], {}).items() if v is not None and int(y) <= 2025}
        if s and (pd.isna(df.at[i, name]) or max(s) > df.at[i, name + "_year"]):
            print("IMF:", df.at[i, "iso3"], name, df.at[i, name], df.at[i, name + "_year"], "->", s[max(s)] * scale, max(s))
            y = max(s); df.at[i, name] = s[y] * scale; df.at[i, name + "_year"] = y
            df.at[i, "imf"] = (df.at[i, "imf"] + ", " if df.at[i, "imf"] else "") + name
df = df.merge(indicator("SI.POV.LMIC.GP", "pg"), on="iso3", how="left")  # Poverty gap at $4.20 a day (2021 PPP) (%), last available year
# Wealth tax base: WID data (2022, current US$) used in global_tax_attitudes/code_global/intl_plan_replication.R. w = total wealth of those above threshold, n = their number
wid = pd.read_csv(os.path.join(os.path.dirname(os.path.abspath(__file__)), "../../global_tax_attitudes/data/wealth_tax_wid.csv"))
wid = wid[wid.year == 2022]
for thr, name in [(5e6, "taxable_5M"), (1e8, "taxable_100M")]:
    x = wid[wid.threshold == thr].assign(**{name: lambda d: d.w - d.threshold * d.n})[["iso", name]]
    x.loc[x[name] <= 0, name] = None  # Egypt has negative wealth above 100M (data issue), removed as in the R script
    df = df.merge(x.rename(columns={"iso": "iso2"}), on="iso2", how="left")
gdp22 = get(f"{API}/country/all/indicator/NY.GDP.MKTP.CD?format=json&per_page=20000&date=2022")[1]
df = df.merge(pd.DataFrame([{"iso3": r["countryiso3code"], "gdp_2022": r["value"]} for r in gdp22 if r["value"] is not None]), on="iso3", how="left")
# ODA provided: OECD DAC1, ODA grant equivalent, current US$ million, latest year (DAC and non-DAC reporting countries)
oda = pd.read_csv(io.StringIO(requests.get("https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC1@DF_DAC1,/._Z.1010._Z.1160.USD.V?startPeriod=2022&format=csvfile", headers={"User-Agent": "curl/8.0"}, timeout=120).text))
oda = oda.sort_values("TIME_PERIOD").groupby("DONOR").last().reset_index()
df = df.merge(oda.rename(columns={"DONOR": "iso3", "OBS_VALUE": "oda_given", "TIME_PERIOD": "oda_given_year"})[["iso3", "oda_given", "oda_given_year"]], on="iso3", how="left")
# ODA received: WB WDI net ODA received (current US$), latest year if >= 2019
rec = indicator("DT.ODA.ODAT.CD", "oda_received"); rec = rec[rec.oda_received_year >= 2019]
df = df.merge(rec, on="iso3", how="left")
df = df.sort_values(["gdp_nom", "country"], ascending=[False, True], na_position="last").reset_index(drop=True)
df.to_csv(out.replace(".xlsx", "_raw.csv"), index=False)

labels = {"LIC": "Low income", "LMC": "Lower middle income", "UMC": "Upper middle income", "HIC": "High income", "INX": "Not classified"}
bold = Font(bold=True); hdr_fill = PatternFill("solid", fgColor="DDEBF7"); par_fill = PatternFill("solid", fgColor="FFF2CC")
wb = Workbook()
ws = wb.active; ws.title = "Transfers"

ws["A1"] = "Global reallocation: each country transfers a share of its base GDP, pooled and redistributed to all countries in proportion to population"; ws["A1"].font = Font(bold=True, size=12)
ws["A2"] = "Transfer size (% of base)"; ws["C2"] = 0.01; ws["C2"].number_format = "0.0%"
ws["A3"] = "Base variable"; ws["C3"] = "GDP pc nominal"
ws["A4"] = "World average of base (pop-weighted)"
for c in ("C2", "C3"): ws[c].fill = par_fill; ws[c].font = bold
ws["D2"] = "← edit (yellow cells)"; ws["D3"] = "← pick from list: any header in E6:G6 (put another variable in column G to use it)"
dv = DataValidation(type="list", formula1="=$E$6:$G$6", allow_blank=False); ws.add_data_validation(dv); dv.add("C3")

H = 6; tot = H + 1; first = H + 2; last = H + 1 + len(df)
headers = ["ISO3", "Country", "Income group (WB)", "Population", "GDP pc nominal", "GDP pc PPP", "Other base (optional)", "Base used",
           "Net transfer to rest of the world (% of GDP)", "Net transfer to rest of the world (bn)",
           "Transfer to LICs (% of GDP)", "Transfer to LICs + LMICs (% of GDP)",
           "Net transfer to rest of the world (% of GDP)", "Net transfer to rest of the world (bn)",
           "Transfer to LICs (% of GDP)", "Transfer to LICs + LMICs (% of GDP)",
           "Net transfer to rest of the world (% of GDP)", "Net transfer to rest of the world (bn)",
           "Transfer to LICs (% of GDP)", "Transfer to LICs + LMICs (% of GDP)",
           "Wealth tax revenue, 2% > $5M (% of GDP)", "Wealth tax revenue, 2% > $100M (% of GDP)",
           *[f"{a} alloc.: net transfer to rest of the world (% of wealth tax revenue > ${thr})" for thr in ("5M", "100M") for a in ("Pop.", "Poverty-gap", "Phase-out")],
           "ODA provided (% of GDP, OECD)", "Net ODA received (% of GDP, WB)", "Net ODA (provided − received, % of GDP)",
           "Net transfer pop. alloc. (col. I) + net ODA (% of GDP)",
           "Year pop", "Year GDP nominal", "Year GDP PPP", "Imputed from IMF WEO"]
for j, h in enumerate(headers, 1):
    c = ws.cell(H, j, h); c.font = bold; c.fill = hdr_fill; c.alignment = Alignment(wrap_text=True, vertical="top")
ws.row_dimensions[H].height = 120
pg_fill = PatternFill("solid", fgColor="E2EFDA"); po_fill = PatternFill("solid", fgColor="E4DFEC"); oda_fill = PatternFill("solid", fgColor="EDEDED")
for j in range(13, 17): ws.cell(H, j).fill = pg_fill
for j in range(17, 21): ws.cell(H, j).fill = po_fill
ws["I5"] = "Allocation in proportion to population"; ws["M5"] = "Allocation in proportion to statutory poverty gap (tab 'Poverty gap')"
ws["Q5"] = "Allocation per capita with phase-out (tab 'Phase-out')"
ws["I5"].font = ws["M5"].font = ws["Q5"].font = bold; ws["I5"].fill = hdr_fill; ws["M5"].fill = pg_fill; ws["Q5"].fill = po_fill
wt_fill = PatternFill("solid", fgColor="FCE4D6")
for j in range(21, 29): ws.cell(H, j).fill = wt_fill
ws["U5"] = "Wealth tax (tab 'Wealth tax'); share of its revenue transferred: W-Y for > $5M, Z-AB for > $100M"; ws["U5"].font = bold; ws["U5"].fill = wt_fill
for j in range(29, 33): ws.cell(H, j).fill = oda_fill
ws["AC5"] = "Official development assistance (tab 'ODA')"; ws["AC5"].font = bold; ws["AC5"].fill = oda_fill

rng = lambda col: f"${col}${first}:${col}${last}"
D, Hb, I, J, N, R = rng("D"), rng("H"), rng("I"), rng("J"), rng("N"), rng("R")
ws["C4"] = f"=SUMPRODUCT({D},{Hb})/SUMPRODUCT({D},--ISNUMBER({Hb}))"; ws["C4"].number_format = "#,##0"; ws["C4"].font = bold
ws["A5"] = "World total base GDP"; ws["C5"] = f"=SUMPRODUCT({D},{Hb})"; ws["C5"].number_format = "#,##0,,,\" bn\""; ws["C5"].font = bold
wb.defined_names["rate"] = DefinedName("rate", attr_text="Transfers!$C$2")
wb.defined_names["base_avg"] = DefinedName("base_avg", attr_text="Transfers!$C$4")
wb.defined_names["base_total"] = DefinedName("base_total", attr_text="Transfers!$C$5")

for i, r in df.iterrows():
    n = first + i
    ws.cell(n, 1, r.iso3); ws.cell(n, 2, r.country); ws.cell(n, 3, labels.get(r.income, r.income))
    for j, k in [(4, "pop"), (5, "gdp_nom"), (6, "gdp_ppp")]:
        if pd.notna(r[k]): ws.cell(n, j, float(r[k]))
    for j, k in [(33, "pop_year"), (34, "gdp_nom_year"), (35, "gdp_ppp_year")]:
        if pd.notna(r[k]): ws.cell(n, j, int(r[k]))
    if r.imf: ws.cell(n, 36,", ".join({"pop": "population", "gdp_nom": "GDP pc nominal", "gdp_ppp": "GDP pc PPP"}[k] for k in r.imf.split(", ")))
    ws.cell(n, 8, f'=IF(AND(ISNUMBER(D{n}),ISNUMBER(INDEX(E{n}:G{n},MATCH($C$3,$E${H}:$G${H},0)))),INDEX(E{n}:G{n},MATCH($C$3,$E${H}:$G${H},0)),"")')
    ws.cell(n, 9, f'=IF(ISNUMBER(H{n}),rate*(1-base_avg/H{n}),"")')
    ws.cell(n, 10, f'=IF(ISNUMBER(H{n}),rate*(H{n}-base_avg)*D{n}/1E9,"")')
    ws.cell(n, 11, f'=IF(ISNUMBER(I{n}),MAX(I{n},0)*share_LIC,"")')
    ws.cell(n, 12, f'=IF(ISNUMBER(I{n}),MAX(I{n},0)*(share_LIC+share_LMIC),"")')
    ws.cell(n, 13, f'=IF(ISNUMBER(H{n}),rate*(1-base_total*\'Poverty gap\'!P{n}/(H{n}*D{n})),"")')
    ws.cell(n, 14, f'=IF(ISNUMBER(H{n}),rate*(H{n}*D{n}-base_total*\'Poverty gap\'!P{n})/1E9,"")')
    ws.cell(n, 15, f'=IF(ISNUMBER(M{n}),MAX(M{n},0)*pg_share_LIC,"")')
    ws.cell(n, 16, f'=IF(ISNUMBER(M{n}),MAX(M{n},0)*(pg_share_LIC+pg_share_LMIC),"")')
    ws.cell(n, 17, f'=IF(ISNUMBER(H{n}),rate*(1-base_total*\'Phase-out\'!G{n}/(H{n}*D{n})),"")')
    ws.cell(n, 18, f'=IF(ISNUMBER(H{n}),rate*(H{n}*D{n}-base_total*\'Phase-out\'!G{n})/1E9,"")')
    ws.cell(n, 19, f'=IF(ISNUMBER(Q{n}),MAX(Q{n},0)*po_share_LIC,"")')
    ws.cell(n, 20, f'=IF(ISNUMBER(Q{n}),MAX(Q{n},0)*(po_share_LIC+po_share_LMIC),"")')
    ws.cell(n, 21, f"='Wealth tax'!Q{n}"); ws.cell(n, 22, f"='Wealth tax'!R{n}")
    for j0, rev in ((23, "U"), (26, "V")):  # transfer in money / wealth tax revenue in money (robust to the base chosen)
        for j, col in zip(range(j0, j0 + 3), "IMQ"):
            ws.cell(n, j, f'=IFERROR({col}{n}*H{n}/({rev}{n}*E{n}),"")')
    ws.cell(n, 29, f'=IF(AND(ISNUMBER(ODA!C{n}),N(E{n})*N(D{n})>0),ODA!C{n}*1E6/(E{n}*D{n}),"")')
    ws.cell(n, 30, f'=IF(AND(ISNUMBER(ODA!E{n}),N(E{n})*N(D{n})>0),ODA!E{n}/(E{n}*D{n}),"")')
    ws.cell(n, 31, f'=IF(OR(ISNUMBER(AC{n}),ISNUMBER(AD{n})),N(AC{n})-N(AD{n}),"")')
    ws.cell(n, 32, f'=IF(ISNUMBER(I{n}),I{n}*H{n}/E{n}+N(AE{n}),"")')

ws.cell(tot, 2, "World (total)").font = bold
ws.cell(tot, 4, f"=SUM({D})")
ws.cell(tot, 5, f"=SUMPRODUCT({D},{rng('E')})/SUMPRODUCT({D},--ISNUMBER({rng('E')}))")
ws.cell(tot, 6, f"=SUMPRODUCT({D},{rng('F')})/SUMPRODUCT({D},--ISNUMBER({rng('F')}))")
ws.cell(tot, 8, "=base_avg")
gdp_tot = f"SUMPRODUCT({D},{Hb})"
ws.cell(tot, 9, f'=SUMIF({J},">0")*1E9/{gdp_tot}')   # gross flow from net payers, % of world GDP
ws.cell(tot, 10, f'=SUMIF({J},">0")')
ws.cell(tot, 11, f'=SUMIF({J},">0")*share_LIC*1E9/{gdp_tot}')
ws.cell(tot, 12, f'=SUMIF({J},">0")*(share_LIC+share_LMIC)*1E9/{gdp_tot}')
ws.cell(tot, 13, f'=SUMIF({N},">0")*1E9/{gdp_tot}')
ws.cell(tot, 14, f'=SUMIF({N},">0")')
ws.cell(tot, 15, f'=SUMIF({N},">0")*pg_share_LIC*1E9/{gdp_tot}')
ws.cell(tot, 16, f'=SUMIF({N},">0")*(pg_share_LIC+pg_share_LMIC)*1E9/{gdp_tot}')
ws.cell(tot, 17, f'=SUMIF({R},">0")*1E9/{gdp_tot}')
ws.cell(tot, 18, f'=SUMIF({R},">0")')
ws.cell(tot, 19, f'=SUMIF({R},">0")*po_share_LIC*1E9/{gdp_tot}')
ws.cell(tot, 20, f'=SUMIF({R},">0")*(po_share_LIC+po_share_LMIC)*1E9/{gdp_tot}')
ws.cell(tot, 21, f"='Wealth tax'!Q{tot}"); ws.cell(tot, 22, f"='Wealth tax'!R{tot}")
for j0, world in ((23, "wt_world_5M"), (26, "wt_world_100M")):
    for j, col in zip(range(j0, j0 + 3), "IMQ"):
        ws.cell(tot, j, f'={col}{tot}*base_total/({world}*1E9)')
gdp_nom_tot = f"SUMPRODUCT({D},{rng('E')})"
ws.cell(tot, 29, f"=SUMPRODUCT(--ISNUMBER({rng('AC')}),{rng('AC')},{D},{rng('E')})/{gdp_nom_tot}")
ws.cell(tot, 30, f"=SUMPRODUCT(--ISNUMBER({rng('AD')}),{rng('AD')},{D},{rng('E')})/{gdp_nom_tot}")
ws.cell(tot, 31, f"=AC{tot}-AD{tot}")
ws.cell(tot, 32, f"=I{tot}*base_total/{gdp_nom_tot}+AC{tot}")  # gross flows: from net payers of col. I + all ODA provided
for j in range(1, 37): ws.cell(tot, j).font = bold

for n in [tot] + list(range(first, last + 1)):
    ws.cell(n, 4).number_format = "#,##0"
    for j in (5, 6, 7, 8): ws.cell(n, j).number_format = "#,##0"
    for j in (9, 11, 12, 13, 15, 16, 17, 19, 20, 29, 30, 31, 32): ws.cell(n, j).number_format = "0.00%"
    for j in (10, 14, 18): ws.cell(n, j).number_format = "#,##0.0"
    for j in (21, 22): ws.cell(n, j).number_format = "0.000%"
    for j in range(23, 29): ws.cell(n, j).number_format = "0%"
for j, w in enumerate([7, 32, 20, 14, 12, 12, 12, 12, 16, 14, 14, 16, 16, 14, 14, 16, 16, 14, 14, 16, 13, 13, 16, 16, 16, 16, 16, 16, 14, 14, 14, 16, 8, 10, 10, 18], 1):
    ws.column_dimensions[L_(j)].width = w
ws.freeze_panes = ws.cell(first, 3)
G20 = {"ARG", "AUS", "BRA", "CAN", "CHN", "FRA", "DEU", "IND", "IDN", "ITA", "JPN", "KOR", "MEX", "RUS", "SAU", "ZAF", "TUR", "GBR", "USA", "POL", "ROU"}  # G20 members + Poland, Romania
ws.auto_filter.ref = f"A{H}:AJ{last}"

# Phase-out tab: allocation per capita, phased out linearly between two multiples of the world average base pc, rows aligned with Transfers
po = wb.create_sheet("Phase-out")
PO = lambda col: f"${col}${first}:${col}${last}"
po["A1"] = "Phase-out start (× world avg base pc)"; po["B1"] = 0.9
po["A2"] = "Phase-out end (× world avg base pc)"; po["B2"] = 1.2
po["A3"] = "Phase-out start (base pc)"; po["B3"] = "=B1*Transfers!$C$4"
po["A4"] = "Phase-out end (base pc)"; po["B4"] = "=B2*Transfers!$C$4"
po["D1"] = "Check: China's net transfer (% of base)"; po["E1"] = f"=INDEX(Transfers!$Q${first}:$Q${last},MATCH(\"CHN\",Transfers!$A${first}:$A${last},0))"
po["D2"] = "Check: min net transfer among HICs (% of base)"
po["D3"] = "Allocation per capita for countries below phase-out start (base units)"; po["E3"] = f"=rate*base_total/SUM({PO('F')})"
po["D4"] = "Default (0.9, 1.2) calibrated so that China's net transfer ≈ 0 with GDP pc nominal; use Goal Seek on E1 by changing B1 to recalibrate"
for c in ("B1", "B2"): po[c].fill = par_fill
for c in ("B1", "B2", "B3", "B4", "E1", "E2", "E3"): po[c].font = bold
po["B1"].number_format = po["B2"].number_format = "0.00"; po["B3"].number_format = po["B4"].number_format = po["E3"].number_format = "#,##0"
po["E1"].number_format = po["E2"].number_format = "0.000%"
po["E2"] = ArrayFormula("E2", f"=MIN(IF(Transfers!$C${first}:$C${last}=\"High income\",IF(ISNUMBER(Transfers!$Q${first}:$Q${last}),Transfers!$Q${first}:$Q${last})))")
for nm, ref in [("po_start_pc", "B3"), ("po_end_pc", "B4")]:
    wb.defined_names[nm] = DefinedName(nm, attr_text=f"'Phase-out'!${ref[0]}${ref[1:]}")
for j, h in enumerate(["ISO3", "Country", "Base pc", "Population", "Phase-out factor (1 below start, 0 above end, linear in between)", "Allocation weight (factor × pop)", "Share of pool"], 1):
    c = po.cell(H, j, h); c.font = bold; c.fill = po_fill; c.alignment = Alignment(wrap_text=True, vertical="top")
po.row_dimensions[H].height = 75
for i, r in df.iterrows():
    n = first + i
    po.cell(n, 1, f"=Transfers!A{n}"); po.cell(n, 2, f"=Transfers!B{n}")
    po.cell(n, 3, f"=Transfers!H{n}"); po.cell(n, 4, f'=IF(ISNUMBER(Transfers!D{n}),Transfers!D{n},"")')
    po.cell(n, 5, f'=IF(ISNUMBER(C{n}),MAX(0,MIN(1,(po_end_pc-C{n})/(po_end_pc-po_start_pc))),"")')
    po.cell(n, 6, f'=IF(AND(ISNUMBER(E{n}),ISNUMBER(D{n})),E{n}*D{n},0)')
    po.cell(n, 7, f'=F{n}/SUM({PO("F")})')
    for j in (3, 4, 6): po.cell(n, j).number_format = "#,##0"
    po.cell(n, 5).number_format = "0.00"; po.cell(n, 7).number_format = "0.00%"
po.cell(tot, 2, "World (total)")
po.cell(tot, 6, f"=SUM({PO('F')})").number_format = "#,##0"
po.cell(tot, 7, f"=SUM({PO('G')})").number_format = "0.00%"
for col, w in zip("ABCDEFG", [44, 30, 12, 14, 16, 16, 10]): po.column_dimensions[col].width = w
po.column_dimensions["D"].width = 44
po.freeze_panes = po.cell(first, 3)
po.auto_filter.ref = f"A{H}:G{last}"

# ODA tab: raw amounts, rows aligned with Transfers
od = wb.create_sheet("ODA")
od["A1"] = "Official development assistance: provided (OECD DAC1, ODA grant equivalent, current US$ million, latest year) and net ODA received (WB WDI DT.ODA.ODAT.CD, current US$, latest year ≥ 2019)"; od["A1"].font = Font(bold=True, size=12)
for j, h in enumerate(["ISO3", "Country", "ODA provided (US$ million, grant equivalent, OECD)", "Year ODA provided", "Net ODA received (US$, WB)", "Year ODA received"], 1):
    c = od.cell(H, j, h); c.font = bold; c.fill = oda_fill; c.alignment = Alignment(wrap_text=True, vertical="top")
od.row_dimensions[H].height = 60
for i, r in df.iterrows():
    n = first + i
    od.cell(n, 1, f"=Transfers!A{n}"); od.cell(n, 2, f"=Transfers!B{n}")
    if pd.notna(r.oda_given): od.cell(n, 3, float(r.oda_given)).number_format = "#,##0"; od.cell(n, 4, int(r.oda_given_year))
    if pd.notna(r.oda_received): od.cell(n, 5, float(r.oda_received)).number_format = '#,##0,," M"'; od.cell(n, 6, int(r.oda_received_year))
od.cell(tot, 2, "World (total)"); od.cell(tot, 3, f"=SUM(C{first}:C{last})").number_format = "#,##0"; od.cell(tot, 5, f"=SUM(E{first}:E{last})").number_format = '#,##0,," M"'
for col, w in zip("ABCDEF", [7, 30, 16, 10, 16, 10]): od.column_dimensions[col].width = w
od.freeze_panes = od.cell(first, 3)
od.auto_filter.ref = f"A{H}:F{last}"

# Poverty gap tab: statutory poverty gap predicted from GDP pc (Fabre 2023 policy brief methodology), rows aligned with Transfers
pgs = wb.create_sheet("Poverty gap")
P = lambda col: f"${col}${first}:${col}${last}"
params = [("A1", "Poverty line ($/day, 2021 PPP) of the data", "B1", 4.2),
          ("A2", "World avg GDP pc nominal (pop-weighted)", "B2", f"=Transfers!E{tot}"),
          ("A3", "Phase-out start (× world avg)", "B3", 1),
          ("A4", "Phase-out end (× world avg)", "B4", 2),
          ("A5", "Cap on statutory poverty gap (%) (default: max observed in regression)", "B5", f"=SUMPRODUCT(MAX({P('C')}*{P('H')}))"),
          ("D1", "Coef. constant (a)", "E1", f"=INDEX(LINEST({P('M')},$J${first}:$L${last},FALSE,FALSE),1,3)"),
          ("D2", "Coef. log10 GDP pc (b)", "E2", f"=INDEX(LINEST({P('M')},$J${first}:$L${last},FALSE,FALSE),1,2)"),
          ("D3", "Coef. (log10 GDP pc)² (c)", "E3", f"=INDEX(LINEST({P('M')},$J${first}:$L${last},FALSE,FALSE),1,1)"),
          ("D4", "Weighted R²", "E4", f"=1-SUMPRODUCT({P('J')},{P('J')},({P('I')}-(pg_a+pg_b*{P('G')}+pg_c*{P('G')}^2))^2)/SUMPRODUCT({P('J')},{P('J')},({P('I')}-SUMPRODUCT({P('J')},{P('J')},{P('I')})/SUMPRODUCT({P('J')},{P('J')}))^2)"),
          ("G1", "Phase-out start (GDP pc)", "H1", "=B2*B3"),
          ("G2", "Phase-out end (GDP pc)", "H2", "=B2*B4"),
          ("G3", "Predicted log10 poverty gap at phase-out start", "H3", "=pg_a+pg_b*LOG10(H1)+pg_c*LOG10(H1)^2"),
          ("G4", "Countries in regression", "H4", f"=COUNTIF({P('H')},TRUE)"),
          ("J1", "Statutory poverty gap, world total ($bn PPP/yr)", "K1", f"=SUMPRODUCT({P('N')},{P('F')})/100*B1*365/1E9"),
          ("J2", "Actual poverty gap, countries with data ($bn PPP/yr)", "K2", f"=SUMPRODUCT({P('C')},{P('F')})/100*B1*365/1E9")]
for lc, lab, vc, v in params:
    pgs[lc] = lab; pgs[vc] = v; pgs[vc].font = bold
for c in ("B3", "B4", "B5"): pgs[c].fill = par_fill
pgs["B5"].number_format = "0.0"
for c in ("B2", "H1", "H2", "K1", "K2"): pgs[c].number_format = "#,##0"
for c in ("E1", "E2", "E3", "E4", "H3"): pgs[c].number_format = "0.000"
for nm, ref in [("pg_a", "E1"), ("pg_b", "E2"), ("pg_c", "E3"), ("po_start", "H1"), ("po_end", "H2"), ("pg_ystart", "H3"), ("pg_cap", "B5")]:
    wb.defined_names[nm] = DefinedName(nm, attr_text=f"'Poverty gap'!${ref[0]}${ref[1:]}")
pg_headers = ["ISO3", "Country", "Poverty gap at $4.20/day (%), WB", "Year poverty gap", "GDP pc nominal", "Population", "log10 GDP pc",
              "In regression (PG > 0, GDP pc < phase-out start)", "log10 poverty gap (if in reg.)", "√pop (if in reg.)", "log10 GDP × √pop", "(log10 GDP)² × √pop", "log10 PG × √pop",
              "Statutory poverty gap (%)", "Allocation weight (statutory PG × pop)", "Share of pool"]
for j, h in enumerate(pg_headers, 1):
    c = pgs.cell(H, j, h); c.font = bold; c.fill = pg_fill; c.alignment = Alignment(wrap_text=True, vertical="top")
pgs.row_dimensions[H].height = 75
for i, r in df.iterrows():
    n = first + i
    pgs.cell(n, 1, f"=Transfers!A{n}"); pgs.cell(n, 2, f"=Transfers!B{n}")
    if pd.notna(r.pg): pgs.cell(n, 3, float(r.pg)); pgs.cell(n, 4, int(r.pg_year))
    pgs.cell(n, 5, f'=IF(ISNUMBER(Transfers!E{n}),Transfers!E{n},"")')
    pgs.cell(n, 6, f'=IF(ISNUMBER(Transfers!D{n}),Transfers!D{n},"")')
    pgs.cell(n, 7, f'=IF(ISNUMBER(E{n}),LOG10(E{n}),0)')
    pgs.cell(n, 8, f'=IF(AND(ISNUMBER(C{n}),ISNUMBER(E{n}),ISNUMBER(F{n})),AND(N(C{n})>0,N(E{n})<po_start),FALSE)')
    pgs.cell(n, 9, f'=IF(H{n},LOG10(C{n}),0)')
    pgs.cell(n, 10, f'=IF(H{n},SQRT(F{n}),0)')
    pgs.cell(n, 11, f'=J{n}*G{n}')
    pgs.cell(n, 12, f'=J{n}*G{n}^2')
    pgs.cell(n, 13, f'=J{n}*I{n}')
    pgs.cell(n, 14, f'=IF(ISNUMBER(E{n}),IF(E{n}<=po_start,MIN(pg_cap,10^(pg_a+pg_b*G{n}+pg_c*G{n}^2)),IF(E{n}<po_end,10^(-1+(pg_ystart+1)*(LOG10(po_end)-G{n})/(LOG10(po_end)-LOG10(po_start))),0)),"")')
    pgs.cell(n, 15, f'=IF(AND(ISNUMBER(N{n}),ISNUMBER(Transfers!H{n})),N{n}*F{n},0)')
    pgs.cell(n, 16, f'=O{n}/SUM({P("O")})')
    pgs.cell(n, 6).number_format = "#,##0"; pgs.cell(n, 5).number_format = "#,##0"
    for j in (3, 14): pgs.cell(n, j).number_format = "0.00"
    for j in (7, 9): pgs.cell(n, j).number_format = "0.000"
    for j in (11, 12, 13): pgs.cell(n, j).number_format = "#,##0"
    pgs.cell(n, 10).number_format = "#,##0"; pgs.cell(n, 15).number_format = "#,##0"; pgs.cell(n, 16).number_format = "0.00%"
pgs.cell(tot, 2, "World (total)").font = bold
pgs.cell(tot, 15, f"=SUM({P('O')})").number_format = "#,##0"
pgs.cell(tot, 16, f"=SUM({P('P')})").number_format = "0.00%"
for col, w in zip("ABCDEFGHIJKLMNOP", [7, 30, 12, 9, 12, 13, 9, 13, 10, 10, 10, 10, 10, 11, 14, 9]):
    pgs.column_dimensions[col].width = w
pgs.column_dimensions["A"].width = 38; pgs.column_dimensions["D"].width = 24; pgs.column_dimensions["G"].width = 22; pgs.column_dimensions["J"].width = 26
pgs.freeze_panes = pgs.cell(first, 3)
pgs.auto_filter.ref = f"A{H}:P{last}"

# Wealth tax tab (method of global_tax_attitudes/code_global/intl_plan_replication.R), rows aligned with Transfers
wt = wb.create_sheet("Wealth tax")
W = lambda col: f"${col}${first}:${col}${last}"
wt_params = [("A1", "Tax rate (marginal, above threshold)", "B1", 0.02),
             ("A2", "Factor for evasion (−20%) and depreciation (−10%)", "B2", "=(1-0.2)*(1-0.1)"),
             ("D1", "Imputation >$5M: constant (a)", "E1", f"=INDEX(LINEST({W('M')},$K${first}:$L${last},FALSE,FALSE),1,2)"),
             ("D2", "Imputation >$5M: slope on log10 GDP pc (b)", "E2", f"=INDEX(LINEST({W('M')},$K${first}:$L${last},FALSE,FALSE),1,1)"),
             ("D3", "Imputation >$100M: constant (a)", "E3", f"=INDEX(LINEST({W('P')},$N${first}:$O${last},FALSE,FALSE),1,2)"),
             ("D4", "Imputation >$100M: slope on log10 GDP pc (b)", "E4", f"=INDEX(LINEST({W('P')},$N${first}:$O${last},FALSE,FALSE),1,1)"),
             ("G1", "World revenue >$5M ($bn, applied to current GDP)", "H1", f"=SUMPRODUCT({W('Q')},{W('D')},{W('C')})/1E9"),
             ("G2", "World revenue >$100M ($bn, applied to current GDP)", "H2", f"=SUMPRODUCT({W('R')},{W('D')},{W('C')})/1E9"),
             ("G3", "Countries with WID data (>$5M)", "H3", f"=COUNT({W('H')})"),
             ("G4", "Countries with WID data (>$100M)", "H4", f"=COUNT({W('I')})")]
for lc, lab, vc, v in wt_params:
    wt[lc] = lab; wt[vc] = v; wt[vc].font = bold
for c in ("B1", "B2"): wt[c].fill = par_fill
wt["B1"].number_format = "0.0%"; wt["B2"].number_format = "0.00"
for c in ("E1", "E2", "E3", "E4"): wt[c].number_format = "0.000"
for c in ("H1", "H2"): wt[c].number_format = "#,##0"
for nm, ref in [("wt_rate", "B1"), ("wt_dep", "B2"), ("wt5_a", "E1"), ("wt5_b", "E2"), ("wt100_a", "E3"), ("wt100_b", "E4"), ("wt_world_5M", "H1"), ("wt_world_100M", "H2")]:
    wb.defined_names[nm] = DefinedName(nm, attr_text=f"'Wealth tax'!${ref[0]}${ref[1:]}")
wt_headers = ["ISO3", "Country", "Population", "GDP pc nominal", "GDP 2022 (US$, WB)", "Taxable wealth > $5M, 2022 (US$, WID)", "Taxable wealth > $100M, 2022 (US$, WID)",
              "Revenue > $5M from WID data (% of GDP 2022)", "Revenue > $100M from WID data (% of GDP 2022)", "log10 GDP pc",
              "√pop (if in reg. >$5M)", "log10 GDP pc × √pop", "log10 revenue × √pop", "√pop (if in reg. >$100M)", "log10 GDP pc × √pop", "log10 revenue × √pop",
              "Wealth tax revenue > $5M (% of GDP)", "Wealth tax revenue > $100M (% of GDP)", "Source"]
for j, h in enumerate(wt_headers, 1):
    c = wt.cell(H, j, h); c.font = bold; c.fill = wt_fill; c.alignment = Alignment(wrap_text=True, vertical="top")
wt.row_dimensions[H].height = 75
for i, r in df.iterrows():
    n = first + i
    wt.cell(n, 1, f"=Transfers!A{n}"); wt.cell(n, 2, f"=Transfers!B{n}")
    wt.cell(n, 3, f'=IF(ISNUMBER(Transfers!D{n}),Transfers!D{n},"")'); wt.cell(n, 4, f'=IF(ISNUMBER(Transfers!E{n}),Transfers!E{n},"")')
    for j, k in [(5, "gdp_2022"), (6, "taxable_5M"), (7, "taxable_100M")]:
        if pd.notna(r[k]): wt.cell(n, j, float(r[k]))
    wt.cell(n, 8, f'=IF(AND(ISNUMBER(F{n}),N(E{n})>0),wt_rate*wt_dep*F{n}/E{n},"")')
    wt.cell(n, 9, f'=IF(AND(ISNUMBER(G{n}),N(E{n})>0),wt_rate*wt_dep*G{n}/E{n},"")')
    wt.cell(n, 10, f'=IF(ISNUMBER(D{n}),LOG10(D{n}),0)')
    wt.cell(n, 11, f'=IF(AND(ISNUMBER(H{n}),ISNUMBER(D{n}),ISNUMBER(C{n})),SQRT(C{n}),0)')
    wt.cell(n, 12, f'=K{n}*J{n}')
    wt.cell(n, 13, f'=IF(K{n}>0,LOG10(H{n})*K{n},0)')
    wt.cell(n, 14, f'=IF(AND(ISNUMBER(I{n}),ISNUMBER(D{n}),ISNUMBER(C{n})),SQRT(C{n}),0)')
    wt.cell(n, 15, f'=N{n}*J{n}')
    wt.cell(n, 16, f'=IF(N{n}>0,LOG10(I{n})*N{n},0)')
    wt.cell(n, 17, f'=IF(ISNUMBER(H{n}),H{n},IF(ISNUMBER(D{n}),10^(wt5_a+wt5_b*J{n}),""))')
    wt.cell(n, 18, f'=IF(ISNUMBER(I{n}),I{n},IF(ISNUMBER(D{n}),10^(wt100_a+wt100_b*J{n}),""))')
    wt.cell(n, 19, f'=IF(ISNUMBER(H{n}),"WID 2022",IF(ISNUMBER(D{n}),"imputed",""))')
    for j in (3, 4): wt.cell(n, j).number_format = "#,##0"
    for j in (5, 6, 7): wt.cell(n, j).number_format = '#,##0,,," bn"'
    for j in (8, 9, 17, 18): wt.cell(n, j).number_format = "0.000%"
    for j in (10, 13, 16): wt.cell(n, j).number_format = "0.000"
    for j in (11, 12, 14, 15): wt.cell(n, j).number_format = "#,##0"
wt.cell(tot, 2, "World (total)")
for j, col in ((17, "Q"), (18, "R")):
    wt.cell(tot, j, f"=SUMPRODUCT({W(col)},{W('D')},{W('C')})/SUMPRODUCT(--ISNUMBER({W(col)}),{W('D')},{W('C')})").number_format = "0.000%"
for j, w in enumerate([7, 30, 13, 11, 12, 13, 13, 12, 12, 9, 10, 10, 10, 10, 10, 10, 12, 12, 10], 1):
    wt.column_dimensions[L_(j)].width = w
wt.column_dimensions["A"].width = 42; wt.column_dimensions["D"].width = 40; wt.column_dimensions["G"].width = 44
wt.freeze_panes = wt.cell(first, 3)
wt.auto_filter.ref = f"A{H}:S{last}"

# Dispersion among HICs of each transfer column
ds = wb.create_sheet("HIC dispersion")
ds["A1"] = "Dispersion among high-income countries of each transfer column of tab Transfers (current parameters)"; ds["A1"].font = Font(bold=True, size=12)
ds["A2"] = "Minimum population for second block"; ds["C2"] = 1e6; ds["C2"].fill = par_fill; ds["C2"].font = bold; ds["C2"].number_format = "#,##0"
TR = lambda col: f"Transfers!${col}${first}:${col}${last}"
items = [("I", "Population", "Net transfer to rest of the world", "% of GDP"), ("K", "Population", "Transfer to LICs", "% of GDP"), ("L", "Population", "Transfer to LICs + LMICs", "% of GDP"),
         ("M", "Poverty gap", "Net transfer to rest of the world", "% of GDP"), ("O", "Poverty gap", "Transfer to LICs", "% of GDP"), ("P", "Poverty gap", "Transfer to LICs + LMICs", "% of GDP"),
         ("Q", "Phase-out", "Net transfer to rest of the world", "% of GDP"), ("S", "Phase-out", "Transfer to LICs", "% of GDP"), ("T", "Phase-out", "Transfer to LICs + LMICs", "% of GDP"),
         *[(L_(23 + 3 * b + a), alloc, "Net transfer to rest of the world", f"% of wealth tax revenue > ${thr}")
           for b, thr in enumerate(("5M", "100M")) for a, alloc in enumerate(("Population", "Poverty gap", "Phase-out"))],
         ("AC", "—", "ODA provided", "% of GDP"), ("AE", "—", "Net ODA", "% of GDP"), ("AF", "Population", "Net transfer + net ODA", "% of GDP")]
stats = ["N", "Mean", "Pop-weighted mean", "SD", "Coef. of variation", "Min", "P10", "Median", "P90", "Max"]
for j, h in enumerate(["Column (Transfers)", "Allocation", "Transfer", "Unit"], 1):
    c = ds.cell(5, j, h); c.font = bold; c.fill = hdr_fill
for b, (label, extra) in enumerate([("All HICs", ""), ("HICs with population ≥ C2", f"*({TR('D')}>=$C$2)")]):
    c0 = 5 + b * len(stats)
    ds.cell(4, c0, label).font = bold
    for k, st in enumerate(stats):
        c = ds.cell(5, c0 + k, st); c.font = bold; c.fill = hdr_fill; c.alignment = Alignment(wrap_text=True)
    for i, (col, alloc, tr, unit) in enumerate(items):
        n = 6 + i
        x, cond = TR(col), f'({TR("C")}="High income")*ISNUMBER({TR(col)}){extra}'
        f = {"N": f"=SUMPRODUCT({cond})",
             "Mean": f"=AVERAGE(IF({cond},{x}))",
             "Pop-weighted mean": f"=SUM(IF({cond},{x}*{TR('D')}))/SUM(IF({cond},{TR('D')}))",
             "SD": f"=STDEV(IF({cond},{x}))",
             "Coef. of variation": f"={L_(c0 + 3)}{n}/ABS({L_(c0 + 1)}{n})",
             "Min": f"=MIN(IF({cond},{x}))", "P10": f"=PERCENTILE(IF({cond},{x}),0.1)", "Median": f"=MEDIAN(IF({cond},{x}))",
             "P90": f"=PERCENTILE(IF({cond},{x}),0.9)", "Max": f"=MAX(IF({cond},{x}))"}
        for k, st in enumerate(stats):
            ref = f"{L_(c0 + k)}{n}"
            ds[ref] = f[st] if st in ("N", "Coef. of variation") else ArrayFormula(ref, f[st])
            ds[ref].number_format = "0" if st == "N" else "0.00" if st == "Coef. of variation" else ("0.00%" if unit == "% of GDP" else "0%")
        if b == 0:
            for j, v in enumerate([col, alloc, tr, unit], 1): ds.cell(n, j, v)
ds.column_dimensions["A"].width = 10; ds.column_dimensions["B"].width = 12; ds.column_dimensions["C"].width = 30; ds.column_dimensions["D"].width = 22
for j in range(5, 5 + 2 * len(stats)): ds.column_dimensions[L_(j)].width = 10
ds.freeze_panes = "E6"

# Shares tab
sh = wb.create_sheet("Shares")
sh["A1"] = "Allocation of net receipts among net recipient countries (net transfer < 0)"; sh["A1"].font = Font(bold=True, size=12)
hs = ["Income group", "Population", "Number of net recipients", "Net receipts (bn)", "Share of total net receipts"]
for j, h in enumerate(hs, 1):
    c = sh.cell(3, j, h); c.font = bold; c.fill = hdr_fill; c.alignment = Alignment(wrap_text=True)
grp = f"Transfers!$C${first}:$C${last}"; netbn = f"Transfers!$J${first}:$J${last}"; pop = f"Transfers!$D${first}:$D${last}"
groups = ["Low income", "Lower middle income", "Upper middle income", "High income", "Not classified"]
for i, g in enumerate(groups):
    n = 4 + i
    sh.cell(n, 1, g)
    sh.cell(n, 2, f'=SUMIF({grp},A{n},{pop})')
    sh.cell(n, 3, f'=COUNTIFS({grp},A{n},{netbn},"<0")')
    sh.cell(n, 4, f'=-SUMIFS({netbn},{grp},A{n},{netbn},"<0")')
    sh.cell(n, 5, f"=D{n}/$D$9")
sh["A9"] = "Total"; sh["B9"] = "=SUM(B4:B8)"; sh["C9"] = "=SUM(C4:C8)"; sh["D9"] = "=SUM(D4:D8)"; sh["E9"] = "=SUM(E4:E8)"
for c in ("A9", "B9", "C9", "D9", "E9"): sh[c].font = bold
sh["A11"] = "Three components used in Transfers tab"; sh["A11"].font = bold
sh["A12"] = "Share to LICs"; sh["E12"] = "=E4"
sh["A13"] = "Share to LMICs"; sh["E13"] = "=E5"
sh["A14"] = "Share to others"; sh["E14"] = "=1-E12-E13"
for n in range(4, 10):
    sh.cell(n, 2).number_format = "#,##0"; sh.cell(n, 4).number_format = "#,##0.0"; sh.cell(n, 5).number_format = "0.0%"
for n in (12, 13, 14): sh.cell(n, 5).number_format = "0.0%"; sh.cell(n, 5).font = bold
wb.defined_names["share_LIC"] = DefinedName("share_LIC", attr_text="Shares!$E$12")
wb.defined_names["share_LMIC"] = DefinedName("share_LMIC", attr_text="Shares!$E$13")
sh["A1"] = "Allocation in proportion to population: net receipts among net recipient countries (net transfer < 0)"
sh["A17"] = "Allocation in proportion to statutory poverty gap: net receipts among net recipient countries (net transfer < 0)"; sh["A17"].font = Font(bold=True, size=12)
netbn_pg = f"Transfers!$N${first}:$N${last}"
for j, h in enumerate(hs, 1):
    c = sh.cell(19, j, h); c.font = bold; c.fill = pg_fill; c.alignment = Alignment(wrap_text=True)
for i, g in enumerate(groups):
    n = 20 + i
    sh.cell(n, 1, g)
    sh.cell(n, 2, f'=SUMIF({grp},A{n},{pop})')
    sh.cell(n, 3, f'=COUNTIFS({grp},A{n},{netbn_pg},"<0")')
    sh.cell(n, 4, f'=-SUMIFS({netbn_pg},{grp},A{n},{netbn_pg},"<0")')
    sh.cell(n, 5, f"=D{n}/$D$25")
sh["A25"] = "Total"; sh["B25"] = "=SUM(B20:B24)"; sh["C25"] = "=SUM(C20:C24)"; sh["D25"] = "=SUM(D20:D24)"; sh["E25"] = "=SUM(E20:E24)"
for c in ("A25", "B25", "C25", "D25", "E25"): sh[c].font = bold
sh["A27"] = "Three components used in Transfers tab (poverty-gap allocation)"; sh["A27"].font = bold
sh["A28"] = "Share to LICs"; sh["E28"] = "=E20"
sh["A29"] = "Share to LMICs"; sh["E29"] = "=E21"
sh["A30"] = "Share to others"; sh["E30"] = "=1-E28-E29"
for n in range(20, 26):
    sh.cell(n, 2).number_format = "#,##0"; sh.cell(n, 4).number_format = "#,##0.0"; sh.cell(n, 5).number_format = "0.0%"
for n in (28, 29, 30): sh.cell(n, 5).number_format = "0.0%"; sh.cell(n, 5).font = bold
wb.defined_names["pg_share_LIC"] = DefinedName("pg_share_LIC", attr_text="Shares!$E$28")
wb.defined_names["pg_share_LMIC"] = DefinedName("pg_share_LMIC", attr_text="Shares!$E$29")
sh["A33"] = "Allocation per capita with phase-out: net receipts among net recipient countries (net transfer < 0)"; sh["A33"].font = Font(bold=True, size=12)
netbn_po = f"Transfers!$R${first}:$R${last}"
for j, h in enumerate(hs, 1):
    c = sh.cell(35, j, h); c.font = bold; c.fill = po_fill; c.alignment = Alignment(wrap_text=True)
for i, g in enumerate(groups):
    n = 36 + i
    sh.cell(n, 1, g)
    sh.cell(n, 2, f'=SUMIF({grp},A{n},{pop})')
    sh.cell(n, 3, f'=COUNTIFS({grp},A{n},{netbn_po},"<0")')
    sh.cell(n, 4, f'=-SUMIFS({netbn_po},{grp},A{n},{netbn_po},"<0")')
    sh.cell(n, 5, f"=D{n}/$D$41")
sh["A41"] = "Total"; sh["B41"] = "=SUM(B36:B40)"; sh["C41"] = "=SUM(C36:C40)"; sh["D41"] = "=SUM(D36:D40)"; sh["E41"] = "=SUM(E36:E40)"
for c in ("A41", "B41", "C41", "D41", "E41"): sh[c].font = bold
sh["A43"] = "Three components used in Transfers tab (phase-out allocation)"; sh["A43"].font = bold
sh["A44"] = "Share to LICs"; sh["E44"] = "=E36"
sh["A45"] = "Share to LMICs"; sh["E45"] = "=E37"
sh["A46"] = "Share to others"; sh["E46"] = "=1-E44-E45"
for n in range(36, 42):
    sh.cell(n, 2).number_format = "#,##0"; sh.cell(n, 4).number_format = "#,##0.0"; sh.cell(n, 5).number_format = "0.0%"
for n in (44, 45, 46): sh.cell(n, 5).number_format = "0.0%"; sh.cell(n, 5).font = bold
wb.defined_names["po_share_LIC"] = DefinedName("po_share_LIC", attr_text="Shares!$E$44")
wb.defined_names["po_share_LMIC"] = DefinedName("po_share_LMIC", attr_text="Shares!$E$45")
sh.column_dimensions["A"].width = 24
for col in "BCDE": sh.column_dimensions[col].width = 16

# Notes tab
nt = wb.create_sheet("Notes")
notes = [
    "Scenario: each country transfers rate × base GDP (base = base per capita × population) to a global pool, redistributed to every country in proportion to its population.",
    "Net transfer of country i (% of its base GDP) = rate × (1 − world average base pc / base pc of i). Positive = net payer; negative = net recipient. Money amounts are rate × (base pc − world avg) × population.",
    "Transfer to LICs / LICs+LMICs: each net payer's net transfer is split into 3 components (LICs, LMICs, others) using the shares of LICs, LMICs and other countries in total net receipts of all net recipients (tab Shares). Net recipients transfer 0.",
    "World row: columns I and M = gross flow from net payers in % of world GDP; K-L and O-P = parts of that flow reaching LICs and LICs+LMICs.",
    "To change the size: edit Transfers!C2. To change the base: pick another header in Transfers!C3 (GDP pc nominal, GDP pc PPP, or fill column G 'Other base (optional)' with any per-capita variable and select it). Percentages are then expressed in % of the chosen base.",
    "Poverty-gap allocation (columns M-P): the pool rate × world base GDP is allocated in proportion to statutory poverty gap × population (tab 'Poverty gap', column P) instead of population. Net transfer of i = rate × (base GDP of i − world base GDP × share of i in the pool). The split into LICs / LMICs / others uses the shares of net receipts of net recipients under this allocation (tab Shares, rows 17-30).",
    "Statutory poverty gap (methodology of Fabre 2023, 'A Global Wealth Tax – Policy Brief', updated): regress log10(poverty gap at $4.20/day, 2021 PPP, %) on log10(GDP pc nominal) and its square, weighting by population, on countries with a positive poverty gap and GDP pc below the phase-out start (default: world average GDP pc nominal). The weighted regression is computed with LINEST on √pop-scaled variables. Statutory gap = 10^(a + b·x + c·x²) below the phase-out start; between phase-out start and end (default 2 × world average) its log10 declines linearly in log10 GDP pc from the predicted value at the start to −1 (i.e. 0.1%); above the end it is 0. Phase-out multipliers are editable (yellow cells). Update: with 2025 data the fitted quadratic is convex (c > 0, unlike in the 2023 brief), so the prediction explodes for the poorest countries; the statutory gap is therefore capped (default: highest observed poverty gap among countries in the regression, editable).",
    "The regression always uses GDP pc nominal (column E), whatever the base chosen for contributions. Poverty gaps: World Bank WDI SI.POV.LMIC.GP (Poverty gap at $4.20 a day, 2021 PPP), last available year per country (fetched 2026-10-26); countries without poverty gap data still receive according to their predicted (statutory) gap.",
    "Wealth tax (tab 'Wealth tax', method of global_tax_attitudes/code_global/intl_plan_replication.R): revenue = rate (2%) × (1−20% evasion) × (1−10% depreciation) × (wealth above threshold − threshold × number of people above it), with WID 2022 data (current US$), expressed in % of 2022 GDP (WB) and applied to current GDP. Missing countries are imputed from a population-weighted regression of log10(revenue in % of GDP) on log10(GDP pc nominal) (equivalent to the R script's regression of log revenue pc on log GNI pc, except that GDP pc is the current one). Two thresholds: > $5M and > $100M (the R script uses 3% above $100M; here 2% for both, editable).",
    "Share of wealth tax revenue transferred (Transfers, columns W-Y for > $5M, Z-AB for > $100M; net transfer to the rest of the world under the population, poverty-gap and phase-out allocations): transfer in money (% of base × base GDP) divided by wealth tax revenue in money. Negative = net receipt. World row: gross flow from net payers / world wealth tax revenue.",
    "Phase-out allocation (columns Q-T, tab 'Phase-out'): the pool rate × world base GDP is allocated in proportion to population × f(base pc), where f = 1 below the phase-out start, 0 above the phase-out end, and declines linearly in between (continuous). Defaults: start = 0.9 × world average base pc, end = 1.2 × world average. These are calibrated so that China's net transfer is ≈ 0 (with GDP pc nominal as base) and every country above the end, i.e. virtually all HICs, transfers exactly the full rate (1% of GDP): equal effort in % of GDP. Net transfer of i = rate × (base GDP of i − world base GDP × share of i in the pool). The LICs / LMICs / others split uses tab Shares, rows 33-46.",
    "ODA (columns AC-AF, tab 'ODA'): ODA provided = OECD DAC1 ODA grant equivalent (headline ODA, current US$), latest year available (2025 preliminary for most reporting countries, else 2024; note the large 2025 drop of the US), reported by DAC members and non-DAC reporters (China, India, Russia etc. do not report: blank). Net ODA received = World Bank WDI DT.ODA.ODAT.CD (latest year ≥ 2019, mostly 2023). Both are divided by current nominal GDP (E × D), so years may differ slightly. Net ODA = provided − received. Column AF = net transfer of the population allocation (column I, converted to % of nominal GDP) + net ODA, i.e. the total net transfer if the scheme came on top of current ODA. World row: AC, AD = totals in % of world GDP; AF = gross flow from net payers of column I + all ODA provided.",
    "Tab 'HIC dispersion': statistics across high-income countries (current WB classification) for each transfer column, for all HICs and for HICs above a population threshold (C2, default 1 million, to exclude microstates). Array formulas: they recompute automatically.",
    "Countries with missing population or base are excluded from both paying and receiving.",
    "Rows are sorted by net transfer to the rest of the world (% of GDP) under the population allocation with the default base, i.e. by GDP pc nominal, descending (static order: it does not re-sort when parameters change). World row on top. Bold: G20 member countries, Poland and Romania.",
    "Sources: World Bank WDI API (fetched 2026-10-26): SP.POP.TOTL, NY.GDP.PCAP.CD (current US$), NY.GDP.PCAP.PP.CD (current international $). Most recent non-missing year per country (see year columns). When the World Bank has no estimate, or only one older than 2024 while the IMF has a more recent year, the value is taken from the IMF World Economic Outlook (DataMapper API: LP, NGDPDPC, PPPPC; latest year up to 2025), see column AH. Income groups: current World Bank classification.",
]
for i, t in enumerate(notes, 1): nt.cell(i, 1, t)
nt.column_dimensions["A"].width = 160
for i, r in df.iterrows():
    if r.iso3 in G20:
        for sheet, ncol in ((ws, 36), (pgs, 16), (wt, 19), (po, 7), (od, 6)):
            for j in range(1, ncol + 1): sheet.cell(first + i, j).font = bold
for sheet in (ws, pgs, wt, po, od):
    for j in range(1, 37): sheet.cell(tot, j).font = bold
# Replace defined names by direct absolute references (robust across Excel versions/locales, LibreOffice, Google Sheets)
names = {k: v.attr_text for k, v in wb.defined_names.items()}
pat = re.compile(r"(?<![\w!$.'])(" + "|".join(sorted(names, key=len, reverse=True)) + r")(?![\w(])")
for sheet in wb.worksheets:
    for row in sheet.iter_rows():
        for c in row:
            if isinstance(c.value, str) and c.value.startswith("="):
                c.value = pat.sub(lambda m: names[m.group(1)], c.value)
            elif isinstance(c.value, ArrayFormula):
                c.value = ArrayFormula(c.value.ref, pat.sub(lambda m: names[m.group(1)], c.value.text))
for k in list(wb.defined_names): del wb.defined_names[k]
wb._sheets = [wb[s] for s in ("Transfers", "Shares", "Poverty gap", "Phase-out", "Wealth tax", "ODA", "HIC dispersion", "Notes")]
wb.save(out)
print(len(df), "countries;", df.gdp_nom.isna().sum(), "missing nominal;", df.gdp_ppp.isna().sum(), "missing PPP")
print(df.gdp_nom_year.value_counts().to_dict())
