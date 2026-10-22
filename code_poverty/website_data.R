# Exports the data of the interactive website (../website/data/) from the September 2026 PIP update.
# Run from code_poverty/ after update.R (which saves .RData_update):  Rscript website_data.R
# Outputs:
#   ../website/data/countries.json   compact per-country data used by the website (distribution at survey year + growth parameters)
#   ../website/data/percentiles.csv  full underlying dataset (one row per country x percentile), offered for download
#   ../website/data/countries.csv    per-country metadata (one row per country), offered for download
#   ../website/data/validation.json  R values of the indicators, used by website/test/validate.js to check the JavaScript port
load(".RData_update")
library(jsonlite)
LIC <- unique(c(setdiff(LIC, "SRE"), "SLE", "CAF")) # Corrects two coding errors in the paper's list of low-income countries (no-op if already corrected)
out <- "../website/data/"
dir.create(out, showWarnings = FALSE, recursive = TRUE)
p <- p[!is.na(p$welfare_avg_1),]
s <- s[match(p$country_code, s$country_code),]
p$country_short <- p$country
p$country_short[p$country == "Democratic Republic of the Congo"] <- "D.R. Congo"
years <- c(2022:2035, 2040, 2050, 2100) # population years exported (UN WPP 2022 projections); horizons offered: 2026, 2030, 2035, 2040, 2050, 2100
sig <- 7 # significant digits kept for incomes (validated by website/test/validate.js)
sig_g <- 10 # significant digits kept for growth factors (raised to powers)

##### Compact JSON for the website #####
# Price levels for the display in local currency or market US$ (country view only; computations stay in PPP $):
# ../data/price_level_2021.csv: WDI PA.NUS.PRVT.PP (LCU per international $, private consumption) and PA.NUS.FCRF (official exchange rate, LCU per US$), 2021
price <- read.csv("../data/price_level_2021.csv")
p$ppp_lcu_2021 <- price$ppp_private_consumption_2021[match(p$country_code, price$country_code)]
p$exchange_rate_2021 <- price$exchange_rate_2021[match(p$country_code, price$country_code)]
# Currency names (WDI country metadata "CurrencyUnit"), cached in ../website/data/currency_units.csv (fetched from the World Bank API if absent)
currency_file <- paste0(out, "currency_units.csv")
if (!file.exists(currency_file)) try({
  cu <- fromJSON(paste0("https://api.worldbank.org/v2/sources/2/country/", paste(p$country_code, collapse = ";"), "/metatypes/CurrencyUnit?format=json&per_page=1000"), simplifyVector = FALSE)
  vars <- cu$source[[1]]$concept[[1]]$variable
  write.csv(data.frame(country_code = sapply(vars, function(v) v$id), currency = sapply(vars, function(v) v$metatype[[1]]$value)), currency_file, row.names = FALSE)
})
# Inflation in local currency from 2021 to 2026 (to display local currency at today's prices), cached in ../data/cpi_2021_2026.csv:
# WDI CPI (FP.CPI.TOTL) from 2021 to its last available year, extended to 2026 with the IMF WEO (October 2026) average inflation (PCPIPCH; 2026 is an estimate; years missing in the WEO repeat the last WDI annual inflation);
# IMF inflation only when WDI lacks the 2021 CPI. LBR: the WDI PPP factor is in US$, so the US inflation is used. ZWE: no consistent series across the 2024 change of currency (ZWL to ZiG).
cpi_file <- "../data/cpi_2021_2026.csv"
if (!file.exists(cpi_file)) try({
  wdi <- fromJSON("https://api.worldbank.org/v2/country/all/indicator/FP.CPI.TOTL?format=json&per_page=20000&date=2021:2026")[[2]]
  wdi <- data.frame(code = wdi$countryiso3code, year = as.numeric(wdi$date), cpi = wdi$value)
  wdi <- wdi[!is.na(wdi$cpi),]
  imf <- fromJSON("https://www.imf.org/external/datamapper/api/v1/PCPIPCH")$values$PCPIPCH
  infl <- function(code) {
    src <- if (code == "LBR") "USA" else code
    w <- wdi[wdi$code == src,]
    i <- imf[[if (src == "PSE") "WBG" else if (src == "XKX") "UVK" else src]]
    last_rate <- if (sum(w$year >= 2021) >= 2) { l <- max(w$year); w$cpi[w$year == l]/w$cpi[w$year == l - 1] - 1 } else NA # last annual inflation in WDI, used for years missing in the WEO (LBN, PSE, SEN, XKX in 2026)
    imf_chain <- function(from) { if (from > 2026) return(1); y <- as.character(from:2026); r <- sapply(y, function(t) if (t %in% names(i)) i[[t]]/100 else last_rate); prod(1 + r) }
    if (code == "ZWE") return(c(NA, NA, "none"))
    if (2021 %in% w$year) { last <- max(w$year); return(c(w$cpi[w$year == last]/w$cpi[w$year == 2021] * imf_chain(last + 1), last, if (last < 2026) "WDI+IMF" else "WDI")) }
    c(imf_chain(2022), 2021, "IMF") }
  res <- t(sapply(p$country_code, infl))
  write.csv(data.frame(country_code = p$country_code, cpi_2026_over_2021 = as.numeric(res[,1]), wdi_last_year = as.numeric(res[,2]), source = res[,3]), cpi_file, row.names = FALSE)
})
cpi <- read.csv(cpi_file)
p$cpi_2021_2026 <- cpi$cpi_2026_over_2021[match(p$country_code, cpi$country_code)]
currency <- if (file.exists(currency_file)) read.csv(currency_file) else data.frame(country_code = character(), currency = character())
p$currency <- currency$currency[match(p$country_code, currency$country_code)]
p$currency[p$country_code == "LBR"] <- "US dollar" # WDI PPP factor of Liberia is in US$

uniform_ps <- all(abs(as.matrix(p[, paste0("pop_share_", 1:100)]) - 0.01) < 1e-12)
countries <- lapply(1:nrow(p), function(c) {
  r <- p[c,]
  l <- list(code = r$country_code, name = r$country_short, ssa = as.integer(r$country_code %in% SSA), lic = as.integer(r$country_code %in% LIC),
       year = r$year, level = r$reporting_level, welfare = r$welfare_type,
       growth_start = r$growth_start, growth_to_start = signif(r$growth_to_start, sig_g),
       growth_2022 = signif(r$mean_y_2022 / r$mean_welfare, sig_g), # observed growth from survey year to 2022 (the "now" scenario of preparation.R)
       trend = if (is.na(r$mean_growth_gdp_pc_15_25)) NULL else signif(r$mean_growth_gdp_pc_15_25, sig_g),
       reg = if (is.na(r$growth_reg_annual)) NULL else signif(r$growth_reg_annual, sig_g),
       scaling_factor = signif(r$scaling_factor, sig_g),
       ppp_lcu = if (is.na(r$ppp_lcu_2021)) NULL else signif(r$ppp_lcu_2021, sig_g), # LCU per PPP $ (private consumption, 2021)
       exchange_rate = if (is.na(r$exchange_rate_2021)) NULL else signif(r$exchange_rate_2021, sig_g), # LCU per US$ (2021)
       cpi = if (is.na(r$cpi_2021_2026)) NULL else signif(r$cpi_2021_2026, sig_g), # local CPI 2026 / 2021
       currency = if (is.na(r$currency) || r$currency == "") NULL else r$currency,
       pop_year = round(r$pop_year), pop = sapply(years, function(y) round(r[[paste0("pop_", y)]])),
       avg = signif(unlist(r[paste0("welfare_avg_", 1:100)]), sig),
       q = signif(unlist(r[paste0("quantile_", 1:99)]), sig)) # quantile_100 is missing in PIP: the max of the top percentile is its average (cf. compute_distribution_2030)
  if (!uniform_ps) l$pop_share <- signif(unlist(r[paste0("pop_share_", 1:100)]), 8)
  names(l$avg) <- names(l$q) <- NULL
  l
})
meta <- list(source = "World Bank Poverty and Inequality Platform (PIP), 100-bin distributions, September 2026 vintage (2021 PPP); WDI GDP per capita (NY.GDP.PCAP.PP.KD, constant 2021 PPP $); UN WPP population",
             generated = as.character(Sys.Date()), pop_years = years, pop_share = if (uniform_ps) 0.01 else NULL, n = nrow(p))
write(toJSON(list(meta = meta, countries = countries), auto_unbox = TRUE, digits = NA, null = "null"), paste0(out, "countries.json"))

##### CSV downloads #####
meta_df <- data.frame(country_code = p$country_code, country = p$country_short, ssa = p$country_code %in% SSA, lic = p$country_code %in% LIC,
                      survey_year = p$year, reporting_level = p$reporting_level, welfare_type = p$welfare_type, growth_start = p$growth_start,
                      growth_to_start = p$growth_to_start, growth_survey_to_2022 = p$mean_y_2022 / p$mean_welfare, trend_growth_2015_2025 = p$mean_growth_gdp_pc_15_25,
                      autoregressive_annual_factor = p$growth_reg_annual, hfce_scaling_factor = p$scaling_factor,
                      lcu_per_ppp_dollar_2021 = p$ppp_lcu_2021, lcu_per_usd_2021 = p$exchange_rate_2021, cpi_2026_over_2021 = p$cpi_2021_2026, currency = p$currency, mean_welfare_survey = p$mean_welfare,
                      pop_survey_year = p$pop_year, setNames(p[, paste0("pop_", years)], paste0("pop_", years)))
write.csv(meta_df, paste0(out, "countries.csv"), row.names = FALSE)
long <- do.call(rbind, lapply(1:nrow(p), function(c) data.frame(country_code = p$country_code[c], survey_year = p$year[c], welfare_type = p$welfare_type[c], percentile = 1:100,
           pop_share = unlist(p[c, paste0("pop_share_", 1:100)]), welfare_avg = unlist(p[c, paste0("welfare_avg_", 1:100)]), quantile_max = unlist(p[c, paste0("quantile_", 1:100)]),
           welfare_avg_hfce_rescaled = unlist(s[c, paste0("welfare_avg_", 1:100)]))))
write.csv(long, paste0(out, "percentiles.csv"), row.names = FALSE)

##### Validation values (R implementation) #####
# Adds a distribution named v, the survey distribution scaled by the factor g (cf. compute_distribution_2030)
add_dist <- function(df, v, g) {
  for (i in 1:100) df[[paste0(v, "_avg_", i)]] <- df[[paste0("welfare_avg_", i)]] * g
  for (i in 1:100) df[[paste0(v, "_max_", i)]] <- df[[paste0("quantile_", i)]] * g
  for (i in 2:100) df[[paste0(v, "_min_", i)]] <- df[[paste0("quantile_", i-1)]] * g
  df[[paste0(v, "_min_1")]] <- 0
  df[[paste0(v, "_max_100")]][is.na(df[[paste0(v, "_max_100")]])] <- df[[paste0(v, "_avg_100")]][is.na(df[[paste0(v, "_max_100")]])]
  df[[paste0("mean_", v)]] <- rowSums(df[, paste0(v, "_avg_", 1:100)] * df[, paste0("pop_share_", 1:100)])
  df
}
for (d in c("p", "s")) {
  df <- get(d)
  df <- add_dist(df, "Y3_35", df$growth_to_start * 1.03^(2035 - df$growth_start))
  df <- add_dist(df, "Y7_35", df$growth_to_start * 1.07^(2035 - df$growth_start))
  df <- add_dist(df, "Y3_50", df$growth_to_start * 1.03^(2050 - df$growth_start))
  df <- add_dist(df, "Y7_2100", df$growth_to_start * 1.07^(2100 - df$growth_start))
  assign(d, df)
}
world_pop <- function(g, df = p) if (g == "now") df$pop_2022 else if (g == "y_2025") df$pop_2025 else if (grepl("_35$", g)) df$pop_2035 else if (grepl("_50$", g)) df$pop_2050 else if (grepl("_2100$", g)) df$pop_2100 else df$pop_2030
world_rate <- function(L, g, df = p) { r <- compute_poverty_rate(df = df, threshold = L, growth = g); pop <- world_pop(g, df); 100 * sum(r * pop, na.rm = T) / sum(pop[!is.na(r)]) }
world_gap <- function(L, g, df = p) {
  y <- name_var_growth(g)
  gp <- compute_poverty_gap(df = df, threshold = L, unit = "mean", growth = g); pop <- world_pop(g, df)
  m <- df[[paste0("mean_", y)]]
  100 * sum(gp * pop, na.rm = T) / sum((m * pop)[!is.na(gp)], na.rm = T) }
eq_line <- 2.46
eq_threshold <- 6.85 * eq_line / 2.15
# Each test: data source, scenario (as in preparation.R), indicator, parameters. 
tests <- list(
  list(src = "p", g = "average", ind = "rate", line = 3), list(src = "p", g = "average", ind = "rate", line = eq_line),
  list(src = "p", g = "very_optimistic", ind = "rate", line = 3), list(src = "p", g = "trend", ind = "rate", line = 3),
  list(src = "p", g = "reg", ind = "rate", line = 3), list(src = "p", g = "now", ind = "rate", line = 3), list(src = "p", g = "y_2025", ind = "rate", line = 3),
  list(src = "p", g = "Y3_35", ind = "rate", line = 4.2), list(src = "p", g = "average", ind = "rate", line = 8.3),
  list(src = "p", g = "average", ind = "gap", line = 3), list(src = "p", g = "trend", ind = "gap", line = 4.2),
  list(src = "p", g = "average", ind = "tax", line = 3, thr = 8.3), list(src = "p", g = "average", ind = "tax", line = eq_line, thr = eq_threshold),
  list(src = "p", g = "very_optimistic", ind = "tax", line = 3, thr = 18.15), list(src = "p", g = "reg", ind = "tax", line = 3, thr = 8.3),
  list(src = "p", g = "very_optimistic", ind = "cap", line = 3), list(src = "p", g = "average", ind = "cap", line = 3), list(src = "p", g = "Y3_35", ind = "cap", line = 4.2),
  list(src = "p", g = "average", ind = "floor", thr = 8.3, rate = 10), list(src = "p", g = "very_optimistic", ind = "floor", thr = 8.3, rate = 10),
  list(src = "p", g = "Y3_35", ind = "floor", thr = 18.15, rate = 20),
  list(src = "p", g = "Y3_50", ind = "tax", line = 3, thr = 8.3), list(src = "p", g = "Y7_2100", ind = "cap", line = 8.3), list(src = "p", g = "Y3_50", ind = "floor", thr = 8.3, rate = 10),
  list(src = "s", g = "average", ind = "rate", line = 3), list(src = "s", g = "average", ind = "tax", line = 3, thr = 8.3),
  list(src = "s", g = "very_optimistic", ind = "cap", line = 3), list(src = "s", g = "very_optimistic", ind = "floor", thr = 8.3, rate = 10))
for (k in seq_along(tests)) {
  t <- tests[[k]]; df <- get(t$src)
  t$values <- switch(t$ind,
    rate = 100 * compute_poverty_rate(df = df, threshold = t$line, growth = t$g),
    gap = 100 * compute_poverty_gap(df = df, threshold = t$line, unit = "%", growth = t$g),
    tax = compute_antipoverty_tax(df = df, exemption_threshold = t$thr, poverty_threshold = t$line, growth = t$g),
    cap = compute_antipoverty_cap(df = df, threshold = t$line, growth = t$g),
    floor = compute_income_floor(df = df, thresholds = t$thr, marginal_rates = t$rate, growth = t$g, scope_tax = df))
  t$values[is.infinite(t$values)] <- 1e300 # JSON has no Inf: 1e300 means no poverty (cap infinite)
  t$values <- signif(t$values, 10)
  tests[[k]] <- t
}
tax_avg <- compute_antipoverty_tax(df = p, exemption_threshold = 8.3, poverty_threshold = 3, growth = "average")
world <- list(RatePovNow = world_rate(3, "now"), RatePovTwentyFive = world_rate(3, "y_2025"), RatePovThree = world_rate(3, "average"),
              RatePovTrend = world_rate(3, "trend"), RatePovSeven = world_rate(3, "very_optimistic"), RatePovReg = world_rate(3, "reg"),
              GapNow = world_gap(3, "now"), GapThree = world_gap(3, "average"), RatePovThreeEq = world_rate(eq_line, "average"),
              RatePovThreeTF = world_rate(3, "Y3_35"), RatePovThreeFifty = world_rate(3, "Y3_50"), GapThreeFifty = world_gap(3, "Y3_50"), RatePovSevenHundred = world_rate(8.3, "Y7_2100"), GapThreeTF = world_gap(3, "Y3_35"),
              NbCapEight = sum(tax_avg > 100, na.rm = T),
              TaxEightSSA = wtd.mean(tax_avg[p$country_code %in% SSA], p$pop_2030[p$country_code %in% SSA]),
              TaxEightLIC = wtd.mean(tax_avg[p$country_code %in% LIC], p$pop_2030[p$country_code %in% LIC]),
              NbCapEq = sum(compute_antipoverty_tax(df = p, exemption_threshold = eq_threshold, poverty_threshold = eq_line, growth = "average") > 100, na.rm = T),
              NbFloor = sum(compute_income_floor(df = p, thresholds = 8.3, marginal_rates = 10, growth = "average", scope_tax = p) < 3, na.rm = T),
              NbFloorSevenHFCE = sum(compute_income_floor(df = s, thresholds = 8.3, marginal_rates = 10, growth = "very_optimistic", scope_tax = p) < 3, na.rm = T))
write(toJSON(list(codes = p$country_code, tests = tests, world = world), auto_unbox = TRUE, digits = NA), paste0(out, "validation.json"))
print(unlist(world))
