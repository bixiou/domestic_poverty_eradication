# Update of the paper's main results with the September 2026 PIP vintage (2021 PPP), cf. paper/update.tex.
# Run after .Rprofile (no need to run preparation.R first).
# Growth scenarios start from max(survey year, last year with GDP data, i.e. 2025 for most countries), after observed growth (cf. create_p).
# Poverty lines in 2021 PPP $/day (World Bank, June 2025): 3.00 (extreme, formerly 2.15 in 2017 PPP), 4.20 (formerly 3.65), 8.30 (formerly 6.85).
# 18.15 (2021 U.S. federal poverty line for a family of four, per person) is already expressed in 2021 $, so it is unchanged.
# Data: ../data/Povcalnet 2021.csv: PIP percentiles, version 20260922 https://datacatalogfiles.worldbank.org/ddh-published/0063646/DR0090357/world_100bin.csv
#       ../data/gdp_pc_ppp2021.csv: WDI NY.GDP.PCAP.PP.KD (constant 2021 PPP $), last updated 08/10/2026, fetched 20/10/2026

prep <- readLines("preparation.R")
run_lines <- function(from, to) eval(parse(text = prep[grep(from, prep)[1]:grep(to, prep)[1]]), envir = globalenv())
run_lines("^name_var_growth <- function", "^SSA <- c\\(") # functions and population/regions data
LIC <- c(setdiff(LIC, "SRE"), "SLE", "CAF") # Corrects two coding errors in the paper's list of low-income countries (Sierra Leone's code and the missing Central African Republic)

start <- Sys.time()
ppp_year <- 2021
p <- create_p(ppp_year = 2021)
pante <- create_p(ppp_year = 2021, year_max = F)
s <- create_p(ppp_year = 2021, rescale = T)
w <- create_world_distribution()
ws <- create_world_distribution(df = s)
p <- p[!duplicated(p$country_code),]
s <- s[!duplicated(s$country_code),]
pante <- pante[!duplicated(pante$country_code),]
p$country_short <- p$country
p$country_short[p$country == "Democratic Republic of the Congo"] <- "D.R. Congo"
save.image(".RData_update") # checkpoint
run_lines("^# Unbalanced growth", "^w <- compute_world_distribution\\(name_var_growth\\(\"trend_ineq\"\\)")
print(Sys.time() - start)
save.image(".RData_update")


##### Global poverty table #####
# World poverty rates and gaps are aggregated from country estimates (population-weighted), rather than computed on the world distribution w,
# whose 100 percentiles only allow a precision of about 1 percentage point on poverty rates.
world_pop <- function(g, df = p) if (g == "now") df$pop_2022 else if (g == "y_2025") df$pop_2025 else if (grepl("_35$", g)) df$pop_2035 else df$pop_2030
world_rate <- function(L, g, df = p) { r <- compute_poverty_rate(df = df, threshold = L, growth = g); pop <- world_pop(g, df); 100 * sum(r * pop, na.rm = T) / sum(pop[!is.na(r)]) }
world_gap <- function(L, g, df = p) { 
  y <- name_var_growth(g)
  gp <- compute_poverty_gap(df = df, threshold = L, unit = "mean", growth = g); pop <- world_pop(g, df)
  m <- if (paste0("mean_", y) %in% names(df)) df[[paste0("mean_", y)]] else rowSums(df[, paste0(y, "_avg_", 1:100)] * df[, paste0(find_pop_share_var(y, df), 1:100)]) # mean_Y3_ineq is not computed in preparation.R
  100 * sum(gp * pop, na.rm = T) / sum((m * pop)[!is.na(gp)], na.rm = T) } # in % of world income
lines_21 <- c(3, 4.2, 8.3, 18.15)
# All 2030 scenarios use observed growth up to 2025 (or the survey year if later), then the scenario's growth.
growth_scenarios <- setNames(c("now", "y_2025", "trend", "trend_pos", "reg", "average", "trend_ineq", "very_optimistic"),
                             c("2022 estimate", "2025 estimate", "Trend (2015--2025)", "Max(Trend, 0)", "Autoregressive projection", "3\\% growth", "3\\% unbalanced growth", "7\\% growth"))
table_poverty_update <- cbind(sapply(lines_21, function(l) sapply(growth_scenarios, function(g) world_rate(l, g))),
                              sapply(lines_21, function(l) sapply(growth_scenarios, function(g) world_gap(l, g))))
cat(sub("\\toprule\n", "\\toprule Growth scenario & \\multicolumn{4}{c}{Poverty rate (\\%)} & \\multicolumn{4}{c}{Poverty gap (\\% of GDP)} \\\\ \n (Poverty line in \\$/day)",
        paste(kbl(table_poverty_update, "latex",
      caption = "Global poverty rates and poverty gaps in 2030 under different growth scenarios, using the September 2026 PIP data. Growth scenarios apply from 2025 (or the survey year if later), after observed growth. Poverty rates are expressed in \\% of world population and poverty gaps in \\% of world GDP. Poverty lines are in 2021 PPP \\$/day.",
      row.names = T, position = "h", escape = F, booktabs = T, digits = c(1, 1, 1, 1, 2, 2, 2, 2), label = "poverty_update", linesep = rep("", nrow(table_poverty_update)-1),
      caption.short = "Global poverty (rates and gaps) in 2030 under different growth scenarios (September 2026 PIP data).",
      col.names = c("3.00", "4.20", "8.30", "18.15", "3.00", "4.20", "8.30", "18.15")), collapse="\n"), fixed = T), file = "../tables/poverty_update.tex")


##### Headline numbers (exported as LaTeX macros) #####
numbers <- list()
c_ <- function(code, var) var[p$country_code == code]
names_list <- function(codes) { n <- sort(p$country_short[p$country_code %in% codes]); if (length(n) < 2) n else paste(paste(n[-length(n)], collapse = ", "), "and", n[length(n)]) }

# Growth
numbers$RatePovNow <- world_rate(3, "now")
numbers$RatePovTwentyFive <- world_rate(3, "y_2025")
numbers$RatePovThree <- world_rate(3, "average")
numbers$RatePovTrend <- world_rate(3, "trend")
numbers$RatePovSeven <- world_rate(3, "very_optimistic")
numbers$GapNow <- world_gap(3, "now")
numbers$GapThree <- world_gap(3, "average")
numbers$ShareSurveyRecent <- 100 * mean(p$year >= 2021 & !is.na(p$welfare_avg_1)) # share of countries with a survey in 2021 or later
numbers$NbCountries <- sum(!is.na(p$welfare_avg_1))
numbers$RegSlope <- format(signif(coef(reg_model)[["x"]], 3))
numbers$RegQuad <- format(signif(coef(reg_model)[["I(x^2)"]], 3))
numbers$RegIntercept <- format(signif(coef(reg_model)[[1]], 3))
numbers$RegRsq <- summary(reg_model)$adj.r.squared
numbers$RegN <- nobs(reg_model)

# Antipoverty cap and tax (above $8.30/day, the 2021-PPP counterpart of $6.85/day)
p$antipoverty_3_tax_8_average <- compute_antipoverty_tax(df = p, exemption_threshold = 8.3, poverty_threshold = 3, growth = "average")
numbers$NbCapEight <- sum(p$antipoverty_3_tax_8_average > 100, na.rm = T)
numbers$CountriesCapEight <- names_list(p$country_code[no.na(p$antipoverty_3_tax_8_average > 100, rep = F, num_as_char = F)])
numbers$NbCapEightLIC <- sum(p$antipoverty_3_tax_8_average > 100 & p$country_code %in% LIC, na.rm = T)
numbers$TaxEightSSA <- wtd.mean(p$antipoverty_3_tax_8_average[p$country_code %in% SSA], p$pop_2030[p$country_code %in% SSA])
numbers$TaxEightLIC <- wtd.mean(p$antipoverty_3_tax_8_average[p$country_code %in% LIC], p$pop_2030[p$country_code %in% LIC])
numbers$TaxEightNGA <- c_("NGA", p$antipoverty_3_tax_8_average)
p$antipoverty_3_cap_very_optimistic <- compute_antipoverty_cap(df = p, threshold = 3, growth = "very_optimistic")
numbers$CapSevenCOD <- c_("COD", p$antipoverty_3_cap_very_optimistic)
numbers$CapSevenBDI <- c_("BDI", p$antipoverty_3_cap_very_optimistic)
numbers$MeanThreeMDG <- c_("MDG", p$mean_Y3)
numbers$NbMeanBelowThree <- sum(p$mean_Y3 < 3, na.rm = T)
numbers$CountriesMeanBelowThree <- names_list(p$country_code[no.na(p$mean_Y3 < 3, rep = F, num_as_char = F)])
p$antipoverty_3_tax_18_very_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3, growth = "very_optimistic")
p$antipoverty_3_tax_18_trend <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3, growth = "trend")
p$antipoverty_3_tax_18_average <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3, growth = "average")
numbers$TaxEighteenSevenIND <- c_("IND", p$antipoverty_3_tax_18_very_optimistic)
numbers$TaxEighteenTrendIND <- c_("IND", p$antipoverty_3_tax_18_trend)
numbers$TaxEighteenThreeIND <- c_("IND", p$antipoverty_3_tax_18_average)
numbers$RateThreeIND <- 100 * c_("IND", compute_poverty_rate(df = p, threshold = 3, growth = "average"))
numbers$TrendIND <- 100 * c_("IND", p$mean_growth_gdp_pc_15_25)

# Income floor financed by a 10% tax above $8.30/day
p$floor_8__10 <- compute_income_floor(df = p, thresholds = 8.3, marginal_rates = 10, growth = "average", scope_tax = p)
p$floor_8__10_very_optimistic <- compute_income_floor(df = p, thresholds = 8.3, marginal_rates = 10, growth = "very_optimistic", scope_tax = p)
p$s_floor_8__10_very_optimistic <- compute_income_floor(df = s, thresholds = 8.3, marginal_rates = 10, growth = "very_optimistic", scope_tax = p)
numbers$NbFloor <- sum(p$floor_8__10 < 3, na.rm = T)
numbers$NbFloorLIC <- sum(p$floor_8__10 < 3 & p$country_code %in% LIC, na.rm = T)
numbers$NbFloorSeven <- sum(p$floor_8__10_very_optimistic < 3, na.rm = T)
numbers$NbFloorSevenHFCE <- sum(p$s_floor_8__10_very_optimistic < 3, na.rm = T)
numbers$FloorETH <- c_("ETH", p$floor_8__10)
numbers$FloorNGA <- c_("NGA", p$floor_8__10)

# Global redistribution
numbers$GlobalFloor <- compute_income_floor(df = w, thresholds = 8.3, marginal_rates = 10, growth = 'average', name_tax = "8__10", scope_tax = w)
numbers$GiniThree <- compute_inequality(df = w, var = "Y3", return = 'gini')
w <- tax_revenues(df = w, thresholds = 8.3, marginal_rates = 10, return = 'df', growth = "average", name_tax = "8__10", scope_tax = w)
numbers$GiniThreeGlobalTax <- compute_inequality(df = w, var = "Y3_tax_8__10", return = 'gini')
# Marginal rate above $100/day that closes the global extreme poverty gap (revenues are linear in the rate)
numbers$RateHundred <- world_gap(3, "average") / 100 / tax_revenues(df = w, thresholds = 100, marginal_rates = 1, growth = "average", return = '%', scope_tax = w)
numbers$RateHundredHFCE <- world_gap(3, "average", df = s) / 100 / tax_revenues(df = ws, thresholds = 100, marginal_rates = 1, growth = "average", return = '%', scope_tax = ws)
numbers$ShareBelowHundred <- 100 * compute_poverty_rate(df = ws, threshold = 100, growth = 'average')

# Same indicators with the 2017-PPP lines converted for prices only: $2.15 becomes $2.46 in 2021 PPP (Foster et al., 2025); the threshold of $6.85 is scaled by the same factor.
# This isolates the effect of the new data from that of the (higher) $3.00 line.
eq_line <- 2.46
eq_threshold <- 6.85 * eq_line / 2.15
numbers$EqLine <- eq_line
numbers$EqThreshold <- eq_threshold
numbers$RatePovNowEq <- world_rate(eq_line, "now")
numbers$RatePovTwentyFiveEq <- world_rate(eq_line, "y_2025")
numbers$RatePovThreeEq <- world_rate(eq_line, "average")
numbers$RatePovSevenEq <- world_rate(eq_line, "very_optimistic")
numbers$GapThreeEq <- world_gap(eq_line, "average")
p$antipoverty_eq_tax_eq_average <- compute_antipoverty_tax(df = p, exemption_threshold = eq_threshold, poverty_threshold = eq_line, growth = "average")
numbers$NbCapEq <- sum(p$antipoverty_eq_tax_eq_average > 100, na.rm = T)
numbers$CountriesCapEq <- names_list(p$country_code[no.na(p$antipoverty_eq_tax_eq_average > 100, rep = F, num_as_char = F)])
numbers$TaxEqSSA <- wtd.mean(p$antipoverty_eq_tax_eq_average[p$country_code %in% SSA], p$pop_2030[p$country_code %in% SSA])
numbers$TaxEqLIC <- wtd.mean(p$antipoverty_eq_tax_eq_average[p$country_code %in% LIC], p$pop_2030[p$country_code %in% LIC])
numbers$TaxEqNGA <- c_("NGA", p$antipoverty_eq_tax_eq_average)
numbers$TaxEighteenThreeINDEq <- c_("IND", compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = eq_line, growth = "average"))
numbers$NbFloorEq <- sum(compute_income_floor(df = p, thresholds = eq_threshold, marginal_rates = 10, growth = "average", scope_tax = p) < eq_line, na.rm = T)
numbers$NbFloorSevenEq <- sum(compute_income_floor(df = p, thresholds = eq_threshold, marginal_rates = 10, growth = "very_optimistic", scope_tax = p) < eq_line, na.rm = T)
numbers$RateHundredEq <- world_gap(eq_line, "average") / 100 / tax_revenues(df = w, thresholds = 100, marginal_rates = 1, growth = "average", return = '%', scope_tax = w)
numbers$GlobalFloorEq <- compute_income_floor(df = w, thresholds = eq_threshold, marginal_rates = 10, growth = 'average', name_tax = "eq__10", scope_tax = w)

##### Alternative horizon: 2035 #####
# With balanced growth, the 2035 distribution is the 2030 one scaled by 5 more years of growth (3% or 7%).
# World aggregates use 2035 population (UN WPP 2022 projections).
extend_to_2035 <- function(df, from, to, factor) {
  for (v in grep(paste0("^", from, "_(avg|max|min)_[0-9]+$"), names(df), value = T)) df[[sub(paste0("^", from), to, v)]] <- df[[v]] * factor
  df[[paste0("mean_", to)]] <- rowSums(df[, paste0(to, "_avg_", 1:100)] * df[, paste0("pop_share_", 1:100)])
  return(df)
}
for (d in c("p", "s")) {
  df <- get(d)
  df <- extend_to_2035(df, "Y3", "Y3_35", 1.03^5)
  df <- extend_to_2035(df, "Y7", "Y7_35", 1.07^5)
  assign(d, df)
}
for (L in list(c(3, 8.3, ""), c(eq_line, eq_threshold, "Eq"))) {
  line <- as.numeric(L[1]); thr <- as.numeric(L[2]); sfx <- L[3]
  numbers[[paste0("RatePovThreeTF", sfx)]] <- world_rate(line, "Y3_35")
  numbers[[paste0("RatePovSevenTF", sfx)]] <- world_rate(line, "Y7_35")
  numbers[[paste0("GapThreeTF", sfx)]] <- world_gap(line, "Y3_35")
  tax <- compute_antipoverty_tax(df = p, exemption_threshold = thr, poverty_threshold = line, growth = "Y3_35")
  numbers[[paste0("NbCapTF", sfx)]] <- sum(tax > 100, na.rm = T)
  numbers[[paste0("CountriesCapTF", sfx)]] <- names_list(p$country_code[no.na(tax > 100, rep = F, num_as_char = F)])
  numbers[[paste0("TaxSSATF", sfx)]] <- wtd.mean(tax[p$country_code %in% SSA], p$pop_2035[p$country_code %in% SSA])
  numbers[[paste0("NbFloorTF", sfx)]] <- sum(compute_income_floor(df = p, thresholds = thr, marginal_rates = 10, growth = "Y3_35", scope_tax = p) < line, na.rm = T)
  numbers[[paste0("NbFloorSevenTF", sfx)]] <- sum(compute_income_floor(df = p, thresholds = thr, marginal_rates = 10, growth = "Y7_35", scope_tax = p) < line, na.rm = T)
}
p35 <- p
p35$pop_2030 <- p35$pop_2035 # compute_world_distribution and tax_revenues weight by pop_2030
w35 <- compute_world_distribution("Y3_35", df = p35, wdf = data.frame(country = "World", pop_2022 = sum(p35$pop_2022), pop_2030 = sum(p35$pop_2030)))
numbers$RateHundredTF <- world_gap(3, "Y3_35") / 100 / tax_revenues(df = w35, thresholds = 100, marginal_rates = 1, growth = "Y3_35", return = '%', scope_tax = w35)

digits <- function(x) if (abs(x) >= 19.95 | x == round(x)) 0 else if (abs(x) >= 0.995) 1 else 2
cat(paste0("% Generated by code_poverty/update.R on ", Sys.Date(), "\n"),
    sapply(names(numbers), function(n) paste0("\\newcommand{\\up", n, "}{", if (is.numeric(numbers[[n]])) format(round(numbers[[n]], digits(numbers[[n]])), nsmall = digits(numbers[[n]]), big.mark = ",") else numbers[[n]], "}\n")),
    sep = "", file = "../tables/update_numbers.tex")
print(unlist(numbers))
