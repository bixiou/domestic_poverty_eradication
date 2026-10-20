# Update of the paper's main results with the September 2026 PIP vintage (2021 PPP), cf. paper/update.tex.
# Run after .Rprofile (no need to run preparation.R first). Lasts ~1h.
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
lines_21 <- c(3, 4.2, 8.3, 18.15)
growth_scenarios <- setNames(c("now", "trend", "trend_pos", "reg", "average", "trend_ineq", "very_optimistic", "sdg8", "realized"),
                             c("2022 Estimate", "Trend (2014--2019)", "Max(Trend, 0)", "Autoregressive projection", "3\\% growth", "3\\% unbalanced growth", "7\\% growth", "7\\% growth since 2016", "Observed to 2025, then 3\\%"))
table_poverty_update <- cbind(sapply(lines_21, function(l) 100*sapply(growth_scenarios, function(g) compute_poverty_rate(df = w, threshold = l, growth = g, return = "rate"))),
                              sapply(lines_21, function(l) 100*sapply(growth_scenarios, function(g) compute_poverty_gap(df = w, threshold = l, unit = '%', growth = g))))
cat(sub("\\toprule\n", "\\toprule Growth scenario & \\multicolumn{4}{c}{Poverty rate (\\%)} & \\multicolumn{4}{c}{Poverty gap (\\% of GDP)} \\\\ \n (Poverty line in \\$/day)",
        paste(kbl(table_poverty_update, "latex",
      caption = "Global poverty rates and poverty gaps in 2030 under different growth scenarios, using the September 2026 PIP data. Poverty rates are expressed in \\% of world population and poverty gaps in \\% of world GDP. Poverty lines are in 2021 PPP \\$/day.",
      row.names = T, position = "h", escape = F, booktabs = T, digits = c(1, 1, 1, 1, 2, 2, 2, 2), label = "poverty_update", linesep = rep("", nrow(table_poverty_update)-1),
      caption.short = "Global poverty (rates and gaps) in 2030 under different growth scenarios (September 2026 PIP data).",
      col.names = c("3.00", "4.20", "8.30", "18.15", "3.00", "4.20", "8.30", "18.15")), collapse="\n"), fixed = T), file = "../tables/poverty_update.tex")


##### Headline numbers (exported as LaTeX macros) #####
numbers <- list()
c_ <- function(code, var) var[p$country_code == code]
names_list <- function(codes) { n <- sort(p$country_short[p$country_code %in% codes]); if (length(n) < 2) n else paste(paste(n[-length(n)], collapse = ", "), "and", n[length(n)]) }

# Growth
numbers$RatePovNow <- 100 * compute_poverty_rate(df = w, threshold = 3, growth = "now")
numbers$RatePovThree <- 100 * compute_poverty_rate(df = w, threshold = 3, growth = "average")
numbers$RatePovTrend <- 100 * compute_poverty_rate(df = w, threshold = 3, growth = "trend")
numbers$RatePovSeven <- 100 * compute_poverty_rate(df = w, threshold = 3, growth = "very_optimistic")
numbers$RatePovSDG <- 100 * compute_poverty_rate(df = w, threshold = 3, growth = "sdg8")
numbers$RatePovRealized <- 100 * compute_poverty_rate(df = w, threshold = 3, growth = "realized")
numbers$GapNow <- 100 * compute_poverty_gap(df = w, threshold = 3, unit = '%', growth = "now")
numbers$GapThree <- 100 * compute_poverty_gap(df = w, threshold = 3, unit = '%', growth = "average")
numbers$GapRealized <- 100 * compute_poverty_gap(df = w, threshold = 3, unit = '%', growth = "realized")
numbers$ShareSurveyRecent <- 100 * mean(p$year >= 2021 & !is.na(p$welfare_avg_1)) # share of countries with a survey in 2021 or later
numbers$NbCountries <- sum(!is.na(p$welfare_avg_1))

# Antipoverty cap and tax (above $8.30/day, the 2021-PPP counterpart of $6.85/day)
p$antipoverty_3_tax_8_average <- compute_antipoverty_tax(df = p, exemption_threshold = 8.3, poverty_threshold = 3, growth = "average")
p$antipoverty_3_tax_8_realized <- compute_antipoverty_tax(df = p, exemption_threshold = 8.3, poverty_threshold = 3, growth = "realized")
numbers$NbCapEight <- sum(p$antipoverty_3_tax_8_average > 100, na.rm = T)
numbers$CountriesCapEight <- names_list(p$country_code[no.na(p$antipoverty_3_tax_8_average > 100, rep = F, num_as_char = F)])
numbers$NbCapEightRealized <- sum(p$antipoverty_3_tax_8_realized > 100, na.rm = T)
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
p$antipoverty_3_tax_18_realized <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3, growth = "realized")
numbers$TaxEighteenSevenIND <- c_("IND", p$antipoverty_3_tax_18_very_optimistic)
numbers$TaxEighteenTrendIND <- c_("IND", p$antipoverty_3_tax_18_trend)
numbers$TaxEighteenThreeIND <- c_("IND", p$antipoverty_3_tax_18_average)
numbers$TaxEighteenRealizedIND <- c_("IND", p$antipoverty_3_tax_18_realized)
numbers$RateThreeIND <- 100 * c_("IND", compute_poverty_rate(df = p, threshold = 3, growth = "average"))
numbers$TrendIND <- 100 * c_("IND", p$mean_growth_gdp_pc_14_19)

# Income floor financed by a 10% tax above $8.30/day
p$floor_8__10 <- compute_income_floor(df = p, thresholds = 8.3, marginal_rates = 10, growth = "average", scope_tax = p)
p$floor_8__10_very_optimistic <- compute_income_floor(df = p, thresholds = 8.3, marginal_rates = 10, growth = "very_optimistic", scope_tax = p)
p$s_floor_8__10_very_optimistic <- compute_income_floor(df = s, thresholds = 8.3, marginal_rates = 10, growth = "very_optimistic", scope_tax = p)
p$floor_8__10_sdg8 <- compute_income_floor(df = p, thresholds = 8.3, marginal_rates = 10, growth = "sdg8", scope_tax = p)
p$floor_8__10_realized <- compute_income_floor(df = p, thresholds = 8.3, marginal_rates = 10, growth = "realized", scope_tax = p)
numbers$NbFloor <- sum(p$floor_8__10 < 3, na.rm = T)
numbers$NbFloorLIC <- sum(p$floor_8__10 < 3 & p$country_code %in% LIC, na.rm = T)
numbers$NbFloorSeven <- sum(p$floor_8__10_very_optimistic < 3, na.rm = T)
numbers$NbFloorSevenHFCE <- sum(p$s_floor_8__10_very_optimistic < 3, na.rm = T)
numbers$NbFloorSDG <- sum(p$floor_8__10_sdg8 < 3, na.rm = T)
numbers$CountriesFloorSDG <- names_list(p$country_code[no.na(p$floor_8__10_sdg8 < 3, rep = F, num_as_char = F)])
numbers$NbFloorRealized <- sum(p$floor_8__10_realized < 3, na.rm = T)
numbers$FloorETH <- c_("ETH", p$floor_8__10)
numbers$FloorNGA <- c_("NGA", p$floor_8__10)

# Global redistribution
numbers$GlobalFloor <- compute_income_floor(df = w, thresholds = 8.3, marginal_rates = 10, growth = 'average', name_tax = "8__10", scope_tax = w)
numbers$GiniThree <- compute_inequality(df = w, var = "Y3", return = 'gini')
w <- tax_revenues(df = w, thresholds = 8.3, marginal_rates = 10, return = 'df', growth = "average", name_tax = "8__10", scope_tax = w)
numbers$GiniThreeGlobalTax <- compute_inequality(df = w, var = "Y3_tax_8__10", return = 'gini')
# Marginal rate above $100/day that closes the global extreme poverty gap (revenues are linear in the rate)
numbers$RateHundred <- compute_poverty_gap(df = w, threshold = 3, unit = '%', growth = "average") / tax_revenues(df = w, thresholds = 100, marginal_rates = 1, growth = "average", return = '%', scope_tax = w)
numbers$RateHundredHFCE <- compute_poverty_gap(df = ws, threshold = 3, unit = '%', growth = "average") / tax_revenues(df = ws, thresholds = 100, marginal_rates = 1, growth = "average", return = '%', scope_tax = ws)
numbers$ShareBelowHundred <- 100 * compute_poverty_rate(df = ws, threshold = 100, growth = 'average')

# Same indicators with the 2017-PPP lines converted for prices only: $2.15 becomes $2.46 in 2021 PPP (Foster et al., 2025); the threshold of $6.85 is scaled by the same factor.
# This isolates the effect of the new data from that of the (higher) $3.00 line.
eq_line <- 2.46
eq_threshold <- 6.85 * eq_line / 2.15
numbers$EqLine <- eq_line
numbers$EqThreshold <- eq_threshold
numbers$RatePovNowEq <- 100 * compute_poverty_rate(df = w, threshold = eq_line, growth = "now")
numbers$RatePovThreeEq <- 100 * compute_poverty_rate(df = w, threshold = eq_line, growth = "average")
numbers$RatePovSevenEq <- 100 * compute_poverty_rate(df = w, threshold = eq_line, growth = "very_optimistic")
numbers$RatePovSDGEq <- 100 * compute_poverty_rate(df = w, threshold = eq_line, growth = "sdg8")
numbers$RatePovRealizedEq <- 100 * compute_poverty_rate(df = w, threshold = eq_line, growth = "realized")
numbers$GapThreeEq <- 100 * compute_poverty_gap(df = w, threshold = eq_line, unit = '%', growth = "average")
p$antipoverty_eq_tax_eq_average <- compute_antipoverty_tax(df = p, exemption_threshold = eq_threshold, poverty_threshold = eq_line, growth = "average")
numbers$NbCapEq <- sum(p$antipoverty_eq_tax_eq_average > 100, na.rm = T)
numbers$CountriesCapEq <- names_list(p$country_code[no.na(p$antipoverty_eq_tax_eq_average > 100, rep = F, num_as_char = F)])
numbers$TaxEqSSA <- wtd.mean(p$antipoverty_eq_tax_eq_average[p$country_code %in% SSA], p$pop_2030[p$country_code %in% SSA])
numbers$TaxEqLIC <- wtd.mean(p$antipoverty_eq_tax_eq_average[p$country_code %in% LIC], p$pop_2030[p$country_code %in% LIC])
numbers$TaxEqNGA <- c_("NGA", p$antipoverty_eq_tax_eq_average)
numbers$TaxEighteenThreeINDEq <- c_("IND", compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = eq_line, growth = "average"))
numbers$NbFloorEq <- sum(compute_income_floor(df = p, thresholds = eq_threshold, marginal_rates = 10, growth = "average", scope_tax = p) < eq_line, na.rm = T)
numbers$NbFloorSevenEq <- sum(compute_income_floor(df = p, thresholds = eq_threshold, marginal_rates = 10, growth = "very_optimistic", scope_tax = p) < eq_line, na.rm = T)
numbers$NbFloorSDGEq <- sum(compute_income_floor(df = p, thresholds = eq_threshold, marginal_rates = 10, growth = "sdg8", scope_tax = p) < eq_line, na.rm = T)
numbers$RateHundredEq <- compute_poverty_gap(df = w, threshold = eq_line, unit = '%', growth = "average") / tax_revenues(df = w, thresholds = 100, marginal_rates = 1, growth = "average", return = '%', scope_tax = w)
numbers$GlobalFloorEq <- compute_income_floor(df = w, thresholds = eq_threshold, marginal_rates = 10, growth = 'average', name_tax = "eq__10", scope_tax = w)

digits <- function(x) if (abs(x) >= 9.95 | x == round(x)) 0 else if (abs(x) >= 0.995) 1 else 2
cat(paste0("% Generated by code_poverty/update.R on ", Sys.Date(), "\n"),
    sapply(names(numbers), function(n) paste0("\\newcommand{\\up", n, "}{", if (is.numeric(numbers[[n]])) format(round(numbers[[n]], digits(numbers[[n]])), nsmall = digits(numbers[[n]]), big.mark = ",") else numbers[[n]], "}\n")),
    sep = "", file = "../tables/update_numbers.tex")
print(unlist(numbers))
