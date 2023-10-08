# Docs:
# En, 2p https://docs.google.com/document/d/12_JPK2F3Kj2CmRdMpL8hSPBPK-tImSLQc84Ordzz3tM/edit
# Fr https://docs.google.com/document/d/1u41m1U0FGlvt6aGKzZET3MWr0ulz3tORPw1xVRSKek0/edit?usp=sharing

# TODO!
# calculer Gini sans redistribution, et avec redistribution linéaire vs. expropriative => indicateur: réduction de Gini nécessaire pour éradiquer pauvreté.
# combine with tax data (WIL) to get better estimates of total GDP
# >reproduire le travail de Bolch et al. (2022)
# Other growth assumptions: >projections de PIB officielles; Make a 7% growth scenario (as in SDG 8.1); max(0, trend)
# Ajouter aux calculs la >basic needs poverty line de Moatsos. Try first with Moatsos 21 (more recent estimates). If results are not satisfactory, use Moatsos 16 (better methodology), cf. data https://clio-infra.eu/Indicators/GlobalExtremePovertyCostofBasicNeeds.html and https://clio-infra.eu/Indicators/GlobalExtremePovertyDollaraDay.html
# Cite Ortiz et al. (18), computing the costs of an UBI at the national poverty line (Figure 2, 3). On Theil, cite Chancel & Piketty (2021)

# Other costing of extreme poverty eradication: UNCTAD (21, p. 15: growth needed), Vorisek & Yu (20, lite review), SDSN (19, excellent: talk about ODA, wealth & carbon taxes, estimate domestic resources, e.g. Table 4), Moyer & Hedden (20), 
# World Bank (2022): "It became clear that the global goal of ending extreme poverty by 2030 would not be achieved."

##### Functions #####
name_var_growth <- function(growth = "optimistic") { 
  return(case_when(growth == "trend" ~ "y",
                    growth == "none" ~ "welfare",
                    growth == "now" ~ "y_2022",
                    growth == "optimistic" ~ "Y",
                    TRUE ~ ""))
} 
find_pop_share_var <- function(var, df) {
  if (paste0(sub("_tax.*|_min.*", "", var), "_pop_share_1") %in% names(df)) return(paste0(sub("_tax.*|_min.*", "", var), "_pop_share_"))
  else return("pop_share_")
}
# Average poverty gap. unit: 'mean' (in $/day/person), 'sum' (100 times 'mean'), '%' (in % of GDP), 'threshold' (in % of threshold), '$' (in $)
compute_poverty_gap <- function(df = p, threshold = 2.15, unit = "sum", growth = "optimistic") {
  y <- name_var_growth(growth)
  poverty_gaps <- sapply(1:100, function(i) { pmax(0, threshold - df[[paste0(y, "_avg_", i)]]) })
  pg <- if (!is.vector(poverty_gaps)) rowSums(poverty_gaps) else sum(poverty_gaps)
  if (unit != "sum") pg <- pg/100
  if (unit %in% c("percent", "%")) pg <- pg/df[[paste0("mean_", y)]]
  if (unit %in% c("threshold", "% threshold")) pg <- pg/threshold
  if (unit %in% c("money", "dollar", "$")) pg <- pg * 365 * ((growth == "now") * df$pop_2022 + (growth != "now") * df$pop_2030)
  return(pg)
}
# Percentile above which we expropriate all y to fill the poverty gap TODO? rename: anti-poverty cap
compute_antipoverty_maximum <- function(df = p, threshold = 2.15, return = "y", growth = "optimistic") { 
  y <- name_var_growth(growth)
  df$poverty_gap <- compute_poverty_gap(df = df, threshold = threshold, unit = "sum", growth = growth)
  df$percentile_expropriated <- 100
  df$y_expropriated <- Inf
  for (c in 1:nrow(df)) {
    funded <- 0
    while (funded < df$poverty_gap[c] & df$percentile_expropriated[c] > 0) { 
      df$y_expropriated[c] <- df[[paste0(y, "_min_", df$percentile_expropriated[c])]][c]
      funded <- funded + df[[paste0(y, "_avg_", df$percentile_expropriated[c])]][c] - df$y_expropriated[c] + (100 - df$percentile_expropriated[c]) * (df[[paste0(y, "_max_", df$percentile_expropriated[c])]][c] - df$y_expropriated[c])
      df$percentile_expropriated[c] <- df$percentile_expropriated[c] - 1
      # By convention, if we cannot close the poverty gap in the country, we set percentile_expropriated to 0 and y_expropriated at gdp_pc_2030
      if (df[[paste0(y, "_min_", df$percentile_expropriated[c])]][c] < threshold) {
        df$percentile_expropriated[c] <- 0
        df$y_expropriated[c] <- df[[paste0("mean_", y)]][c] # df$gdp_pc_2030[c]/365
      }
    }
  }
  if (return == "percentile") return(df$percentile_expropriated)
  else if (return == "df") {
    for (i in 1:100) df[[paste0(y, "_min_", round(poverty_threshold), "_cap_avg_", i)]] <- pmax(threshold, pmin(df$y_expropriated, df[[paste0(y, "_avg_", i)]]))
    df[[paste0("mean_", y, "_min_", round(poverty_threshold), "_cap")]] <- rowSums(df[,paste0(y, "_min_", round(poverty_threshold), "_cap_avg_", 1:100)] * df[,paste0(find_pop_share_var(y, df), 1:100)])
    df <- compute_inequality(var = paste0(y, "_min_", round(poverty_threshold), "_cap"), df = df, return = "df")
    return(df)
  }
  else return(df$y_expropriated)
}
compute_antipoverty_tax <- function(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, return = "tax", growth = "optimistic") {
  y <- name_var_growth(growth)
  taxable_base_all <- sapply(1:100, function(i) { pmax(0, df[[paste0(y, "_avg_", i)]] - exemption_threshold) })
  df$taxable_base <- if (!is.vector(taxable_base_all)) rowSums(taxable_base_all) else sum(taxable_base_all)
  df$antipoverty_tax <- 100 * compute_poverty_gap(df = df, threshold = poverty_threshold, growth = growth) / df$taxable_base
  if (return == "base") return(df$taxable_base)
  else if (return == "df") {
    for (i in 1:100) df[[paste0(y, "_min_", round(poverty_threshold), "_tax_", round(exemption_threshold),  "_avg_", i)]] <- pmax(poverty_threshold, df[[paste0(y, "_avg_", i)]] - df$antipoverty_tax * pmax(0, df[[paste0(y, "_avg_", i)]] - exemption_threshold))
    df[[paste0("mean_", y, "_min_", round(poverty_threshold), "_tax_", round(exemption_threshold))]] <- rowSums(df[,paste0(y, "_min_", round(poverty_threshold), "_tax_", round(exemption_threshold),  "_avg_", 1:100)] * df[,paste0(find_pop_share_var(y, df), 1:100)])
    df <- compute_inequality(var = paste0(y, "_min_", round(poverty_threshold), "_tax_", round(exemption_threshold)), df = df, return = "df")
    return(df)
  }
  else return(df$antipoverty_tax)
}
# Computes the demogrant that can be funded with given revenues (in $ per person) 
compute_min_funded <- function(revenues, var = name_var_growth("optimistic"), df = w, return = "min") {
  if (length(revenues) == 1) {
    for (i in 1:100) df[[paste0("pop_share_", i)]] <- if (sub("_tax.*|_min.*", "", paste0(var, "_pop_share_", i)) %in% names(df)) df[[sub("_tax.*|_min.*", "", paste0(var, "_pop_share_", i))]] else df[[paste0("pop_share_", i)]]
    cost <- df[[paste0(var, "_avg_", 1)]] * df[[paste0("pop_share_", 1)]]
    cumulated_pop <- df[[paste0("pop_share_", 1)]]
    i <- 1
    while (cost < revenues) {
      cost <- cost + (df[[paste0(var, "_avg_", i+1)]] - df[[paste0(var, "_avg_", i)]]) * cumulated_pop
      cumulated_pop <- cumulated_pop + df[[paste0("pop_share_", i+1)]]
      i <- i+1
    }
    demogrant <- df[[paste0(var, "_avg_", i)]] - (cost - revenues)/cumulated_pop
    for (j in 1:i) df[[paste0(var, "_avg_", j)]] <- demogrant
    if (return == "df") return(df)
    else if (return == "percentile") return(cumulated_pop)
    else return(demogrant)
  } else if (nrow(df) == 1) { return(compute_min_funded(revenues = revenues, var = var, df = df, return = "min")) # Unused. Beware, revenues must be in $/person
  } else return(sapply(1:length(revenues), function(i) compute_min_funded(revenues = revenues[i], var = var, df = df[i,], return = "min")))
}
tax_revenues <- function(thresholds, marginal_rates, name_tax = "custom", df = p, growth = "optimistic", return = '%', var = name_var_growth(growth), scope_tax = w) { 
  # thresholds (in $/day) and marginal_rates (in %) should be vectors of same length
  # scope_tax can be p or w depending on whether the revenues are recycled nationally or internationally
  df$revenues <- 0
  marginal_rates <- c(0, marginal_rates)
  for (i in 1:length(thresholds)) {
    taxable_base_all <- sapply(1:100, function(j) { pmax(0, df[[paste0(var, "_avg_", j)]] - thresholds[i])/100 })
    df$taxable_base_i <- if (!is.vector(taxable_base_all)) rowSums(taxable_base_all) else sum(taxable_base_all)
    df$revenues <- df$revenues + df$taxable_base_i * (marginal_rates[i+1] - marginal_rates[i])/100
  }
  pops <- ((growth == "now") * df$pop_2022 + (growth != "now") * df$pop_2030)
  if (return == 'pc') return(df$revenues)
  else if (return == 'total') return(df$revenues * 365 * pops)
  else if (return == "df") { 
    revenues <- if (nrow(scope_tax) == 1) sum(df$revenues * pops)/sum(pops) else df$revenues
    demogrant <- compute_min_funded(revenues, var = var, df = scope_tax, return = "min")
    for (i in 1:100) df[[paste0(var, "_tax_", name_tax,  "_avg_", i)]] <- pmax(demogrant, df[[paste0(var, "_avg_", i)]])
    for (j in 1:length(thresholds)) for (i in 1:100) df[[paste0(var, "_tax_", name_tax,  "_avg_", i)]] <- df[[paste0(var, "_tax_", name_tax,  "_avg_", i)]]  - (marginal_rates[j+1]/100) * pmax(0, df[[paste0(var, "_avg_", i)]] - thresholds[j])
    df[[paste0("mean_", var, "_tax_", name_tax)]] <- rowSums(df[,paste0(var, "_tax_", name_tax,  "_avg_", 1:100)] * df[,paste0(find_pop_share_var(var, df), 1:100)])
    df <- compute_inequality(var = paste0(var, "_tax_", name_tax), df = df, return = "df")
    return(df)
  }
  else if (return %in% c("%", "% GDP")) return(df$revenues / df[[paste0("mean_", var)]])
}
compute_inequality <- function(var = name_var_growth("optimistic"), df = p, return = "df") {
  d <- df
  for (i in 1:100) d[[paste0("pop_share_", i)]] <- df[[paste0(find_pop_share_var(var, df), i)]]
  d$y_cumulated_0 <- 0
  for (i in 1:100) d[[paste0("y_cumulated_", i)]] <- d[[paste0("y_cumulated_", i-1)]] + d[[paste0("pop_share_", i)]] * df[[paste0(var, "_avg_", i)]] / df[[paste0("mean_", var)]]
  antigini <- sapply(1:100, function(i) { d[[paste0("pop_share_", i)]] * (d[[paste0("y_cumulated_", i)]] + d[[paste0("y_cumulated_", i-1)]])}) # Brown formula
  if (!paste0(var, "_gini") %in% names(df)) df[[paste0(var, "_gini")]] <- 1 - if (!is.vector(antigini)) rowSums(antigini) else sum(antigini)
  if (!paste0(var, "_top1") %in% names(df)) df[[paste0(var, "_top1")]] <- 0.01*(df[[paste0(var, "_avg_100")]]/d$pop_share_100)/df[[paste0("mean_", var)]]
  if (!paste0(var, "_top10") %in% names(df)) df[[paste0(var, "_top10")]] <- 0.1*(sum(sapply(91:100, function(i) { df[[paste0(var, "_avg_", i)]] }))/sum(sapply(91:100, function(i) { d[[paste0("pop_share_", i)]] })))/df[[paste0("mean_", var)]]
  if (!paste0(var, "_bottom50") %in% names(df)) df[[paste0(var, "_bottom50")]] <- 0.5*(sum(sapply(1:50, function(i) { df[[paste0(var, "_avg_", i)]] }))/sum(sapply(1:50, function(i) { d[[paste0("pop_share_", i)]] })))/df[[paste0("mean_", var)]]
  if (!paste0(var, "_d9d1") %in% names(df)) df[[paste0(var, "_d9d1")]] <- df[[paste0(var, "_avg_90")]]/df[[paste0(var, "_avg_10")]] 
  if (!paste0(var, "_d9d5") %in% names(df)) df[[paste0(var, "_d9d5")]] <- df[[paste0(var, "_avg_90")]]/df[[paste0(var, "_avg_50")]]
  # if (!paste0(var, "_d9d1") %in% names(df)) df[[paste0(var, "_d9d1")]] <- df[[paste0(var, "_max_90")]]/df[[paste0(var, "_max_10")]] # df[[paste0(var, "_max_10")]] == 0 for SUR
  # if (!paste0(var, "_d9d5") %in% names(df)) df[[paste0(var, "_d9d5")]] <- df[[paste0(var, "_max_90")]]/df[[paste0(var, "_max_50")]] # Better in theory, but not implemented for after-tax distributions
  entropy <- sapply(1:100, function(i) { d[[paste0("pop_share_", i)]] * log(pmax(1e-10, df[[paste0(var, "_avg_", i)]])) })
  if (!paste0(var, "_theil") %in% names(df)) df[[paste0(var, "_theil")]] <- log(df[[paste0("mean_", var)]]) - if (!is.vector(entropy)) rowSums(entropy) else sum(entropy) # I use Theil L. To normalize, use 1 - exp(-df[[paste0(var, "_theil")]])
  # if (nrow(df) > 1) df[[paste0(var, "_coef_theil")]] <- df[[paste0("pop_", if (grepl("2022|now", var)) "2022" else "2030")]] * df[[paste0("mean_", var)]] # / (pop_tot * mean_world)
  pop_yr <- paste0("pop_", if (grepl("2022|now", var)) "2022" else "2030")
  # if (nrow(df) > 1) theil_T_within <- sum(df[[paste0(var, "_theil")]] * (sum(df[[pop_yr]] * df[[paste0("mean_", var)]]) / (sum(df[[pop_yr]]) * sum(df[[pop_yr]] * df[[paste0("mean_", var)]]))))
  if (nrow(df) > 1) theil_within <- sum(df[[paste0(var, "_theil")]] * (df[[pop_yr]]/sum(df[[pop_yr]])))
  if (nrow(df) > 1) theil_between <- sum((df[[pop_yr]]/sum(df[[pop_yr]])) * log(df[[pop_yr]]/sum(df[[pop_yr]]) / (df[[pop_yr]]*df[[paste0("mean_", var)]] / sum(df[[pop_yr]] * df[[paste0("mean_", var)]]))))
  theil <- if (nrow(df) > 1) (theil_between + theil_within) else df[[paste0(var, "_theil")]]
  
  ineq <- df[, paste0(var, c("_top1", "_top10", "_bottom50", "_d9d1", "_d9d5", "_gini", "_theil"))]
  if (return == "df") {
    if (nrow(df) == 1) print(ineq) 
    else print(paste0("Theil-L: ", round(theil_within + theil_between, 3), " [within: ", round(theil_within, 3), " (", round(100*theil_within/theil), "%) / between: ", round(theil_between, 3), " (", round(100*theil_between/theil), "%)]"))
    return(df)
  } else if (return %in% c("gini", "Gini")) { return(df[[paste0(var, "_gini")]])
  } else if (return %in% c("top1")) { return(df[[paste0(var, "_top1")]])
  } else if (return %in% c("top10")) { return(df[[paste0(var, "_top10")]])
  } else if (return %in% c("bottom50")) { return(df[[paste0(var, "_bottom50")]])
  } else if (return %in% c("d9d1")) { return(df[[paste0(var, "_d9d1")]]) 
  } else if (return %in% c("d9d5")) { return(df[[paste0(var, "_d9d5")]]) 
  } else if (return %in% c("theil")) { return(df[[paste0(var, "_theil")]]) 
  } else if (return %in% c("theil_within")) { return(theil_within) 
  } else if (return %in% c("theil_between")) { return(theil_between) 
  } else if (return %in% c("theil_within_share")) { return(theil_within/theil) 
  } else if (return %in% c("theil_between_share")) { return(theil_between/theil) 
  } else if (return %in% c("theil")) { return(theil) 
  } else if (return %in% c("table", "ineq", "summary")) { return(ineq) 
  } else warning("'return' unknown")
}
# TODO! pourquoi w$mean_Y != w$mean_Y_tax_min8?

##### Data #####
# PIP/PovcalNet data is *per capita* (without adjustment for household composition).
{ # ~ 8 min
start <- Sys.time()
data <- read.csv("../data/Povcalnet 2017.csv") # https://datacatalogfiles.worldbank.org/ddh-published/0063646/DR0090251/world_100bin.csv?versionId=2023-05-31T15:19:01.1473846Z on https://datacatalog.worldbank.org/search/dataset/0063646
# AF: il vaut mieux écrire le code en anglais pour qu'il puisse être compris par le monde entier
# Croissance_pays <- read_excel("../data/Croissance pays.xls") # AF: c'est pas dans le répertoire ! Faut mettre ces trucs dans un dossier /Data dans le répertoire github
# PIB_capita <- read_excel("../data/PIB_capita.xls") # AF: same here # NY.GDP.PCAP.PP.KD	GDP per capita, PPP (constant 2017 international $)
# AF: il faut que vous rajoutiez en commentaire l'URL et la date où vous avez téléchargé les données

# Data cleaning
temp <- data %>% group_by(country_code) %>% dplyr::summarize(year_max = max(year))
year_max <- setNames(temp$year_max, temp$country_code)
data$year_max <- year_max[data$country_code]
# data <- data[data$year == data$year_max,]
p <- data[data$year == data$year_max,] %>% pivot_wider(names_from = percentile, values_from = c(avg_welfare, pop_share, welfare_share, quantile))
names(p) <- sub("avg_welfare_", "welfare_avg_", names(p), fixed = T)
p$mean_welfare <- rowSums(df[,paste0("welfare_avg_", 1:100)] * df[,paste0("pop_share_", 1:100)])

# Add population
pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
pop$PopTotal <- 1e3 * pop$PopTotal
pop <- pop[pop$Time %in% c(sort(unique(p$year)), 2030),]
pop_iso3 <- aggregate(PopTotal ~ Time + ISO3_code, data = pop, FUN = sum)
pop_iso3 <- pop_iso3 %>% pivot_wider(names_from = Time, values_from = PopTotal)
names(pop_iso3) <- c("country_code", paste0("pop_", names(pop_iso3)[-1]))
p <- merge(p, pop_iso3)
p$pop_year <- sapply(1:nrow(p), function(c) { p[[paste0("pop_", p$year[c])]][c] }) # in thousands

# Estimate future GDP pc
gdp_pc <- read_excel("../data/gdp_pc_ppp.xls") # NY.GDP.PCAP.PP.KD
colnames(gdp_pc)[-1] <- paste0("gdp_pc_", colnames(gdp_pc)[-1])
p <- merge(p, gdp_pc)
# p$gdp_pc_2019_over_2014 <- p$gdp_pc_2019/p$gdp_pc_2014
p$gdp_pc_2022[is.na(p$gdp_pc_2022)] <- pmax(p$gdp_pc_2021, pmax(p$gdp_pc_2020, p$gdp_pc_2019, na.rm = T), na.rm = T)[is.na(p$gdp_pc_2022)]
p$mean_growth_gdp_pc_14_19 <- pmax(0, (p$gdp_pc_2019/p$gdp_pc_2014)^(1/5)-1)
# sort(setNames(p$mean_growth_gdp_pc_14_19, p$country), decreasing = T) # max 14-19: CN = 6.15%, max 10-19:  CN 6.7%
p$gdp_pc_year <- sapply(1:nrow(p), function(c) { p[[paste0("gdp_pc_", min(2022, p$year[c]))]][c] }) 

# Add country name
iso3 <- read.csv("../data/country_iso3.csv")
# iso3$country_code[!iso3$country_code %in% p$country_code] # TODO: many countries absent! AFG, CUB, LBY...
p <- merge(p, iso3)
p$country[p$country_code == "RUS"] <- "Russia"
p$country[p$country_code == "BOL"] <- "Bolivia"
p$country[p$country_code == "GUF"] <- "France"
p$country[p$country_code == "IRN"] <- "Iran"
p$country[p$country_code == "KOR"] <- "South Korea"
p$country[p$country_code == "LAO"] <- "Laos"
p$country[p$country_code == "MDA"] <- "Moldova"
p$country[p$country_code == "PSE"] <- "Palestine"
p$country[p$country_code == "SWZ"] <- "Swaziland"
p$country[p$country_code == "SYR"] <- "Syria"
p$country[p$country_code == "TZA"] <- "Tanzania"
p$country[p$country_code == "VEN"] <- "Venezuela"
p$country[p$country_code == "VNM"] <- "Vietnam"
#Missing in PIP : Afghanistan, Brunei, Cuba, Erythrée, Guinée équatoriale, Cambodge, Kuwait, Lybie, Liechtenstein, Nouvelle Calédonie, Nouvelle Zélande, Oman, Puerto Rico, Qatar, Sahara Occidental, Arabie Saoudite, Singapore, Somalie, Taiwan
countries_names <- setNames(p$country, p$country_code)

pop_rural_urban <- read.csv2("../data/pop_rural_urban.csv") # Last updated 07/05/2023 https://databank.worldbank.org/source/population-estimates-and-projections/preview/on#

compute_distribution_2030 <- function(growth = "optimistic", growth_rate = 1.06, name_var = NULL, df = p, pop_rurb = pop_rural_urban) {
  y <- if (is.null(name_var)) name_var_growth(growth) else name_var
  if (growth == "trend") { # TODO create another assumption: max(0, trend)
    df$gdp_pc_2030 <- df$gdp_pc_2022 * (1 + df$mean_growth_gdp_pc_14_19)^8 # TODO: check whether this hypothesis makes sense (or whether we should use gdp projections / pop projection instead)
    df$growth_gdp_pc_year_30 <- df$gdp_pc_2030/df$gdp_pc_year
    # df$country_code[is.na(df$gdp_pc_year)] # "SSD" "SYR" "VEN" "YEM"
    # df$country_code[is.na(df$growth_gdp_pc_year_30)] # "SSD" "SYR" "TKM" "VEN" "YEM"
    df$growth_gdp_pc_year_30[is.na(df$growth_gdp_pc_year_30)] <- 1 # TODO: improve this assumption
    growths <- df$growth_gdp_pc_year_30
  } else if (growth == "optimistic") {
    df$gdp_pc_max_2030 <- df$gdp_pc_2022 * growth_rate^8 # 1.08^9 = 1.999, 1.07^9 = 1.84, 1.06^9 = 1.7, CN 99-07: 1.095^9 = 2.26. Beyond 6.3%, RDC antipoverty_2_tax_7 < 100%
    df$growth_gdp_pc_max_year_30 <- df$gdp_pc_max_2030/df$gdp_pc_year
    df$growth_gdp_pc_max_year_30[is.na(df$growth_gdp_pc_max_year_30)] <- growth_rate^(2030 - df$year[is.na(df$growth_gdp_pc_max_year_30)])
    growths <- df$growth_gdp_pc_max_year_30
  } else if (growth == "now") {
    df$growth_gdp_pc_2022 <- df$gdp_pc_2022/df$gdp_pc_year
    df$growth_gdp_pc_2022[is.na(df$growth_gdp_pc_2022)] <- growth_rate^(2022 - df$year[is.na(df$growth_gdp_pc_2022)])
    growths <- df$growth_gdp_pc_2022
  } else if (growth == "none") growths <- rep(1, nrow(df))
  for (i in 1:100) df[[paste0(y, "_avg_", i)]] <- df[[paste0("welfare_avg_", i)]] * growths
  for (i in 1:100) df[[paste0(y, "_max_", i)]] <- df[[paste0("quantile_", i)]] * growths
  for (i in 2:100) df[[paste0(y, "_min_", i)]] <- df[[paste0("quantile_", i-1)]] * growths
  df[[paste0(y, "_min_1")]] <- df[[paste0(y, "_max_0")]] <- 0
  df[[paste0(y, "_max_100")]][is.na(df[[paste0(y, "_max_100")]])] <- df[[paste0(y, "_avg_100")]][is.na(df[[paste0(y, "_max_100")]])]
  
  # Manage CN, IA, ID rural/urban (/!\ in ARG, SUR, there is only urban as reporting_level)
  for (c in c("CHN", "IDN", "IND")) { 
    u <- df$country_code == c & df$reporting_level == "urban"
    r <- df$country_code == c & df$reporting_level == "rural"
    yr <- if (growth == "now") "yr_2022" else "yr_2030"
    if (sum(df$reporting_level == "national" & df$country_code == c) == 0) {
      frac_urb <- pop_rurb[[yr]][pop_rurb$country_code == c & pop_rurb$reporting_level == "urban"]/pop_rurb[[yr]][pop_rurb$reporting_level == "national" & pop_rurb$country_code == c]
      for (v in names(df)[grepl("pop_share_", names(df))]) df[[v]][u] <- df[[v]][u] * frac_urb
      for (v in names(df)[grepl("pop_share_", names(df))]) df[[v]][r] <- df[[v]][r] * (1 - frac_urb)
      new_line <- df[u, ] # TODO? Create new_line with all NAs instead? Here we impute wrong values for certain variables (e.g. gdp...)
      new_line$reporting_level <- "national"
      df <- rbind(df, new_line)
    }
    quantiles <- sort(unlist(sapply(1:100, function(i) {df[[paste0(y, "_avg_", i)]][df$country_code == c] }))) # Before, was _max_ here in and cdf, but this didn't work (some avg were missing). TODO? recompute df[[paste0(y, "_max_", i)]][n] with _max_ instead of _avg_?
    cdf <- c() # TODO!! solve bug
    for (q in quantiles) cdf <- c(cdf, sum(sapply(1:100, function(j) { (df[[paste0(y, "_avg_", j)]][u] <= q) * df[[paste0("pop_share_", j)]][u] + (df[[paste0(y, "_avg_", j)]][r] <= q) * df[[paste0("pop_share_", j)]][r] })))
    percentiles <- findInterval(seq(0, 1, .01), cdf)[-1] # was , left.open = T; computes the indices for which the pop_share is lesser or equal to the percentiles.
    n <- df$country_code == c & df$reporting_level == "national"
    for (i in 1:100) {
      df[[paste0(y, "_max_", i)]][n] <- quantiles[percentiles[i]]
      if (i == 1) df$pop_share_1[n] <- cdf[percentiles[1]] 
      else df[[paste0("pop_share_", i)]][n] <- cdf[percentiles[i]] - cdf[percentiles[i-1]]
      df[[paste0(y, "_min_", i)]][n] <- df[[paste0(y, "_max_", i-1)]][n]
      df[[paste0(y, "_avg_", i)]][n] <- (
        sum(sapply(1:100, function(k) { df[[paste0("pop_share_", k)]][u] * df[[paste0(y, "_avg_", k)]][u] * (df[[paste0(y, "_avg_", k)]][u] <= df[[paste0(y, "_max_", i)]][n]) * (df[[paste0(y, "_avg_", k)]][u] > df[[paste0(y, "_max_", i-1)]][n]) })) +
        sum(sapply(1:100,function(k){df[[paste0("pop_share_", k)]][r] * df[[paste0(y, "_avg_", k)]][r] * (df[[paste0(y, "_avg_", k)]][r] <= df[[paste0(y, "_max_", i)]][n]) * (df[[paste0(y, "_avg_", k)]][r] > df[[paste0(y, "_max_", i-1)]][n]) }))) / (
            sum(sapply(1:100, function(k) { df[[paste0("pop_share_", k)]][u] * (df[[paste0(y, "_avg_", k)]][u] <= df[[paste0(y, "_max_", i)]][n]) * (df[[paste0(y, "_avg_", k)]][u] > df[[paste0(y, "_max_", i-1)]][n]) })) +
            sum(sapply(1:100,function(k){df[[paste0("pop_share_", k)]][r] * (df[[paste0(y, "_avg_", k)]][r] <= df[[paste0(y, "_max_", i)]][n]) * (df[[paste0(y, "_avg_", k)]][r] > df[[paste0(y, "_max_", i-1)]][n]) })) )
    }
    # df <- df[df$country_code != c | df$reporting_level == "national",]
  }
  df[[paste0("mean_", y)]] <- rowSums(df[,paste0(y, "_avg_", 1:100)] * df[,paste0("pop_share_", 1:100)])
  wdf <- compute_inequality(var = y, df = df, return = "df")
  
  return(df)
}

# Create world income distribution in 2030 
# (y makes the assumption of constant growth while Y assumes 6% growth after 2022)
compute_world_distribution <- function(var = name_var_growth("optimistic"), df = p, wdf = w) {
  pop_yr <- if (grepl("2022|now", var)) "pop_2022" else "pop_2030"
  wquantiles <- unique(sort(unlist(sapply(1:100, function(i) {df[[paste0(var, "_max_", i)]] }))))
  wcdf <- c() # ~ 1 min
  for (q in wquantiles) wcdf <- c(wcdf, sum(sapply(1:100, function(j) { sum((df[[paste0(var, "_max_", j)]] <= q) * df[[paste0("pop_share_", j)]] * df[[pop_yr]]) })))
  wpop <- wcdf[length(wcdf)]
  wcdf <- wcdf/wpop
  wpercentiles <- findInterval(seq(0, 1, .01), wcdf)[-1] # computes the indices for which the pop_share is lesser or equal to the percentiles.
  # w <- data.frame("pop_2030" = wpop) # df[df$country_code == "USA", !names(p) %in% c("country", "country_code")]
  wdf[[paste0(var, "_", pop_yr)]] <- wpop
  wdf[[paste0(var, "_max_0")]] <- 0
  for (i in 1:100) {
    wdf[[paste0(var, "_max_", i)]] <- wquantiles[wpercentiles[i]]
    if (i == 1) wdf[[paste0(var, "_pop_share_1")]] <- wdf[[paste0(var, "_pop_share_1")]] <- wcdf[wpercentiles[1]] 
    else wdf[[paste0(var, "_pop_share_", i)]] <- wcdf[wpercentiles[i]] - wcdf[wpercentiles[i-1]]
    wdf[[paste0(var, "_min_", i)]] <- wdf[[paste0(var, "_max_", i-1)]]
    wdf[[paste0(var, "_avg_", i)]] <- (
      sum(sapply(1:100, function(k) { sum(df[[pop_yr]] * df[[paste0("pop_share_", k)]] * df[[paste0(var, "_avg_", k)]] * (df[[paste0(var, "_avg_", k)]] <= wdf[[paste0(var, "_max_", i)]]) * (df[[paste0(var, "_avg_", k)]] > wdf[[paste0(var, "_max_", i-1)]]), na.rm = T) }))) / (
        sum(sapply(1:100, function(k) { sum(df[[pop_yr]] * df[[paste0("pop_share_", k)]] * (df[[paste0(var, "_avg_", k)]] <= wdf[[paste0(var, "_max_", i)]]) * (df[[paste0(var, "_avg_", k)]] > wdf[[paste0(var, "_max_", i-1)]]), na.rm = T) })))
  }
  wdf[[paste0("mean_", var)]] <- rowSums(df[,paste0(var, "_avg_", 1:100)] * df[,paste0(var, "_pop_share_", 1:100)]) # mean(t(wdf[,grepl(paste0(var, "_avg"), names(wdf))]))
  
  wdf <- compute_inequality(var = var, df = wdf, return = "df")
  return(wdf)
}
# TODO Use 7% growth: the SDG 8.1. Compute the antipoverty_tax, etc. that would have been needed with a 7% growth starting in 2016.
# y makes the assumption of constant growth while Y assumes 6% growth after 2022
# p <- df # To run compute_distribution_2030, this line is needed to avoid bug (Indeed, urban/rural have been removed otherwise).
p <- compute_distribution_2030(growth = "optimistic", growth_rate = 1.06)
p <- compute_distribution_2030(growth = "optimistic", growth_rate = 1.1, name_var = "Y10")
p <- compute_distribution_2030(growth = "trend")
p <- compute_distribution_2030(growth = "none")
df <- p <- compute_distribution_2030(growth = "now")
# df <- p

p <- p[(!p$country_code %in% c("CHN", "IDN", "IND")) | p$reporting_level == "national",]

w <- data.frame(country = "World", pop_2022 = sum(p$pop_2022), pop_2030 = sum(p$pop_2030))
w <- compute_world_distribution(name_var_growth("optimistic"))  # ~ 1.5 min
w <- compute_world_distribution("Y10")  # ~ 1.5 min
w <- compute_world_distribution(name_var_growth("trend"))
w <- compute_world_distribution(name_var_growth("none"))
w <- compute_world_distribution(name_var_growth("now"))
w.bak <- w
print(Sys.time() - start)
beep()
}


##### Computations #####
# p <- compute_inequality(var = name_var_growth("optimistic"), df = p, return = "df")
# w <- compute_inequality(var = name_var_growth("optimistic"), df = w, return = "df")
(w$poverty_gap_2 <- compute_poverty_gap(df = w, threshold = 2.15, unit = 'threshold', growth = "now")) # 3.1% estimated in 2022 vs. official 2019: 2.6% (higher because 2019 vs. 2022?) official: https://data.worldbank.org/indicator/SI.POV.GAPS
(w$poverty_gap_4 <- compute_poverty_gap(df = w, threshold = 3.65, unit = 'threshold', growth = "now")) # 8.1% vs. official: 8%
(w$poverty_gap_7 <- compute_poverty_gap(df = w, threshold = 6.85, unit = 'threshold', growth = "now")) # 19% vs. official: 21%
(w$poverty_gap_8 <- tax_revenues(df = w, thresholds = c(33, 66, 100, 200, 300), marginal_rates = c(1, 4, 8, 20, 30), return = '%', growth = "optimistic")) # 1.18% of world GDP
compute_min_funded(revenues = tax_revenues(df = w, thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(1, 4, 8, 20, 30), return = 'pc', growth = "optimistic"))
w <- tax_revenues(df = w, name_tax = "min8", thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(1, 4, 8, 20, 30), return = 'df', growth = "optimistic")
df <- tax_revenues(df = p, scope_tax = w, name_tax = "min8", thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(1, 4, 8, 20, 30), return = 'df', growth = "optimistic")
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 8.22, unit = '%', growth = "optimistic")) # 3.6% of world GDP to reach 250$/month
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 8.22, unit = '$', growth = "optimistic")) # 3.5T$ to reach 250$/month
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 8.22, unit = 'threshold', growth = "optimistic")) # 14%: 1.15 $/day/person to reach 250$/month
(w$poverty_gap_35 <- compute_poverty_gap(df = w, threshold = 35, unit = 'mean', growth = "optimistic")) # 9 $/day/person to reach world average
(w$antipoverty_8_tax_66 <- compute_antipoverty_tax(df = w, exemption_threshold = 66.67, poverty_threshold = 8.22, growth = "optimistic")) # 2% tax above 2000$/month to finance 250$/month to everyone
(w$antipoverty_8_tax_100 <- compute_antipoverty_tax(df = w, exemption_threshold = 100, poverty_threshold = 8.22, growth = "optimistic")) # 4.2
(w$antipoverty_10_tax_100 <- compute_antipoverty_tax(df = w, exemption_threshold = 100, poverty_threshold = 10, growth = "optimistic")) # 8.66
(w$y_expropriated_9 <- compute_antipoverty_maximum(df = w, threshold = 9, growth = "optimistic")) # 470


plot(0:100, t(w[,c("y_avg_1", paste0("y_max_", 1:99), "y_avg_100")]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:98, t(w[,c("y_avg_1", paste0("y_max_", 1:98))]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:100, t(w[,c("Y10_avg_1", paste0("Y10_max_", 1:99), "Y10_avg_100")]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:98, t(w[,c("Y10_avg_1", paste0("Y10_max_", 1:98))]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:100, t(w[,c("welfare_avg_1", paste0("welfare_max_", 1:99), "welfare_avg_100")]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:98, t(w[,c("welfare_avg_1", paste0("welfare_max_", 1:98))]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()

plot(0:100, t(w[,c("Y_avg_1", paste0("Y_avg_", 1:99), "Y_avg_100")]), col = 'red', type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
lines(0:100, t(w[,c("Y_tax_min8_avg_1", paste0("Y_tax_min8_avg_", 1:100))]), type = "l", col = 'darkgreen', lwd = 2) 
plot(0:99, t(w[,c("Y_avg_1", paste0("Y_avg_", 1:99))]), type = "l", col = 'red', lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
lines(0:99, t(w[,c("Y_tax_min8_avg_1", paste0("Y_tax_min8_avg_", 1:99))]), type = "l", col = 'darkgreen', lwd = 2) 
plot(80:99, t(w[,c(paste0("Y_avg_", 80:99))]), type = "l", col = 'red', lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
lines(80:99, t(w[,c(paste0("Y_tax_min8_avg_", 80:99))]), type = "l", col = 'darkgreen', lwd = 2) 
compute_gini(df = w, var = "Y", return = "gini") # .63
compute_gini(df = w, var = "Y_tax_min8", return = "gini") # .56
sort(setNames(compute_gini(df = p, var = "Y", return = "gini"), p$country))


# Poverty gap
p$poverty_gap_2 <- compute_poverty_gap(growth = "trend")
p$poverty_gap_4 <- compute_poverty_gap(threshold = 3.65, growth = "trend")
p$poverty_gap_7 <- compute_poverty_gap(threshold = 6.85, growth = "trend")

# Percentile expropriated
p$percentile_expropriated_2 <- compute_antipoverty_maximum(df = p, threshold = 2.15, return = "percentile", growth = "trend")
p$percentile_expropriated_4 <- compute_antipoverty_maximum(df = p, threshold = 3.65, return = "percentile", growth = "trend")
p$percentile_expropriated_7 <- compute_antipoverty_maximum(df = p, threshold = 6.85, return = "percentile", growth = "trend")
p$percentile_expropriated_13 <- compute_antipoverty_maximum(df = p, threshold = 13, return = "percentile", growth = "trend")

p$y_expropriated_2 <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "trend")
p$y_expropriated_4 <- compute_antipoverty_maximum(df = p, threshold = 3.65, growth = "trend")
p$y_expropriated_7 <- compute_antipoverty_maximum(df = p, threshold = 6.85, growth = "trend")
p$y_expropriated_13 <- compute_antipoverty_maximum(df = p, threshold = 13, growth = "trend")

p$percentile_expropriated_2_optimistic <- compute_antipoverty_maximum(df = p, threshold = 2.15, return = "percentile", growth = "optimistic")
p$percentile_expropriated_4_optimistic <- compute_antipoverty_maximum(df = p, threshold = 3.65, return = "percentile", growth = "optimistic")
p$percentile_expropriated_7_optimistic <- compute_antipoverty_maximum(df = p, threshold = 6.85, return = "percentile", growth = "optimistic")
p$percentile_expropriated_13_optimistic <- compute_antipoverty_maximum(df = p, threshold = 13, return = "percentile", growth = "optimistic")

p$y_expropriated_2_optimistic <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "optimistic")
p$y_expropriated_4_optimistic <- compute_antipoverty_maximum(df = p, threshold = 3.65, growth = "optimistic")
p$y_expropriated_7_optimistic <- compute_antipoverty_maximum(df = p, threshold = 6.85, growth = "optimistic")
p$y_expropriated_13_optimistic <- compute_antipoverty_maximum(df = p, threshold = 13, growth = "optimistic")

# Antipoverty tax
p$antipoverty_2_tax_13 <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 2.15, growth = "trend")
p$antipoverty_2_tax_7 <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "trend")
p$antipoverty_2_tax_4 <- compute_antipoverty_tax(df = p, exemption_threshold = 3.65, poverty_threshold = 2.15, growth = "trend")
p$antipoverty_2_tax_2 <- compute_antipoverty_tax(df = p, exemption_threshold = 2.15, poverty_threshold = 2.15, growth = "trend")
p$antipoverty_4_tax_7 <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 3.65, growth = "trend")
p$antipoverty_4_tax_13 <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 3.65, growth = "trend")
p$antipoverty_7_tax_13 <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 6.85, growth = "trend")
p$antipoverty_13_tax_13 <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 13, growth = "trend")

p$antipoverty_2_tax_13_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_7_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_4_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 3.65, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_2_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 2.15, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_4_tax_7_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 3.65, growth = "optimistic")
p$antipoverty_4_tax_13_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 3.65, growth = "optimistic")
p$antipoverty_7_tax_13_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 6.85, growth = "optimistic")
p$antipoverty_13_tax_13_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 13, growth = "optimistic")

##### Results ##### 
sort(setNames(p$poverty_gap_2, p$country))
sort(setNames(p$antipoverty_2_tax_2, p$country))
sort(setNames(p$antipoverty_2_tax_7_optimistic, p$country))
sort(setNames(p$y_expropriated_2, p$country), decreasing = T)
sum(p$y_expropriated_2 < 13) # 19
sum(p$pop_2030[p$y_expropriated_2 < 13]) # 700M
sum(p$pop_2022[p$y_expropriated_2 < 13]) # 571M
sum(p$mean_Y < 6.85) # 8
sum(p$pop_2030[p$mean_Y < 6.85]) # 300M
sum(p$gdp_pc_2021 < 6.85*365, na.rm = T) # 22
sum(p$pop_2022[p$gdp_pc_2021 < 6.85*365], na.rm = T) # 521M
sort(setNames(p$gdp_pc_2030/365, p$country), decreasing = T)
decrit("antipoverty_2_tax_13")
decrit("antipoverty_2_tax_7")
decrit("antipoverty_2_tax_4")
decrit("antipoverty_2_tax_2")
decrit("antipoverty_4_tax_7")
decrit("antipoverty_4_tax_13")
decrit("antipoverty_7_tax_13")
decrit("antipoverty_13_tax_13")
decrit("y_expropriated_2")
decrit("y_expropriated_4")
decrit("y_expropriated_7")
decrit("y_expropriated_13")


# /!\ PROBLEM: huge discrepancies between PovcalNet and GDP pc data (this is because conso survey underestimates high-incomes and doesn't include investment), e.g. for MDG: 
p$mean_y_2022[p$country == "Madagascar"]
p$gdp_pc_2022[p$country == "Madagascar"]/365 # GDP pc from World Bank


#####  Maps ##### 
plot_world_map("antipoverty_2_tax_2", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $2.15/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_2_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_2_tax_13", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $13/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("percentile_expropriated_2", breaks = c(-Inf, 0, 50, 90, 95, 99, 100), sep = " to ", end = "", strict_ineq_lower = T, 
#                legend = "Percentile above which\nall should be expropriated\nto lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
#                save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("percentile_expropriated_7", breaks = c(-Inf, 0, 50, 90, 95, 99, 100), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
#                legend = "Percentile above which\nall should be expropriated\nto lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
#                save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("y_expropriated_2", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("y_expropriated_7", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $6.85/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("y_expropriated_13", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $13/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("poverty_gap_2", breaks = c(0, 2, 10, 20, 40, 60, 100), sep = " to ", end = "", strict_ineq_lower = FALSE, # svg, pdf 
               legend = "Poverty gap (in %)\n at $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .05, trim = T)  

plot_world_map("antipoverty_2_tax_2_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $2.15/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_2_tax_7_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_2_tax_13_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $13/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("y_expropriated_2_optimistic", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("y_expropriated_7_optimistic", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $6.85/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("y_expropriated_13_optimistic", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $13/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  


#Maps in French
plot_world_map("antipoverty_2_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), add_folder = 'fr_',
               # labels = sub("≤", "<", agg_thresholds(c(0), c(0, .1, 1, 5, 10, 25, 50, 100, Inf), sep = "to", return = "levels")),
               legend = "Taux de taxe linéaire \nau-dessus de $6.85/jour\nnécessaire pour éradiquer\nl'extrême pauvreté à $2.15/jour\n(en 2017 PPA)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T) 
plot_world_map("antipoverty_2_tax_2", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), add_folder = 'fr_',
               legend = "Taux de taxe linéaire \nau-dessus de $2.15/jour\nnécessaire pour éradiquer\nl'extrême pauvreté à $2.15/jour\n(en 2017 PPA)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T) 
plot_world_map("antipoverty_2_tax_13", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), add_folder = 'fr_',
               legend = "Taux de taxe linéaire\n au-dessus de $13/jour\nnécessaire pour éradiquer\nl'extrême pauvreté à $2.15/jour\n(en 2017 PPA)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T) 
# plot_world_map("percentile_expropriated_2", breaks = c(-Inf, 0, 50, 90, 95, 99, 100), sep = " to ", end = "", strict_ineq_lower = T, add_folder = 'fr_',
#                legend = "Percentile above which\nall should be expropriated\nto lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T, 
#                save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T) 
# plot_world_map("percentile_expropriated_7", breaks = c(-Inf, 0, 50, 90, 95, 99, 100), sep = " to ", end = "", strict_ineq_lower = T, add_folder = 'fr_', # svg, pdf
#                legend = "Percentile above which\nall should be expropriated\nto lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T, 
#                save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T) 
plot_world_map("y_expropriated_2", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, add_folder = 'fr_',
               legend = "Revenu journalier au\ndessus duquel tout devrait\nêtre exproprié pour élever \ntous les habitants du pays\nau-dessus de $2.15/jour\n(en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)
plot_world_map("y_expropriated_7", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T,  add_folder = 'fr_',# svg, pdf
               legend = "Revenu journalier au\ndessus duquel tout devrait\nêtre exproprié pour élever \ntous les habitants du pays\nau-dessus de $6.85/jour\n(en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)
plot_world_map("y_expropriated_13", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T,  add_folder = 'fr_',# svg, pdf
               legend = "Revenu journalier au\ndessus duquel tout devrait\nêtre exproprié pour élever \ntous les habitants du pays\nau-dessus de $13/jour\n(en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)
plot_world_map("poverty_gap_2", breaks = c(0, 2, 10, 20, 40, 60, 100), sep = " to ", end = "", strict_ineq_lower = FALSE,  add_folder = 'fr_',# svg, pdf
               legend = "Écart de pauvreté (en %)\nà $2.15/jour (en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .05, trim = T)
