# Docs:
# En, 2p https://docs.google.com/document/d/12_JPK2F3Kj2CmRdMpL8hSPBPK-tImSLQc84Ordzz3tM/edit
# Fr https://docs.google.com/document/d/1u41m1U0FGlvt6aGKzZET3MWr0ulz3tORPw1xVRSKek0/edit?usp=sharing

# TODO! 
# faire tourner figs après ajouter le scénario de croissance
# calculer Gini sans redistribution, et avec redistribution linéaire vs. expropriative => indicateur: réduction de Gini nécessaire pour éradiquer pauvreté. => done no? just use return = 'df' in antipoverty_tax/maximum
# demogrant for a given tax in MER (more important for the book than for the paper)

# There is an R package to access PIP: pipR
# Cite Hoy & Sumner (16) who do almost the same thing (and even include reallocation of fuel subsidies and excess military spending), take 50% marginal tax > 10$/d as maximum
# On estimates of poverty rate in 2030: Karver et al. (12): IMF proj - 1pp, Chandy et al. (13) EIU - 10%, Bicaba et al. (17) EIU - 10%, Lakner et al. (22); on future global income distrib, Hellebrandt & Mauro (15): IMF proj and one sensitivity analysis with 0.84+0.35*(past growth)
# Cite GCIP (Lahoti et al., 16), WIID (Gradín, 21) and WID as alternative sources
# Cite Lakner & Milanovic (16), Anand & Segal (15) on estimating global income distribution; and Pinkovskiy & Sala-i-Martin (09), Jordá & Niño-Zarazúa (19) on parametric estimates of global distribution.
# Cite Deaton (05) and Prydz et al. (22) on survey vs. tax data
# Other costing of extreme poverty eradication: UNCTAD (21, p. 15: growth needed), Vorisek & Yu (20, lite review), SDSN (19, excellent: talk about ODA, wealth & carbon taxes, estimate domestic resources, e.g. Table 4), Moyer & Hedden (20), Manuel et al. (18) (cost to end extreme poverty: 2.5 T$, incl. 150 G$ in countries lacking resources - vs. 200 G$ in ODA)
# Cite Ortiz et al. (18), computing the costs of an UBI at the national poverty line (Figure 2, 3). On Theil, cite Chancel & Piketty (2021). On cheap diets, cite https://sites.tufts.edu/foodpricesfornutrition/research-and-publications/
# On definitions of poverty, cite: Woodward & Abdallah (10) - no data but based on infant mortality rate, Pritchett (06) - 15$ based on HIC poverty lines, Edward (06) - 7$ based on kink in relation between GDP and life expectancy
# To justify not rescaling: cite Angrist et al. (21) that GDP poorly measures agricultural output, Martinez (22) that dictators inflate estimates; Deaton (2001) argues that in India approximately half of the gap between national accounts consumption and household surveys is due to imputed rents.

# compare Bolch with the same survey years as them: not replicated because the data has been revised. They use old data (2014) and old survey years (2009). Using most recent PIP data is surely preferable.
# World Bank (2022): "It became clear that the global goal of ending extreme poverty by 2030 would not be achieved."
# SDG: "2. We commit ourselves to working tirelessly for the full implementation of this Agenda by 2030. We recognize that eradicating poverty in all its forms and dimensions, including extreme poverty, is the greatest global challenge and an indispensable requirement for sustainable development. 
#          We are committed to achieving sustainable development in its three dimensions – economic, social and environmental – in a balanced and integrated manner. We will also build upon the achievements of the Millennium Development Goals and seek to address their unfinished business."  ttps://sdgs.un.org/2030agenda
#       "41. We recognize that each country has primary responsibility for its own economic and social development."
#       "60. We reaffirm our strong commitment to the full implementation of this new Agenda. We recognize that we will not be able to achieve our ambitious Goals and targets without a revitalized and enhanced Global Partnership and comparably ambitious means of implementation. 
#            The revitalized Global Partnership will facilitate an intensive global engagement in support of implementation of all the goals and targets, bringing together Governments, civil society, the private sector, the United Nations system and other actors and mobilizing all available resources."

# Past ideas:
# combine with tax data (WIL) to get better estimates of total GDP or use WIL data directly. => WID post-tax income data is only available in 38 countries (https://wid.world/summary-table/)
# Use GDP growth projections (EIU paid, OECD (or PwC) incomplete, consensuseconomics.com paid, IMF 2027), cf. Hellebrandt & Mauro (15) for different growth projections method
# check which has lowest HFCE p.c.: Burundi, < 2$ in 2022

##### Functions #####
name_var_growth <- function(growth = "optimistic") { 
  return(case_when(growth == "trend" ~ "y", # 2014-19 trend extended over 2022-30
                    growth == "trend_pos" ~ "y_pos", # max 2014-19 trend, 0
                    growth == "none" ~ "welfare", # original data (~2017, cf. p$year)
                    growth == "now" ~ "y_2022", # in 2022 (original data rescaled with observed GDP growth)
                    growth == "bolch" ~ "bolch", # replicates Bolch data (using their data years)
                    growth == "strong" ~ "Y4", # 4.5% growth over 2023-30
                    growth == "average" ~ "Y3", # 3% growth over 2023-30
                    growth == "optimistic" ~ "Y", # 6% growth over 2023-30
                    growth == "very_optimistic" ~ "Y7", # 7% growth over 2023-30
                    growth == "sdg8" ~ "y7", # 7% growth over 2015-30
                    growth == "reg" ~ "y_reg", # Forecasts future growth using a quadratic model of past growth (middle ground between our average (world growth) and (country) trend)
                    growth == "imf" ~ "y_imf", # Uses IMF forecasts (in the envelope of none - very_optimistic)
                    TRUE ~ growth))
} 
find_pop_share_var <- function(var, df) {
  if (paste0(var, "_pop_share_1") %in% names(df)) return(paste0(var, "_pop_share_"))
  else if (paste0(sub("_tax.*|_min.*", "", var), "_pop_share_1") %in% names(df)) return(paste0(sub("_tax.*|_min.*", "", var), "_pop_share_"))
  else return("pop_share_")
}
compute_poverty_rate <- function(df = p, threshold = 3.44, growth = "optimistic", return = "rate") {
  y <- name_var_growth(growth)
  name_poverty_rate <- paste0(y, "poverty_rate_", if (is.numeric(threshold)) round(threshold) else threshold)
  if (is.character(threshold)) threshold <- df[[threshold]]
  df[[name_poverty_rate]] <- 0
  for (i in 1:100) df[[name_poverty_rate]] <- df[[name_poverty_rate]] + (df[[paste0(y, "_avg_", i)]] < threshold) * df[[paste0(find_pop_share_var(y, df), i)]]
  if (return == "df") return(df)
  else return(df[[name_poverty_rate]])
}
# Average poverty gap. unit: 'mean' (in $/day/person), 'sum' (100 times 'mean'), '%' (in % of GDP), 'threshold' (in % of threshold), '$' (in $)
compute_poverty_gap <- function(df = p, threshold = 2.15, unit = "sum", growth = "optimistic") {
  y <- name_var_growth(growth)
  if (is.character(threshold)) threshold <- df[[threshold]] 
  poverty_gaps <- sapply(1:100, function(i) { pmax(0, threshold - df[[paste0(y, "_avg_", i)]]) })
  pg <- if (!is.vector(poverty_gaps)) rowSums(poverty_gaps * df[,paste0(find_pop_share_var(var = name_var_growth(growth), df), 1:100)]) else sum(poverty_gaps * df[,paste0(find_pop_share_var(var = name_var_growth(growth), df), 1:100)])
  if (unit == "sum") pg <- 100*pg
  if (unit %in% c("percent", "%")) pg <- pg/df[[paste0("mean_", y)]]
  if (unit %in% c("threshold", "% threshold")) pg <- pg/threshold
  if (unit %in% c("money", "dollar", "$")) pg <- pg * 365 * ((growth == "now") * df$pop_2022 + (growth != "now") * df$pop_2030)
  return(pg)
}
# Percentile above which we expropriate all y to fill the poverty gap TODO? rename: anti-poverty cap
compute_antipoverty_maximum <- function(df = p, threshold = 2.15, return = "y", growth = "optimistic") { 
  y <- name_var_growth(growth)
  thresholds <- if (is.numeric(threshold)) rep(threshold, nrow(df)) else p[[threshold]]
  poverty_gap <- compute_poverty_gap(df = df, threshold = threshold, unit = "mean", growth = growth)
  percentile_expropriated <- rep(100, nrow(df))
  y_expropriated <- rep(Inf, nrow(df))
  df[[paste0(find_pop_share_var(var = name_var_growth(growth), df), 101)]] <- 0
  for (c in 1:nrow(df)) {
    if (!is.na(poverty_gap[c])) {
      funded <- funded_old <- lower_percentile_effect <- lower_threshold_effect <- 0
      while (funded < poverty_gap[c] & percentile_expropriated[c] > 0) {
        y_expropriated[c] <- df[[paste0(y, "_min_", percentile_expropriated[c])]][c]
        # funded <- funded + df[[paste0(y, "_avg_", percentile_expropriated[c])]][c] - y_expropriated[c] + (100 - percentile_expropriated[c]) * (df[[paste0(y, "_max_", percentile_expropriated[c])]][c] - y_expropriated[c])
        funded_old <- funded
        lower_percentile_effect <- (df[[paste0(y, "_avg_", percentile_expropriated[c])]][c] - y_expropriated[c]) * df[c, paste0(find_pop_share_var(var = name_var_growth(growth), df), percentile_expropriated[c])]
        lower_threshold_effect <- (df[[paste0(y, "_max_", percentile_expropriated[c])]][c] - y_expropriated[c]) * sum(df[c,paste0(find_pop_share_var(var = name_var_growth(growth), df), (percentile_expropriated[c]+1):100)])
        funded <- funded + lower_percentile_effect + (percentile_expropriated[c] < 100) * lower_threshold_effect
        percentile_expropriated[c] <- percentile_expropriated[c] - 1 
        # By convention, if we cannot close the poverty gap in the country, we set percentile_expropriated to 0 and y_expropriated at gdp_pc_2030
        if (df[[paste0(y, "_min_", percentile_expropriated[c])]][c] < thresholds[c]) {
          percentile_expropriated[c] <- 0
          y_expropriated[c] <- df[[paste0("mean_", y)]][c] # df$gdp_pc_2030[c]/365
        }
      }
      # The following implicitly assumes linear distribution of income within a percentile. This overestimates the antipoverty-cap as in reality, average is above median. A more precise estimate would be a bracketing method (dichotomy) from antipoverty_tax.
      if (percentile_expropriated[c] == 99) y_expropriated[c] <- y_expropriated[c] + 2*(df[[paste0(y, "_avg_100")]][c] - y_expropriated[c]) * ((funded - poverty_gap[c])/(funded - funded_old))^2
      else if (funded > poverty_gap[c]) y_expropriated[c] <- y_expropriated[c] + (df[[paste0(y, "_max_", (percentile_expropriated[c]+1))]][c] - y_expropriated[c]) * (lower_threshold_effect / (lower_percentile_effect + lower_threshold_effect)) * (funded - poverty_gap[c])/(funded - funded_old)
    } else y_expropriated[c] <- NA
  }
  if (return == "percentile") return(percentile_expropriated)
  else if (return == "df") {
    name_var <- paste0(y, "_min_", if (is.numeric(threshold)) round(threshold) else threshold, "_cap")
    df[[name_var]] <- y_expropriated
    df[[paste0(name_var, "_percentile")]] <- percentile_expropriated
    for (i in 1:100) df[[paste0(name_var, "_avg_", i)]] <- pmax(threshold, pmin(y_expropriated, df[[paste0(y, "_avg_", i)]]))
    df[[paste0("mean_", name_var)]] <- rowSums(df[,paste0(name_var, "_avg_", 1:100)] * df[,paste0(find_pop_share_var(y, df), 1:100)])
    df <- compute_inequality(var = paste0(name_var, "_avg_"), df = df, return = "df")
    return(df)
  }
  else return(y_expropriated)
}
compute_antipoverty_tax <- function(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, return = "tax", growth = "optimistic") {
  y <- name_var_growth(growth)
  if (any(exemption_threshold > df[[paste0(y, "_max_100")]])) warning("exemption_threshold too high, this case is not properly")
  name_exemption <- if (is.numeric(exemption_threshold)) round(exemption_threshold) else exemption_threshold
  exemption_threshold <- if (is.numeric(exemption_threshold)) rep(exemption_threshold, nrow(df)) else p[[exemption_threshold]]
  taxable_base_all <- sapply(1:100, function(i) { pmax(0, df[[paste0(y, "_avg_", i)]] - exemption_threshold) }) # Implicitly assums step function with everyone within percentile at the percentile avg
  taxable_base <- 100 * if (!is.vector(taxable_base_all)) rowSums(taxable_base_all * df[,paste0(find_pop_share_var(var = name_var_growth(growth), df), 1:100)]) else sum(taxable_base_all * df[,paste0(find_pop_share_var(var = name_var_growth(growth), df), 1:100)])
  antipoverty_tax <- compute_poverty_gap(df = df, threshold = poverty_threshold, growth = growth) / pmax(1e-10, taxable_base)
  name_threshold <- if (is.numeric(poverty_threshold)) round(poverty_threshold) else poverty_threshold
  df[[paste0(y, "_antipoverty_", name_threshold, "_tax_", name_exemption)]] <- 100 * antipoverty_tax
  df[[paste0(y, "_bolch_index_min_", name_threshold, "_tax_", name_exemption)]] <- (antipoverty_tax <= 1) * (antipoverty_tax/2 + 1-antipoverty_tax) + (antipoverty_tax > 1) * (0.5/pmax(1e-8, antipoverty_tax))
  if (return == "base") return(taxable_base)
  else if (return == "df") {
    for (i in 1:100) df[[paste0(y, "_min_", name_threshold, "_tax_", name_exemption,  "_avg_", i)]] <- pmax(poverty_threshold, df[[paste0(y, "_avg_", i)]] - df[[paste0(y, "_antipoverty_", name_threshold, "_tax_", name_exemption)]] * pmax(0, df[[paste0(y, "_avg_", i)]] - exemption_threshold))
    df[[paste0("mean_", y, "_min_", name_threshold, "_tax_", name_exemption)]] <- rowSums(df[,paste0(y, "_min_", name_threshold, "_tax_", name_exemption,  "_avg_", 1:100)] * df[,paste0(find_pop_share_var(y, df), 1:100)])
    df <- compute_inequality(var = paste0(y, "_min_", name_threshold, "_tax_", name_exemption), df = df, return = "df")
    return(df)
  } else if (return %in% c("poverty_eradication_capacity", "PEC", "pec", "bolch_index", "bolch")) { return(df[[paste0(y, "_bolch_index_min_", name_threshold, "_tax_", name_exemption)]])
  } else return(df[[paste0(y, "_antipoverty_", name_threshold, "_tax_", name_exemption)]])
}
# Computes the demogrant that can be funded with given revenues (in $ per person) 
compute_min_funded <- function(revenues, var = name_var_growth("optimistic"), df = w, return = "min") {
  if (length(revenues) == 1) {
    cost <- (df[[paste0(var, "_avg_", 2)]] - df[[paste0(var, "_avg_", 1)]]) * df[[paste0(find_pop_share_var(var, df), 1)]]
    cumulated_pop <- df[[paste0(find_pop_share_var(var, df), 1)]]
    i <- 2
    while (cost < revenues) {
      cumulated_pop <- cumulated_pop + df[[paste0(find_pop_share_var(var, df), i)]]
      cost <- cost + (df[[paste0(var, "_avg_", i+1)]] - df[[paste0(var, "_avg_", i)]]) * cumulated_pop
      i <- i+1
    }
    demogrant <- df[[paste0(var, "_avg_", i)]] - (cost - revenues)/cumulated_pop # /!\ beware of this line
    for (j in 1:(i-1)) df[[paste0(var, "_avg_", j)]] <- demogrant
    if (return == "df") return(df)
    else if (return == "percentile") return(cumulated_pop)
    else return(demogrant)
  } else if (nrow(df) == 1) { return(compute_min_funded(revenues = revenues, var = var, df = df, return = "min")) # Unused. Beware, revenues must be in $/person
  } else return(sapply(1:length(revenues), function(i) compute_min_funded(revenues = revenues[i], var = var, df = df[i,], return = "min")))
}
# /!\ These sorts of computations are flawed because the poverty gaps should be computed in MER, not PPP
# TODO! conversion PPP -> MER: define a poverty line in PPP, compute the poverty gaps in MER country by country, aggregate them, find percentile of world MER distribution corresponding to world poverty gap, and associated MER poverty line.
tax_revenues <- function(thresholds, marginal_rates, name_tax = "custom", df = p, growth = "optimistic", return = '%', var = name_var_growth(growth), scope_tax = w) { 
  # thresholds (in $/day) and marginal_rates (in %) should be vectors of same length
  # scope_tax can be p or w depending on whether the revenues are recycled nationally or internationally
  revenues <- 0
  marginal_rates <- c(0, marginal_rates)
  for (i in 1:length(thresholds)) {
    taxable_base_all <- sapply(1:100, function(j) { pmax(0, df[[paste0(find_pop_share_var(var, df), j)]]*(df[[paste0(var, "_avg_", j)]] - thresholds[i])) })
    taxable_base_i <- if (!is.vector(taxable_base_all)) rowSums(taxable_base_all) else sum(taxable_base_all)
    revenues <- revenues + taxable_base_i * (marginal_rates[i+1] - marginal_rates[i])/100
  }
  pops <- ((growth == "now") * df$pop_2022 + (growth != "now") * df$pop_2030)
  if (return == 'pc') return(revenues)
  else if (return == 'total') return(revenues * 365 * pops)
  else if (return == "df") { 
    revenues <- if (nrow(scope_tax) == 1) sum(revenues * pops)/sum(pops) else revenues
    demogrant <- compute_min_funded(revenues, var = var, df = scope_tax, return = "min")
    for (i in 1:100) df[[paste0(var, "_tax_", name_tax,  "_avg_", i)]] <- pmax(demogrant, df[[paste0(var, "_avg_", i)]])
    for (i in 1:100) df[[paste0(var, "_tax_", name_tax,  "_max_", i)]] <- pmax(demogrant, df[[paste0(var, "_max_", i)]])
    for (j in 1:length(thresholds)) for (i in 1:100) df[[paste0(var, "_tax_", name_tax,  "_avg_", i)]] <- df[[paste0(var, "_tax_", name_tax,  "_avg_", i)]]  - ((marginal_rates[j+1] - marginal_rates[j])/100) * pmax(0, df[[paste0(var, "_avg_", i)]] - thresholds[j])
    for (j in 1:length(thresholds)) for (i in 1:100) df[[paste0(var, "_tax_", name_tax,  "_max_", i)]] <- df[[paste0(var, "_tax_", name_tax,  "_max_", i)]]  - ((marginal_rates[j+1] - marginal_rates[j])/100) * pmax(0, df[[paste0(var, "_max_", i)]] - thresholds[j])
    df[[paste0("mean_", var, "_tax_", name_tax)]] <- rowSums(df[,paste0(var, "_tax_", name_tax,  "_avg_", 1:100)] * df[,paste0(find_pop_share_var(var, df), 1:100)])
    if (nrow(df) > 1) {
      df[[paste0("gain_", var, "_tax_", name_tax)]] <- df[[paste0("mean_", var, "_tax_", name_tax)]]/df[[paste0("mean_", var)]] - 1
      international_transfer <- sum(pmax(0, df[[paste0("mean_", var, "_tax_", name_tax)]] - df[[paste0("mean_", var)]]) * pops, na.rm = T)/sum(df[[paste0("mean_", var)]] * pops, na.rm = T)
      print(paste("International transfer (relative to world income):", round(international_transfer, 4)))
    } else {
      df[[paste0("gain_", var, "_tax_", name_tax)]] <- rowSums(pmax(0, df[,paste0(var, "_tax_", name_tax,  "_avg_", 1:100)] - df[,paste0(var, "_avg_", 1:100)]) * df[,paste0(find_pop_share_var(var, df), 1:100)])
      print(paste("Inter-personal transfer (relative to world income):", round(df[[paste0("gain_", var, "_tax_", name_tax)]], 4)))
    }
    df <- compute_inequality(var = paste0(var, "_tax_", name_tax), df = df, return = "df")
    return(df)
  }
  else if (return %in% c("%", "% GDP")) return(revenues / df[[paste0("mean_", var)]])
}
compute_inequality <- function(var = name_var_growth("optimistic"), df = p, return = "df", recompute = FALSE) {
  d <- df
  d$y_cumulated_0 <- 0
  for (i in 1:100) d[[paste0("y_cumulated_", i)]] <- d[[paste0("y_cumulated_", i-1)]] + df[[paste0(find_pop_share_var(var, df), i)]] * df[[paste0(var, "_avg_", i)]] / df[[paste0("mean_", var)]]
  antigini <- sapply(1:100, function(i) { df[[paste0(find_pop_share_var(var, df), i)]] * (d[[paste0("y_cumulated_", i)]] + d[[paste0("y_cumulated_", i-1)]])}) # Brown formula
  if (recompute | !paste0(var, "_gini") %in% names(df)) df[[paste0(var, "_gini")]] <- 1 - if (!is.vector(antigini)) rowSums(antigini) else sum(antigini)
  if (recompute | !paste0(var, "_top1") %in% names(df)) df[[paste0(var, "_top1")]] <- 0.01*(df[[paste0(var, "_avg_100")]]/df[[paste0(find_pop_share_var(var, df), 100)]])/df[[paste0("mean_", var)]]
  if (nrow(df) == 1) {
    if (recompute | !paste0(var, "_top10") %in% names(df)) df[[paste0(var, "_top10")]] <- 0.1*(sum(sapply(91:100, function(i) { df[[paste0(var, "_avg_", i)]] }))/sum(sapply(91:100, function(i) { df[[paste0(find_pop_share_var(var, df), i)]] })))/df[[paste0("mean_", var)]]
    if (recompute | !paste0(var, "_bottom50") %in% names(df)) df[[paste0(var, "_bottom50")]] <- 0.5*(sum(sapply(1:50, function(i) { df[[paste0(var, "_avg_", i)]] }))/sum(sapply(1:50, function(i) { df[[paste0(find_pop_share_var(var, df), i)]] })))/df[[paste0("mean_", var)]]    
  } else {
    if (recompute | !paste0(var, "_top10") %in% names(df)) df[[paste0(var, "_top10")]] <- 0.1*(rowSums(sapply(91:100, function(i) { df[[paste0(var, "_avg_", i)]] }))/rowSums(sapply(91:100, function(i) { df[[paste0(find_pop_share_var(var, df), i)]] })))/df[[paste0("mean_", var)]]
    if (recompute | !paste0(var, "_bottom50") %in% names(df)) df[[paste0(var, "_bottom50")]] <- 0.5*(rowSums(sapply(1:50, function(i) { df[[paste0(var, "_avg_", i)]] }))/rowSums(sapply(1:50, function(i) { df[[paste0(find_pop_share_var(var, df), i)]] })))/df[[paste0("mean_", var)]]
  }
  if (recompute | !paste0(var, "_d9d1") %in% names(df)) df[[paste0(var, "_d9d1")]] <- df[[paste0(var, "_avg_90")]]/df[[paste0(var, "_avg_10")]] 
  if (recompute | !paste0(var, "_d9d5") %in% names(df)) df[[paste0(var, "_d9d5")]] <- df[[paste0(var, "_avg_90")]]/df[[paste0(var, "_avg_50")]]
  # if (!paste0(var, "_d9d1") %in% names(df)) df[[paste0(var, "_d9d1")]] <- df[[paste0(var, "_max_90")]]/df[[paste0(var, "_max_10")]] # df[[paste0(var, "_max_10")]] == 0 for SUR
  # if (!paste0(var, "_d9d5") %in% names(df)) df[[paste0(var, "_d9d5")]] <- df[[paste0(var, "_max_90")]]/df[[paste0(var, "_max_50")]] # Better in theory, but not implemented for after-tax distributions
  entropy <- sapply(1:100, function(i) { df[[paste0(find_pop_share_var(var, df), i)]] * log(pmax(1e-10, df[[paste0(var, "_avg_", i)]])) })
  if (recompute | !paste0(var, "_theil") %in% names(df)) df[[paste0(var, "_theil")]] <- log(df[[paste0("mean_", var)]]) - if (!is.vector(entropy)) rowSums(entropy) else sum(entropy) # I use Theil L. To normalize, use 1 - exp(-df[[paste0(var, "_theil")]])
  # if (nrow(df) > 1) df[[paste0(var, "_coef_theil")]] <- df[[paste0("pop_", if (grepl("2022|now", var)) "2022" else "2030")]] * df[[paste0("mean_", var)]] # / (pop_tot * mean_world)
  pop_yr <- paste0("pop_", if (grepl("2022|now", var)) "2022" else "2030")
  # if (nrow(df) > 1) theil_T_within <- sum(df[[paste0(var, "_theil")]] * (sum(df[[pop_yr]] * df[[paste0("mean_", var)]]) / (sum(df[[pop_yr]]) * sum(df[[pop_yr]] * df[[paste0("mean_", var)]]))))
  if (nrow(df) > 1) theil_within <- sum(df[[paste0(var, "_theil")]] * (df[[pop_yr]]/sum(df[[pop_yr]])))
  if (nrow(df) > 1) theil_between <- sum((df[[pop_yr]]/sum(df[[pop_yr]])) * log(df[[pop_yr]]/sum(df[[pop_yr]]) / (df[[pop_yr]]*df[[paste0("mean_", var)]] / sum(df[[pop_yr]] * df[[paste0("mean_", var)]]))))
  theil <- if (nrow(df) > 1) (theil_between + theil_within) else df[[paste0(var, "_theil")]]
  
  ineq <- df[, paste0(var, c("_top1", "_top10", "_bottom50", "_d9d1", "_d9d5", "_gini", "_theil"))]
  if (return == "df") {
    # if (nrow(df) == 1) print(ineq) 
    # else print(paste0(var," Theil-L: ", round(theil, 3), " [within: ", round(theil_within, 3), " (", round(100*theil_within/theil), "%) / between: ", round(theil_between, 3), " (", round(100*theil_between/theil), "%)]"))
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
  } else if (return %in% c("theil_total")) { return(theil) 
  } else if (return %in% c("theil_total")) { return(c(theil, theil_within/theil, theil_between/theil)) 
  } else if (return %in% c("table", "ineq", "summary")) { return(ineq) 
  } else warning("'return' unknown")
}

compute_distribution_2030 <- function(growth = "optimistic", growth_rate = NULL, name_var = NULL, df = p, pop_rurb = pop_rural_urban) {
  # if (is.null(growth_rate)) growth_rate <- if (growth == "very_optimistic") 1.07 else { if (growth == "strong") 1.04 else 1.06 }
  if (is.null(growth_rate)) growth_rate <- case_when(growth == "average" ~ 1.03,
                                                    growth == "very_optimistic" ~ 1.07,
                                                    growth == "strong" ~ 1.045,
                                                    TRUE ~ 1.06)
  y <- if (is.null(name_var)) name_var_growth(growth) else name_var
  if (grepl("trend", growth)) { 
    df$gdp_pc_2030 <- df$gdp_pc_2022 * (1 + df$mean_growth_gdp_pc_14_19)^8 
    df$growth_gdp_pc_year_30 <- df$gdp_pc_2030/df$gdp_pc_year
    # df$country_code[is.na(df$gdp_pc_year)] # "SSD" "SYR" "VEN" "YEM"
    # df$country_code[is.na(df$growth_gdp_pc_year_30)] # "SSD" "SYR" "TKM" "VEN" "YEM". TKM has grown 7.368 times in 98-22 and .994 in 14-19 https://www.imf.org/external/datamapper/PPPPC@WEO/SYR/VEN/YEM/SSD/TKM
    df$growth_gdp_pc_year_30[is.na(df$growth_gdp_pc_year_30)] <- 1 
    growths <- df$growth_gdp_pc_year_30
    if (growth == "trend_pos") growths <- pmax(1, df$growth_gdp_pc_year_30)
  } else if (growth == "sdg8") {
    df$gdp_pc_2030_sdg <- df$gdp_pc_2015 * (1.07^15) 
    df$growth_gdp_pc_year_30_sdg <- df$gdp_pc_2030_sdg/df$gdp_pc_year
    df$growth_gdp_pc_year_30_sdg[is.na(df$growth_gdp_pc_year_30_sdg)] <- 1.07^15
    growths <- df$growth_gdp_pc_year_30_sdg
  } else if (growth == "imf") {
    df$growth_gdp_pc_year_30_imf <- df$gdp_pc_2030_imf/df$gdp_pc_year
    # df$growth_gdp_pc_year_30_imf[is.na(df$growth_gdp_pc_year_30_imf)] <- mean(df$gdp_pc_2030_imf/df$gdp_pc_2022, na.rm = T)^(15/8)
    growths <- df$growth_gdp_pc_year_30_imf
  } else if (growth == "reg") {
    df$growth_gdp_pc_year_30_reg <- df$gdp_pc_2030_reg/df$gdp_pc_year
    # df$growth_gdp_pc_year_30_ref[is.na(df$growth_gdp_pc_year_30_reg)] <- mean(df$gdp_pc_2030_reg/df$gdp_pc_2022, na.rm = T)^(15/8)
    growths <- df$growth_gdp_pc_year_30_reg
  } else if (grepl("optimistic|strong|average", growth)) {
    df$gdp_pc_max_2030 <- df$gdp_pc_2022 * growth_rate^8 # 1.08^9 = 1.999, 1.07^9 = 1.84, 1.06^9 = 1.7, CN 99-07: 1.095^9 = 2.26. Beyond 6.3%, RDC antipoverty_2_tax_7 < 100%
    df$growth_gdp_pc_max_year_30 <- df$gdp_pc_max_2030/df$gdp_pc_year
    df$growth_gdp_pc_max_year_30[is.na(df$growth_gdp_pc_max_year_30)] <- growth_rate^(2030 - df$year[is.na(df$growth_gdp_pc_max_year_30)])
    growths <- df$growth_gdp_pc_max_year_30
  } else if (growth == "now") {
    df$growth_gdp_pc_2022 <- df$gdp_pc_2022/df$gdp_pc_year
    df$growth_gdp_pc_2022[is.na(df$growth_gdp_pc_2022)] <- 1 # Optimistic (in practice it's more .48-.58*), before: growth_rate^(2022 - df$year[is.na(df$growth_gdp_pc_2022)]) was even more optimistic. *Growth 2022/p$year according to IMF "South Sudan"-2016-0.568 "Syria"-2003-? "Venezuela"-2006-0.477 "Yemen"-2014-0.585 https://www.imf.org/external/datamapper/PPPPC@WEO/SYR/VEN/YEM/SSD
    growths <- df$growth_gdp_pc_2022
  } else if (growth %in% c("none", "bolch")) growths <- rep(1, nrow(df))
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
    cdf <- c() 
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
  for (q in wquantiles) wcdf <- c(wcdf, sum(sapply(1:100, function(j) { sum((df[[paste0(var, "_max_", j)]] <= q) * df[[paste0("pop_share_", j)]] * df[[pop_yr]], na.rm = T) })))
  wpop <- wcdf[length(wcdf)]
  wcdf <- wcdf/wpop
  wpercentiles <- findInterval(seq(0, 1, .01), wcdf)[-1] # computes the indices for which the pop_share is lesser or equal to the percentiles.
  # w <- data.frame("pop_2030" = wpop) # df[df$country_code == "USA", !names(p) %in% c("country", "country_code")]
  wdf[[paste0(var, "_", pop_yr)]] <- wpop
  wdf[[paste0(var, "_max_0")]] <- 0
  for (i in 1:100) {
    wdf[[paste0(var, "_max_", i)]] <- wquantiles[wpercentiles[i]]
    wdf[[paste0(var, "_min_", i)]] <- wdf[[paste0(var, "_max_", i-1)]]
    wdf[[paste0(var, "_pop_share_", i)]] <- sum(sapply(1:100, function(k) { sum(df[[pop_yr]] * df[[paste0("pop_share_", k)]] * (df[[paste0(var, "_avg_", k)]] <= wdf[[paste0(var, "_max_", i)]]) * (df[[paste0(var, "_avg_", k)]] > wdf[[paste0(var, "_max_", i-1)]]), na.rm = T) }))/sum(df[[pop_yr]])
    wdf[[paste0(var, "_avg_", i)]] <- if (wdf[[paste0(var, "_pop_share_", i)]] == 0) wdf[[paste0(var, "_avg_", i-1)]] else (sum(sapply(1:100, function(k) { sum(df[[pop_yr]] * df[[paste0("pop_share_", k)]] * df[[paste0(var, "_avg_", k)]] * (df[[paste0(var, "_avg_", k)]] <= wdf[[paste0(var, "_max_", i)]]) * (df[[paste0(var, "_avg_", k)]] > wdf[[paste0(var, "_max_", i-1)]]), na.rm = T) }))) / (wdf[[paste0(var, "_pop_share_", i)]]*sum(df[[pop_yr]]))
  }
  wdf[[paste0("mean_", var)]] <- rowSums(wdf[,paste0(var, "_avg_", 1:100)] * wdf[,paste0(var, "_pop_share_", 1:100)]) # mean(t(wdf[,grepl(paste0(var, "_avg"), names(wdf))]))

  wdf <- compute_inequality(var = var, df = wdf, return = "df")
  return(wdf)
}


##### Data #####
# PIP/PovcalNet data is *per capita* (without adjustment for household composition).
create_p <- function(ppp_year = 2017, pop_iso = pop_iso3, rescale = FALSE) { 
  data <- read.csv(paste0("../data/Povcalnet ", ppp_year, ".csv")) # https://datacatalogfiles.worldbank.org/ddh-published/0063646/DR0090251/world_100bin.csv?versionId=2023-05-31T15:19:01.1473846Z on https://datacatalog.worldbank.org/search/dataset/0063646
  
  # Data cleaning
  temp <- data %>% group_by(country_code) %>% dplyr::summarize(year_max = max(year))
  year_max <- setNames(temp$year_max, temp$country_code)
  data$year_max <- year_max[data$country_code]
  # data <- data[data$year == data$year_max,]
  p <- data[data$year == data$year_max,] %>% pivot_wider(names_from = percentile, values_from = c(avg_welfare, pop_share, welfare_share, quantile))
  names(p) <- sub("avg_welfare_", "welfare_avg_", names(p), fixed = T)
  p$mean_welfare <- rowSums(p[,paste0("welfare_avg_", 1:100)] * p[,paste0("pop_share_", 1:100)])
  
  # Add population
  p <- merge(p, pop_iso)
  p$pop_year <- sapply(1:nrow(p), function(c) { p[[paste0("pop_", p$year[c])]][c] }) # in thousands
  
  # HFCE
  temp <- read.xlsx("../data/HFCEpc.xlsx") # Household Final Consumption Expenditures https://data.worldbank.org/indicator/NE.CON.PRVT.PP.KD PPP 2017$ 31/01/2024
  p$hfce <- sapply(p$country_code, function(c) { temp[[as.character(unique(p$year[p$country_code == c]))]][temp$country_code == c] })
  p$hfce <- (p$hfce/p$pop_year)/365
  p$scaling_factor <- pmax(1, p$hfce/p$mean_welfare) # Rescale only if survey income < HFCE, as in Lakner & Milanovic (2013)
  p$scaling_factor[is.na(p$scaling_factor)] <- 1.1235 # impute mean((p$hfce/p$mean_welfare)[p$country_code %in% LIC], na.rm = T)
  if (rescale) p$welfare_avg_100 <- p$welfare_avg_100 + 100 * (p$scaling_factor - 1) * p$mean_welfare
  if (rescale) p$mean_welfare <- rowSums(p[,paste0("welfare_avg_", 1:100)] * p[,paste0("pop_share_", 1:100)])
  
  # Estimate GDP pc in 2022 assuming country growth same as 2014-19
  gdp_pc <- read_excel("../data/gdp_pc_ppp.xls") # Fetched in 2023 NY.GDP.PCAP.PP.KD
  colnames(gdp_pc)[-1] <- paste0("gdp_pc_", colnames(gdp_pc)[-1])
  p <- merge(p, gdp_pc)
  # p$gdp_pc_2019_over_2014 <- p$gdp_pc_2019/p$gdp_pc_2014
  p$gdp_pc_2022[is.na(p$gdp_pc_2022)] <- pmax(p$gdp_pc_2021, pmax(p$gdp_pc_2020, p$gdp_pc_2019, na.rm = T), na.rm = T)[is.na(p$gdp_pc_2022)]
  p$mean_growth_gdp_pc_14_19 <- pmax(0, (p$gdp_pc_2019/p$gdp_pc_2014)^(1/5)-1)
  # sort(setNames(p$mean_growth_gdp_pc_14_19, p$country), decreasing = T) # max 14-19: CN = 6.15%, max 10-19:  CN 6.7%
  p$gdp_pc_year <- sapply(1:nrow(p), function(c) { p[[paste0("gdp_pc_", min(2022, p$year[c]))]][c] }) 
  
  # Project GDP pc in 2030 using IMF forecast
  imf <- read.xlsx("../data/IMF_WEO_2023.xlsx") # WEO (2023) https://www.imf.org/en/Publications/WEO/weo-database/2023/October NGDPRPPPPC
  imf <- imf[imf$WEO.Subject.Code == "NGDPRPPPPC",] # GDP pc PPP constant $
  imf$growth_2022_2028 <- as.numeric(imf$`2028`)/as.numeric(imf$`2022`)
  imf$growth_2026_2028 <- as.numeric(imf$`2028`)/as.numeric(imf$`2026`)
  imf$growth_2022_2030 <- imf$growth_2026_2028 * imf$growth_2022_2028 # Guyana outlier, is projected to grow by a factor of 10 between 2018 and 2028
  p$gdp_pc_2030_imf <- sapply(p$country_code, function(c) { imf$growth_2022_2030[imf$country_code == c] * unique(p$gdp_pc_2022[p$country_code == c]) })

  # Project GDP pc in 2030 using quadratic model of past growth
  # summary(lm(I(gdp_pc_2019/gdp_pc_2011) ~ I(gdp_pc_2011/gdp_pc_1991) + I((gdp_pc_2011/gdp_pc_1991)^2), data = gdp_pc)) # Adj-R²: .27, the best from the models I have tested (linear or quadratic with different time periods). 0.9507844 + 0.1470747x - 0.0046657x²
  p$gdp_pc_2030_reg <- (0.9507844 + 0.1470747 * (p$gdp_pc_2022/p$gdp_pc_2002) - 0.0046657 * (p$gdp_pc_2022/p$gdp_pc_2002)^2) * p$gdp_pc_2022
  # plot(p$gdp_pc_2030_imf/p$gdp_pc_2022, p$gdp_pc_2030_reg/p$gdp_pc_2022, xlim = c(0.8, 1.7), ylim = c(0.8, 1.7))
  
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
  countries_names <- setNames(p$country, p$country_code) # /!\  Poland appears twice
  
  # Merge with original Bolch et al. (2022) results  
  bolch_table_original <- read.csv("../data/bolch_table_original.csv") # Table imported from Bolch et al. (2022) PDF using tabula.technology
  p <- merge(p, bolch_table_original, all.x = T)
  # Keep data from Bolch's survey years to replicate it
  year_bolch <- setNames(p$bolch_year, p$country_code) # sum(!is.na(year_bolch)) # 123
  data$year_bolch <- year_bolch[data$country_code]
  for (c in unique(data$country_code)) {
    rows_to_change <- data$country_code == c & data$year_bolch %in% (unique(data$year[data$country_code == c])+1) & !data$year_bolch %in% unique(data$year[data$country_code == c])
    data$year_bolch[rows_to_change] <- data$year_bolch[rows_to_change] - 1 }
  temp <- data[data$year == data$year_bolch,]
  temp <- temp %>% pivot_wider(names_from = percentile, values_from = c(avg_welfare, pop_share, welfare_share, quantile), values_fn = mean)
  temp <- temp[!is.na(temp$country_code),!grepl("_NA|year_max|welfare_type|year$", names(temp))]
  # temp <- temp[,!names(temp) %in% c("year", "year_max")] # If we put it again, change 7 to 5 in 7:ncol...
  names(temp) <- sub("avg_welfare_", "avg_", names(temp), fixed = T)
  names(temp) <- sub("quantile_", "max_", names(temp), fixed = T)
  temp$min_1 <- temp$max_0 <- 0
  for (i in 2:100) temp[[paste0("min_", i)]] <- temp[[paste0("max_", i-1)]]
  names(temp)[4:ncol(temp)] <- paste0("bolch_", names(temp)[4:ncol(temp)])
  temp$mean_bolch <- rowSums(temp[,paste0("bolch_avg_", 1:100)] * temp[,paste0("bolch_pop_share_", 1:100)], na.rm = T)
  p <- merge(p, temp, all.x = T, by = c("country_code", "reporting_level")) #, by = c("country_code", "reporting_level"))
  
  # Add Moatsos' basic need poverty lines (Moatsos, 2021) and Bare bones basket with Consumption Shares (Moatsos, 2016)
  conversion_17_11 <- if (ppp_year == 2017) 2.15/1.9 else 1
  bnpl <- read.xlsx("../data/Moatsos2021_BNPL.xlsx", rowNames = T) # /!\ This is in 2011$ and cannot be converted to 2017$ simply by applying a conversion factor (as we do below with 2.15/1.9)
  for (c in p$country_code) if (c %in% colnames(bnpl)) p$bnpl[p$country_code == c] <- conversion_17_11 * bnpl[as.character(unique(p$year[p$country_code == c])), c] 
  
  bcs <- read.xlsx("../data/Moatsos2016_BCS.xlsx", rowNames = T) # For some countries (like India), CPIs were missing; while others (like DRC) are simply absent from Moatsos' data 
  ppp11 <- read.xlsx("../data/LCU_per_PPP11.xlsx", rowNames = T) # World Bank (10/12/23) PPP conversion factor (LCU per int'l $, 2011) PA.NUS.PPP https://databank.worldbank.org/reports.aspx?source=2&series=PA.NUS.PPP&country#
  for (c in p$country_code) if (sum(bcs$iso3 == c & !is.na(bcs$BBPlus)) > 1) { # /!\ For the many countries for which PIP survey is more recent than BCS estimates, we use the last year for which BCS is estimated.
    c_year <- paste0(c, min(p$year[p$country_code == c], max(bcs$year[bcs$iso3 == c & !is.na(bcs$BBPlus)])))
    # if (is.na(bcs[paste0(c, 2011), "CPI"]/bcs[c_year, "CPI"])) print(paste("CPI", c)) # CHN, IND, COM Comores, PNG Papua, TON. Have added manually CPI in the .xlsx for them (except COM: no data) from World Bank (12/10/2023) FP.CPI.TOTL Consumer Price Index.
    # if (is.na(ppp11[c,"PPP11"])) print(paste0("PPP", c)) # none
    p$bcs[p$country_code == c] <- conversion_17_11 * bcs[c_year, "BBPlus"] * (bcs[paste0(c, 2011), "CPI"]/bcs[c_year, "CPI"]) / ppp11[c,"PPP11"] / 365
  }

  # y makes the assumption of constant growth while Y assumes 6% growth after 2022
  # p <- df # To run compute_distribution_2030, this line is needed to avoid bug (Indeed, urban/rural have been removed otherwise).
  # p <- compute_inequality(var = "bolch", df = p, return = "df")
  p <- compute_distribution_2030(growth = "bolch", df = p)
  p <- compute_distribution_2030(growth = "reg", df = p)
  p <- compute_distribution_2030(growth = "imf", df = p)
  p <- compute_distribution_2030(growth = "optimistic", df = p, growth_rate = 1.06)
  p <- compute_distribution_2030(growth = "very_optimistic", df = p, growth_rate = 1.07, name_var = "Y7")
  p <- compute_distribution_2030(growth = "trend", df = p)
  p <- compute_distribution_2030(growth = "trend_pos", df = p) # TODO! compute average trend/trend_pos growth
  p <- compute_distribution_2030(growth = "strong", df = p, growth_rate = 1.045, name_var = "Y4")
  p <- compute_distribution_2030(growth = "average", df = p, growth_rate = 1.03, name_var = "Y3")
  p <- compute_distribution_2030(growth = "sdg8", df = p) # The SDG 8.1: sustained 7% growth starting over 2016-30.
  p <- compute_distribution_2030(growth = "none", df = p)
  p <- compute_distribution_2030(growth = "now", df = p)
  # df <- p

  p <- p[(!p$country_code %in% c("CHN", "IDN", "IND")) | p$reporting_level == "national",]
  
  return(p)
}

create_world_distribution <- function(df = p17) {
  w <- data.frame(country = "World", pop_2022 = sum(df$pop_2022), pop_2030 = sum(df$pop_2030))
  w <- compute_world_distribution(name_var_growth("imf"), df = df, wdf = w)
  w <- compute_world_distribution(name_var_growth("reg"), df = df, wdf = w)
  w <- compute_world_distribution(name_var_growth("optimistic"), df = df, wdf = w)  
  w <- compute_world_distribution("Y7", df = df, wdf = w)  # ~ 1.5 min
  w <- compute_world_distribution(name_var_growth("trend"), df = df, wdf = w)
  w <- compute_world_distribution(name_var_growth("trend_pos"), df = df, wdf = w)
  w <- compute_world_distribution("Y4", df = df, wdf = w)
  w <- compute_world_distribution("Y3", df = df, wdf = w)
  w <- compute_world_distribution(name_var_growth("sdg8"), df = df, wdf = w)
  w <- compute_world_distribution(name_var_growth("none"), df = df, wdf = w)
  w <- compute_world_distribution(name_var_growth("now"), df = df, wdf = w)
  return(w)
}

# Load population
pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
pop$PopTotal <- 1e3 * pop$PopTotal
pop <- pop[pop$Time %in% c(sort(unique(p$year)), 2030),]
pop_iso3 <- aggregate(PopTotal ~ Time + ISO3_code, data = pop, FUN = sum)
pop_iso3 <- pop_iso3 %>% pivot_wider(names_from = Time, values_from = PopTotal)
names(pop_iso3) <- c("country_code", paste0("pop_", names(pop_iso3)[-1]))
rm(pop)

pop_rural_urban <- read.csv2("../data/pop_rural_urban.csv") # Last updated 07/05/2023 https://databank.worldbank.org/source/population-estimates-and-projections/preview/on#

start <- Sys.time()
p <- p17 <- create_p()
s <- create_p(rescale = T)
p11 <- create_p(ppp_year = 2011)
w <- create_world_distribution()
ws <- create_world_distribution(df = s)
# w11 <- create_world_distribution(df = p11) # 9 min
print(Sys.time() - start) # 20 min
beep()
save.image(".RData")

# wr <- w
# r <- p
# p <- s
# w <- ws
# 
# p <- r
# w <- wr

##### Book #####
p$mean_y_2022[p$country == "Democratic Republic of the Congo"] + 44/0.4/(365/12) # 0.4 is the PPP conversion factor in 2022 https://databank.worldbank.org/source/world-development-indicators/Series/PA.NUS.PPPC.RF
(p$mean_y_2022[p$country == "Democratic Republic of the Congo"] + 44/0.4/(365/12))/p$mean_y_2022[p$country == "Democratic Republic of the Congo"] # x2.4
p$mean_y_2022[p$country == "Sri Lanka"] # 8.3
(compute_poverty_rate(df = w, threshold = 7.5, growth = "trend_pos")) # 39% of 8.33G i.e. 3.25G sum(p$pop_2030). GDP2030 = 100*1.03^7= 123T
(compute_poverty_rate(df = w, threshold = 7.5, growth = "strong")) # 33% 
(compute_poverty_rate(df = w, threshold = 7.5, growth = "now")) # 48% => 36% in terms of posttax income (Gethin 23, p. 77) / 29% => 24% using WID data
(compute_poverty_rate(df = w, threshold = 10, growth = "now")) # 57%
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 7.5, unit = '%', growth = "trend_pos")) # 5.16% GDP
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 7.5, unit = '%', growth = "strong")) # 4.1% GDP / 3% growth: 5.5% PG, 3.5: 5, 4: 4.55, 4.5: 4, 5: 0.37
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 7.5, unit = '$', growth = "strong")) # 3.3T$ = 2.2% of 150T$ i.e. GDP in 2030 with 4.5% growth
1420*4# (w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 7.5, unit = '$', growth = "optimistic")) # 2.7T$
# (w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 7.5, unit = '$', growth = "trend_pos")) # 3.6T$
# (w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 7.5, unit = '$', growth = "none")) # 11% / 5.8T$ (6.85$: 4.8T)
sum(p$pop_2022[p$mean_y_2022 < 7.5]) # 3.2G
sum(p$pop_2022[p$gdp_pc_2021 < 7.5*365], na.rm = T) # 600M
w$mean_y_pos # 23
w$y_pos_avg_50 # 10
(tax_revenues(df = w, thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(2, 6, 15, 30, 50), return = '%', growth = "average")) # 3.6% of world GDP
(tax_revenues(df = w, thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(2, 6, 15, 30, 50), return = '%', growth = "trend_pos")) # 3% of world GDP
w <- tax_revenues(df = w, name_tax = "min8", thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(1, 4, 8, 20, 30), return = 'df', growth = "optimistic")
w <- tax_revenues(df = w, name_tax = 'drastic', thresholds = c(1, 2, 3, 4, 5)*1e3/(365/12), marginal_rates = c(10, 20, 30, 40, 90), return = 'df', growth = "optimistic")


##### Computations #####
# p <- compute_inequality(var = name_var_growth("optimistic"), df = p, return = "df")
# w <- compute_inequality(var = name_var_growth("optimistic"), df = w, return = "df") 
(w$poverty_gap_2 <- compute_poverty_gap(df = w, threshold = 2.15, unit = 'threshold', growth = "now")) # 2.2% estimated in 2022 vs. official 2019: 2.6% (higher because 2019 vs. 2022?) official: https://data.worldbank.org/indicator/SI.POV.GAPS
(w$poverty_gap_4 <- compute_poverty_gap(df = w, threshold = 3.65, unit = 'threshold', growth = "now")) # 7.1% vs. official: 8%
(w$poverty_gap_7 <- compute_poverty_gap(df = w, threshold = 6.85, unit = 'threshold', growth = "now")) # 19.8% vs. official: 21%
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 8.22, unit = '%', growth = "optimistic")) # 3.6% of world GDP to reach 250$/month
(tax_revenues(df = w, thresholds = c(33, 66, 100, 200, 300), marginal_rates = c(1, 4, 8, 20, 30), return = '%', growth = "optimistic")) # 1.18% of world GDP
compute_min_funded(revenues = tax_revenues(df = w, thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(1, 4, 8, 20, 30), return = 'pc', growth = "optimistic"))
w <- tax_revenues(df = w, name_tax = "min8", thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(1, 4, 8, 20, 30), return = 'df', growth = "optimistic")
df <- tax_revenues(df = p, scope_tax = w, name_tax = "min8", thresholds = c(1, 2, 3, 6, 9)*1e3/(365/12), marginal_rates = c(1, 4, 8, 20, 30), return = 'df', growth = "optimistic")
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 8.22, unit = '$', growth = "optimistic")) # 3.5T$ to reach 250$/month
(w$poverty_gap_8 <- compute_poverty_gap(df = w, threshold = 8.22, unit = 'threshold', growth = "optimistic")) # 14%: 1.15 $/day/person to reach 250$/month
(w$poverty_gap_35 <- compute_poverty_gap(df = w, threshold = 35, unit = 'mean', growth = "optimistic")) # 9 $/day/person to reach world average
(w$antipoverty_8_tax_66 <- compute_antipoverty_tax(df = w, exemption_threshold = 66.67, poverty_threshold = 8.22, growth = "optimistic")) # 2% tax above 2000$/month to finance 250$/month to everyone
(w$antipoverty_8_tax_100 <- compute_antipoverty_tax(df = w, exemption_threshold = 100, poverty_threshold = 8.22, growth = "optimistic")) # 4.2
(w$antipoverty_10_tax_100 <- compute_antipoverty_tax(df = w, exemption_threshold = 100, poverty_threshold = 10, growth = "optimistic")) # 8.66
(w$y_expropriated_9 <- compute_antipoverty_maximum(df = w, threshold = 9, growth = "optimistic")) # 470
(w$antipoverty_7_tax_100 <- compute_antipoverty_tax(df = w, exemption_threshold = 100, poverty_threshold = 7.5, growth = "strong")) # 22%
(w$y_expropriated_7 <- compute_antipoverty_maximum(df = w, threshold = 7.5, growth = "strong")) # 7.7k$
compute_min_funded(revenues = tax_revenues(df = w, thresholds = c(3, 6, 9)*1e3/(365/12), marginal_rates = c(10, 30, 80), return = 'pc', growth = "strong")) # 7.48

# comparer bolch_index_1/2 avec # TODO! debug
p$bolch_poverty_rate_3 <- compute_poverty_rate(df = p, threshold = 3.44, growth = "bolch", return = "rate")
p$poverty_rate_3 <- compute_poverty_rate(df = p, threshold = 3.44, growth = "now", return = "rate")
p$bolch_index_1_original <- compute_antipoverty_tax(df = p, exemption_threshold = 3.44, poverty_threshold = 3.44, growth = "bolch", return = "bolch") 
p$bolch_index_2_original <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3.44, growth = "bolch", return = "bolch")
p$bolch_index_1_now <- compute_antipoverty_tax(df = p, exemption_threshold = 3.44, poverty_threshold = 3.44, growth = "now", return = "bolch") 
p$bolch_index_2_now <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3.44, growth = "now", return = "bolch")
(bolch_index_1 <- round(sort(setNames(p$bolch_index_1_now, p$country), decreasing = T), 2))
(bolch_index_2 <- round(sort(setNames(p$bolch_index_2_now, p$country), decreasing = T), 2))

mean_gap(p$bolch_poverty_rate_3, p$bolch_poverty_rate_original) # 50%
mean_gap(p$bolch_index_1_now, p$bolch_pec_1) # 21%
mean_gap(p$bolch_index_2_now, p$bolch_pec_2) # 65% mean(c(50, 21, 65)) # 45
table_bolch_replication <- cbind("year" = p$year_bolch, "poverty_rate_replicated" = p$bolch_poverty_rate_3, "poverty_rate_original" = p$bolch_poverty_rate_original, 
                                 "bolch_1_replicated" = p$bolch_index_1_now, "bolch_1_original" = p$bolch_pec_1, "bolch_2_replicated" = p$bolch_index_2_now, "bolch_2_original" = p$bolch_pec_2)
row.names(table_bolch_replication) <- p$country
(table_bolch_replication <- table_bolch_replication[!is.na(table_bolch_replication[,5]) & p$pop_2022 > 20e6,]) # "Indicators from Bolch et al. (2022) replicated"
# Why two Poland? Because it has both a consumption and income reporting_level. TODO? remove one?
# Why some NA in year_bolch (because years don't coincide, perhaps because Dykstra recodes 2008.5 into 2009 and me 2008?) and bolch_poverty_rate_3?
cat(paste(kbl(table_bolch_replication, "latex", caption = "Indicators from Bolch et al. (2022) replicated", position = "b", escape = F, booktabs = T, digits = c(0, 2, 2, 2, 2, 2, 2), linesep = rep("", nrow(table_bolch_replication)-1), longtable = T, label = "bolch_replication",
              col.names = c("\\makecell{Survey\\\\year}", "\\makecell{Poverty rate\\\\at $\\$_{05PPP}$2}/day\\\\replicated", "\\makecell{Poverty rate\\\\at $\\$_{05PPP}$2}/day\\\\original", "\\makecell{Poverty Eradication Capacity\\\\Scenario 1 (tax above \\$2/day)\\\\replicated}", 
                            "\\makecell{Poverty Eradication Capacity\\\\Scenario 1 (tax above \\$2/day)\\\\original}", "\\makecell{Poverty Eradication Capacity\\\\Scenario 1 (tax above \\$18/day)\\\\replicated}", "\\makecell{Poverty Eradication Capacity\\\\Scenario 1 (tax above \\$18/day)\\\\original}")), collapse="\n"), file = "../tables/bolch_replication.tex") 
# The discrepancy comes from the difference in datasets. The ratio between mean_bolch (from my recent data) and Dykstra mean (in SummaryStatistics.csv) varies from 1.2 (Argentina) to 2.1 (Burundi). 
# It's not only due to 05PPP - 17PPP conversion varying across countries, but also *within* countries (or the data has been revised). The best example is to compare Latvia 1996 between Dykstra/povcal_abbrev.csv. and Povcalnet 2017.csv (as for this country, the pop_share are all equal), the distributions are different (not just rescaled):
# 30*c(1.7372003194365129, 3.9919969942092446, 6.16212692894546, 8.731745353766852, 11.794384103141129, 15.963861157869772, 19.86166234416087, 23.30031010587956, 26.384911935703354, 32.26016078712433, 67.48526045940788)/c(42.28, 90.65, 138.04, 194.88, 263.62, 359.96, 455.07, 545.36, 633.56, 829.43, 9507.42) # for percentiles 1, 25, 50, 75, 95, 97:100

# Update of Bolch et al. (2022) fraction of poverty gap eliminable by raising taxes above threshold / tax rate needed (2$ in S1, 13$ in S2). What are 2/13 05$ in 17$? Based on 2.15/1.25: 3.44. While U.S. poverty line a family of 4 (13$ in 05PPP) is 18.15 (26500/4/365) in 2021 https://aspe.hhs.gov/2021-poverty-guidelines
(table_bolch_update <- cbind("year" = p$year, "poverty_rate" = p$poverty_rate_3, "bolch_1" = p$bolch_index_1_now, "bolch_2" = p$bolch_index_2_now))
row.names(table_bolch_update) <- p$country
cat(paste(kbl(table_bolch_update[p$pop_2022 > 20e6,], "latex", caption = "Indicators from Bolch et al. (2022) with updated data", position = "b", escape = F, booktabs = T, digits = c(0, 2, 2, 2), linesep = rep("", nrow(table_bolch_update)-1), longtable = T, label = "bolch_update",
              col.names = c("\\makecell{Survey\\\\year}", "\\makecell{Poverty rate\\\\at $\\$_{05PPP}$2}/day", "\\makecell{Poverty Eradication Capacity\\\\Scenario 1 (tax above \\$2/day)}", "\\makecell{Poverty Eradication Capacity\\\\Scenario 1 (tax above \\$18/day)}")), collapse="\n"), file = "../tables/bolch_update.tex") 


plot(0:100, t(w[,c("y_avg_1", paste0("y_max_", 1:99), "y_avg_100")]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:98, t(w[,c("y_avg_1", paste0("y_max_", 1:98))]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:100, t(w[,c("Y7_avg_1", paste0("Y7_max_", 1:99), "Y10_avg_100")]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:98, t(w[,c("Y7_avg_1", paste0("Y7_max_", 1:98))]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:100, t(w[,c("welfare_avg_1", paste0("welfare_max_", 1:99), "welfare_avg_100")]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
plot(0:98, t(w[,c("welfare_avg_1", paste0("welfare_max_", 1:98))]), type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()

plot(0:100, t(w[,c("Y_avg_1", paste0("Y_avg_", 1:99), "Y_avg_100")]), col = 'red', type = "l", lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
lines(0:100, t(w[,c("Y_tax_min8_avg_1", paste0("Y_tax_min8_avg_", 1:100))]), type = "l", col = 'darkgreen', lwd = 2) 
plot(0:99, t(w[,c("Y_avg_1", paste0("Y_avg_", 1:99))]), type = "l", col = 'red', lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
lines(0:99, t(w[,c("Y_tax_min8_avg_1", paste0("Y_tax_min8_avg_", 1:99))]), type = "l", col = 'darkgreen', lwd = 2) 
plot(80:99, t(w[,c(paste0("Y_avg_", 80:99))]), type = "l", col = 'red', lwd = 2, xlab = "Percentile of world income distribution", ylab = "Income (in $/day)") + grid()
lines(80:99, t(w[,c(paste0("Y_tax_min8_avg_", 80:99))]), type = "l", col = 'darkgreen', lwd = 2) 
sort(setNames(compute_gini(df = p, var = "Y", return = "gini"), p$country))


# Poverty gap
p$poverty_gap_2 <- compute_poverty_gap(growth = "trend")
p$poverty_gap_4 <- compute_poverty_gap(threshold = 3.65, growth = "trend")
p$poverty_gap_7 <- compute_poverty_gap(threshold = 6.85, growth = "trend")

# Percentile expropriated
p$percentile_expropriated_2 <- compute_antipoverty_maximum(df = p, threshold = 2.15, return = "percentile", growth = "trend")
p$percentile_expropriated_4 <- compute_antipoverty_maximum(df = p, threshold = 3.65, return = "percentile", growth = "trend")
p$percentile_expropriated_7 <- compute_antipoverty_maximum(df = p, threshold = 6.85, return = "percentile", growth = "trend")
p$percentile_expropriated_18 <- compute_antipoverty_maximum(df = p, threshold = 18.15, return = "percentile", growth = "trend")

p$y_expropriated_2 <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "trend")
p$y_expropriated_4 <- compute_antipoverty_maximum(df = p, threshold = 3.65, growth = "trend")
p$y_expropriated_7 <- compute_antipoverty_maximum(df = p, threshold = 6.85, growth = "trend")
p$y_expropriated_18 <- compute_antipoverty_maximum(df = p, threshold = 18.15, growth = "trend")

p$percentile_expropriated_2_optimistic <- compute_antipoverty_maximum(df = p, threshold = 2.15, return = "percentile", growth = "optimistic")
p$percentile_expropriated_4_optimistic <- compute_antipoverty_maximum(df = p, threshold = 3.65, return = "percentile", growth = "optimistic")
p$percentile_expropriated_7_optimistic <- compute_antipoverty_maximum(df = p, threshold = 6.85, return = "percentile", growth = "optimistic")
p$percentile_expropriated_18_optimistic <- compute_antipoverty_maximum(df = p, threshold = 18.15, return = "percentile", growth = "optimistic")

p$y_expropriated_2_optimistic <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "optimistic")
p$y_expropriated_4_optimistic <- compute_antipoverty_maximum(df = p, threshold = 3.65, growth = "optimistic")
p$y_expropriated_7_optimistic <- compute_antipoverty_maximum(df = p, threshold = 6.85, growth = "optimistic")
p$y_expropriated_18_optimistic <- compute_antipoverty_maximum(df = p, threshold = 18.15, growth = "optimistic")

# Antipoverty tax
p$antipoverty_2_tax_18 <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "trend")
p$antipoverty_2_tax_7 <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "trend")
p$antipoverty_2_tax_4 <- compute_antipoverty_tax(df = p, exemption_threshold = 3.65, poverty_threshold = 2.15, growth = "trend")
p$antipoverty_2_tax_2 <- compute_antipoverty_tax(df = p, exemption_threshold = 2.15, poverty_threshold = 2.15, growth = "trend")
p$antipoverty_bnpl_tax_bnpl <- compute_antipoverty_tax(df = p11, exemption_threshold = "bnpl", poverty_threshold = "bnpl", growth = "trend")
p$antipoverty_bnpl_tax_7 <- compute_antipoverty_tax(df = p11, exemption_threshold = 5.5, poverty_threshold = "bnpl", growth = "trend") # In 2011PPP, 2.15/3.65/6.85 is 1.9/3.2/5.5 https://documents1.worldbank.org/curated/en/099700509122212929/pdf/IDU05b43a261041c504a5f0bb3405d0ef310b9e1.pdf
p$antipoverty_bnpl_tax_18 <- compute_antipoverty_tax(df = p11, exemption_threshold = 15.31, poverty_threshold = "bnpl", growth = "trend") # 2011 Poverty line in the U.S. for a family of 4 is 15.31=22350/365/4 https://aspe.hhs.gov/2011-hhs-poverty-guidelines
p$antipoverty_bcs_tax_bcs <- compute_antipoverty_tax(df = p11, exemption_threshold = "bcs", poverty_threshold = "bcs", growth = "trend")
p$antipoverty_bcs_tax_7 <- compute_antipoverty_tax(df = p11, exemption_threshold = 5.5, poverty_threshold = "bcs", growth = "trend") # In 2011PPP, 2.15/3.65/6.85 is 1.9/3.2/5.5 https://documents1.worldbank.org/curated/en/099700509122212929/pdf/IDU05b43a261041c504a5f0bb3405d0ef310b9e1.pdf
p$antipoverty_bcs_tax_18 <- compute_antipoverty_tax(df = p11, exemption_threshold = 15.31, poverty_threshold = "bcs", growth = "trend") # 2011 Poverty line in the U.S. for a family of 4 is 15.31=22350/365/4 https://aspe.hhs.gov/2011-hhs-poverty-guidelines
p$antipoverty_4_tax_7 <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 3.65, growth = "trend")
p$antipoverty_4_tax_18 <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3.65, growth = "trend")
p$antipoverty_7_tax_18 <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 6.85, growth = "trend")
p$antipoverty_18_tax_18 <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 18.15, growth = "trend")

p$antipoverty_2_tax_18_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_7_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_4_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 3.65, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_2_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 2.15, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_4_tax_7_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 3.65, growth = "optimistic")
p$antipoverty_4_tax_18_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3.65, growth = "optimistic")
p$antipoverty_7_tax_18_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 6.85, growth = "optimistic")
p$antipoverty_18_tax_18_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 18.15, growth = "optimistic")

# Poverty gap
p$s_poverty_gap_2 <- compute_poverty_gap(growth = "trend", df = s)
p$s_poverty_gap_4 <- compute_poverty_gap(threshold = 3.65, growth = "trend", df = s)
p$s_poverty_gap_7 <- compute_poverty_gap(threshold = 6.85, growth = "trend", df = s)

# Percentile expropriated
p$s_percentile_expropriated_2 <- compute_antipoverty_maximum(df = s, threshold = 2.15, return = "percentile", growth = "trend")
p$s_percentile_expropriated_4 <- compute_antipoverty_maximum(df = s, threshold = 3.65, return = "percentile", growth = "trend")
p$s_percentile_expropriated_7 <- compute_antipoverty_maximum(df = s, threshold = 6.85, return = "percentile", growth = "trend")
p$s_percentile_expropriated_18 <- compute_antipoverty_maximum(df = s, threshold = 18.15, return = "percentile", growth = "trend")

p$s_y_expropriated_2 <- compute_antipoverty_maximum(df = s, threshold = 2.15, growth = "trend")
p$s_y_expropriated_4 <- compute_antipoverty_maximum(df = s, threshold = 3.65, growth = "trend")
p$s_y_expropriated_7 <- compute_antipoverty_maximum(df = s, threshold = 6.85, growth = "trend")
p$s_y_expropriated_18 <- compute_antipoverty_maximum(df = s, threshold = 18.15, growth = "trend")

p$s_percentile_expropriated_2_optimistic <- compute_antipoverty_maximum(df = s, threshold = 2.15, return = "percentile", growth = "optimistic")
p$s_percentile_expropriated_4_optimistic <- compute_antipoverty_maximum(df = s, threshold = 3.65, return = "percentile", growth = "optimistic")
p$s_percentile_expropriated_7_optimistic <- compute_antipoverty_maximum(df = s, threshold = 6.85, return = "percentile", growth = "optimistic")
p$s_percentile_expropriated_18_optimistic <- compute_antipoverty_maximum(df = s, threshold = 18.15, return = "percentile", growth = "optimistic")

p$s_y_expropriated_2_optimistic <- compute_antipoverty_maximum(df = s, threshold = 2.15, growth = "optimistic")
p$s_y_expropriated_4_optimistic <- compute_antipoverty_maximum(df = s, threshold = 3.65, growth = "optimistic")
p$s_y_expropriated_7_optimistic <- compute_antipoverty_maximum(df = s, threshold = 6.85, growth = "optimistic")
p$s_y_expropriated_18_optimistic <- compute_antipoverty_maximum(df = s, threshold = 18.15, growth = "optimistic")

# Antipoverty tax
p$s_antipoverty_2_tax_18 <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "trend")
p$s_antipoverty_2_tax_7 <- compute_antipoverty_tax(df = s, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "trend")
p$s_antipoverty_2_tax_4 <- compute_antipoverty_tax(df = s, exemption_threshold = 3.65, poverty_threshold = 2.15, growth = "trend")
p$s_antipoverty_2_tax_2 <- compute_antipoverty_tax(df = s, exemption_threshold = 2.15, poverty_threshold = 2.15, growth = "trend")
# p$s_antipoverty_bnpl_tax_bnpl <- compute_antipoverty_tax(df = s11, exemption_threshold = "bnpl", poverty_threshold = "bnpl", growth = "trend")
# p$s_antipoverty_bnpl_tax_7 <- compute_antipoverty_tax(df = s11, exemption_threshold = 5.5, poverty_threshold = "bnpl", growth = "trend") # In 2011PPP, 2.15/3.65/6.85 is 1.9/3.2/5.5 https://documents1.worldbank.org/curated/en/099700509122212929/pdf/IDU05b43a261041c504a5f0bb3405d0ef310b9e1.pdf
# p$s_antipoverty_bnpl_tax_18 <- compute_antipoverty_tax(df = s11, exemption_threshold = 15.31, poverty_threshold = "bnpl", growth = "trend") # 2011 Poverty line in the U.S. for a family of 4 is 15.31=22350/365/4 https://aspe.hhs.gov/2011-hhs-poverty-guidelines
# p$s_antipoverty_bcs_tax_bcs <- compute_antipoverty_tax(df = s11, exemption_threshold = "bcs", poverty_threshold = "bcs", growth = "trend")
# p$s_antipoverty_bcs_tax_7 <- compute_antipoverty_tax(df = s11, exemption_threshold = 5.5, poverty_threshold = "bcs", growth = "trend") # In 2011PPP, 2.15/3.65/6.85 is 1.9/3.2/5.5 https://documents1.worldbank.org/curated/en/099700509122212929/pdf/IDU05b43a261041c504a5f0bb3405d0ef310b9e1.pdf
# p$s_antipoverty_bcs_tax_18 <- compute_antipoverty_tax(df = s11, exemption_threshold = 15.31, poverty_threshold = "bcs", growth = "trend") # 2011 Poverty line in the U.S. for a family of 4 is 15.31=22350/365/4 https://aspe.hhs.gov/2011-hhs-poverty-guidelines
p$s_antipoverty_4_tax_7 <- compute_antipoverty_tax(df = s, exemption_threshold = 6.85, poverty_threshold = 3.65, growth = "trend")
p$s_antipoverty_4_tax_18 <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 3.65, growth = "trend")
p$s_antipoverty_7_tax_18 <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 6.85, growth = "trend")
p$s_antipoverty_18_tax_18 <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 18.15, growth = "trend")

p$s_antipoverty_2_tax_18_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "optimistic")
p$s_antipoverty_2_tax_7_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "optimistic")
p$s_antipoverty_2_tax_4_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 3.65, poverty_threshold = 2.15, growth = "optimistic")
p$s_antipoverty_2_tax_2_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 2.15, poverty_threshold = 2.15, growth = "optimistic")
p$s_antipoverty_4_tax_7_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 6.85, poverty_threshold = 3.65, growth = "optimistic")
p$s_antipoverty_4_tax_18_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 3.65, growth = "optimistic")
p$s_antipoverty_7_tax_18_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 6.85, growth = "optimistic")
p$s_antipoverty_18_tax_18_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 18.15, growth = "optimistic")


##### Results ##### 
sort(setNames(p$poverty_gap_2, p$country))
sort(setNames(p$antipoverty_2_tax_2, p$country))
sort(setNames(p$antipoverty_2_tax_7_optimistic, p$country))
sort(setNames(p$y_expropriated_2, p$country), decreasing = T)
sum(p$y_expropriated_2 < 18.15) # 19
sum(p$pop_2030[p$y_expropriated_2 < 18.15]) # 700M
sum(p$pop_2022[p$y_expropriated_2 < 18.15]) # 571M
sum(p$mean_Y < 6.85) # 8
sum(p$pop_2030[p$mean_Y < 6.85]) # 300M
sum(p$gdp_pc_2021 < 6.85*365, na.rm = T) # 22
sum(p$pop_2022[p$gdp_pc_2021 < 6.85*365], na.rm = T) # 521M
sort(setNames(p$gdp_pc_2030/365, p$country), decreasing = T)
decrit("antipoverty_2_tax_18")
decrit("antipoverty_2_tax_7")
decrit("antipoverty_2_tax_4")
decrit("antipoverty_2_tax_2")
decrit("antipoverty_4_tax_7")
decrit("antipoverty_4_tax_18")
decrit("antipoverty_7_tax_18")
decrit("antipoverty_18_tax_18")
decrit("y_expropriated_2")
decrit("y_expropriated_4")
decrit("y_expropriated_7")
decrit("y_expropriated_18")

# /!\ PROBLEM: huge discrepancies between PovcalNet and GDP pc data (this is because conso survey underestimates high-incomes and doesn't include investment), e.g. for MDG: 
p$mean_y_2022[p$country == "Madagascar"]
p$mean_y_2022[p$country == "Madagascar"]
p$scaling_factor[p$country == "Madagascar"]
p$gdp_pc_2022[p$country == "Madagascar"]/365 # GDP pc from World Bank


#####  Maps ##### 
plot_world_map("antipoverty_2_tax_2", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $2.15/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_2_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_2_tax_18", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $13/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_bnpl_tax_bnpl", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove Basic Needs\nPoverty Line (BNPL)\nrequired to lift all\nabove BNPL\n", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_bnpl_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $5.5/day\n(in 2011 PPP$)\nrequired to lift all\nabove Basic Needs", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_bnpl_tax_18", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $15.3/day\n(in 2011 PPP$)\nrequired to lift all\nabove Basic Needs", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_bcs_tax_bcs", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove Basic Consumption\nPoverty Line (BCS PL)\nrequired to lift all\nabove BCS PL\n", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_bcs_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $5.5/day\n(in 2011 PPP$)\nrequired to lift all\nabove Basic Consumption", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("antipoverty_bcs_tax_18", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $15.3/day\n(in 2011 PPP$)\nrequired to lift all\nabove Basic Consumption", #fill_na = T,  
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
plot_world_map("y_expropriated_18", breaks = c(0, 2.15, 4, 7, 13, 18.15, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
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
plot_world_map("antipoverty_2_tax_18_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $13/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("y_expropriated_2_optimistic", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("y_expropriated_7_optimistic", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $6.85/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("y_expropriated_18_optimistic", breaks = c(0, 2.15, 4, 7, 13, 18.15, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $13/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  


##### Maps in French #####
plot_world_map("antipoverty_2_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), add_folder = 'fr_',
               # labels = sub("≤", "<", agg_thresholds(c(0), c(0, .1, 1, 5, 10, 25, 50, 100, Inf), sep = "to", return = "levels")),
               legend = "Taux de taxe linéaire \nau-dessus de $6.85/jour\nnécessaire pour éradiquer\nl'extrême pauvreté à $2.15/jour\n(en 2017 PPA)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T) 
plot_world_map("antipoverty_2_tax_2", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), add_folder = 'fr_',
               legend = "Taux de taxe linéaire \nau-dessus de $2.15/jour\nnécessaire pour éradiquer\nl'extrême pauvreté à $2.15/jour\n(en 2017 PPA)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T) 
plot_world_map("antipoverty_2_tax_18", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), add_folder = 'fr_',
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
plot_world_map("y_expropriated_18", breaks = c(0, 2.15, 4, 7, 13, 18.15, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T,  add_folder = 'fr_',# svg, pdf
               legend = "Revenu journalier au\ndessus duquel tout devrait\nêtre exproprié pour élever \ntous les habitants du pays\nau-dessus de $13/jour\n(en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)
plot_world_map("poverty_gap_2", breaks = c(0, 2, 10, 20, 40, 60, 100), sep = " to ", end = "", strict_ineq_lower = FALSE,  add_folder = 'fr_',# svg, pdf
               legend = "Écart de pauvreté (en %)\nà $2.15/jour (en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .05, trim = T)

##### Maps with rescaling #####
plot_world_map("s_antipoverty_2_tax_2", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $2.15/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("s_antipoverty_2_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("s_antipoverty_2_tax_18", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $13/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("s_antipoverty_bnpl_tax_bnpl", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
#                legend = "Linear tax rate\nabove Basic Needs\nPoverty Line (BNPL)\nrequired to lift all\nabove BNPL\n", #fill_na = T,  
#                save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("s_antipoverty_bnpl_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
#                legend = "Linear tax rate\nabove $5.5/day\n(in 2011 PPP$)\nrequired to lift all\nabove Basic Needs", #fill_na = T,  
#                save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("s_antipoverty_bnpl_tax_18", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
#                legend = "Linear tax rate\nabove $15.3/day\n(in 2011 PPP$)\nrequired to lift all\nabove Basic Needs", #fill_na = T,  
#                save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("s_antipoverty_bcs_tax_bcs", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
#                legend = "Linear tax rate\nabove Basic Consumption\nPoverty Line (BCS PL)\nrequired to lift all\nabove BCS PL\n", #fill_na = T,  
#                save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("s_antipoverty_bcs_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
#                legend = "Linear tax rate\nabove $5.5/day\n(in 2011 PPP$)\nrequired to lift all\nabove Basic Consumption", #fill_na = T,  
#                save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("s_antipoverty_bcs_tax_18", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
#                legend = "Linear tax rate\nabove $15.3/day\n(in 2011 PPP$)\nrequired to lift all\nabove Basic Consumption", #fill_na = T,  
#                save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("percentile_expropriated_2", breaks = c(-Inf, 0, 50, 90, 95, 99, 100), sep = " to ", end = "", strict_ineq_lower = T, 
#                legend = "Percentile above which\nall should be expropriated\nto lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
#                save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T)  
# plot_world_map("percentile_expropriated_7", breaks = c(-Inf, 0, 50, 90, 95, 99, 100), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
#                legend = "Percentile above which\nall should be expropriated\nto lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
#                save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("s_y_expropriated_2", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("s_y_expropriated_7", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $6.85/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("s_y_expropriated_18", breaks = c(0, 2.15, 4, 7, 13, 18.15, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $13/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("s_poverty_gap_2", breaks = c(0, 2, 10, 20, 40, 60, 100), sep = " to ", end = "", strict_ineq_lower = FALSE, # svg, pdf 
               legend = "Poverty gap (in %)\n at $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .05, trim = T)  

plot_world_map("s_antipoverty_2_tax_2_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $2.15/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("s_antipoverty_2_tax_7_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("s_antipoverty_2_tax_18_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $13/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
plot_world_map("s_y_expropriated_2_optimistic", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("s_y_expropriated_7_optimistic", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $6.85/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  
plot_world_map("s_y_expropriated_18_optimistic", breaks = c(0, 2.15, 4, 7, 13, 18.15, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $13/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)  


mean(p$hfce/p$mean_welfare, na.rm = T) # 1.44 (20% NA)
mean((p$hfce/p$mean_welfare)[p$country_code %in% SSA], na.rm = T) # 1.32 (18% NA)
mean((p$hfce/p$mean_welfare)[p$country_code %in% LIC], na.rm = T) # 1.12 (32% NA)
mean(p$scaling_factor == 1, na.rm = T) # 15% HFCE < mean income
mean((p$scaling_factor == 1)[p$country_code %in% SSA], na.rm = T) # 25% HFCE < mean income
mean((p$scaling_factor == 1)[p$country_code %in% LIC], na.rm = T) # 41% HFCE < mean income
LIC <- c("AFG", "BFA", "BDI", "TCD", "COD", "ERI", "ETH", "GMB", "GIN", "GNB", "PRK", "LBR", "MDG", "MWI", "MLI", "MOZ", "NER", "RWA", "SOM", "SRE", "SDN", "SSD", "SYR", "TGO", "UGA", "YEM", "ZMB") # 2023 official classification. LIC: 650M people
SSA <- c("SDN", "AGO", "GIN", "GMB", "GNB", "GNQ", "BDI", "BEN", "BFA", "SEN", "BWA", "CAF", "SLE", "SOM", "SSD", "CIV", "CMR", "COD", "COG", "COM", "LBR", "LSO", "SWZ", "TCD", "TGO", "MLI", "MDG", "DJI", "ERI", "ESH", "ETH", "MWI", "MUS", "MRT", "MOZ", "TZA", "UGA", "ZMB", "ZWE", "NGA", "NER", "NAM", "GHA", "GAB")
summary(lm(I(gdp_pc_2019/gdp_pc_2011) ~ I(gdp_pc_2011/gdp_pc_1991) + I((gdp_pc_2011/gdp_pc_1991)^2), data = gdp_pc)) # Adj-R²: .27, the best from the models I have tested (linear or quadratic with different time periods). 0.9507844 + 0.1470747x - 0.0046657x²
summary(lm(I(gdp_pc_2019/gdp_pc_2017) ~ I(gdp_pc_2017/gdp_pc_2002) + I((gdp_pc_2017/gdp_pc_2002)^2), data = gdp_pc))


##### Paper #####
# Data
p$scaling_factor[p$country %in% c("Burundi", "Democratic Republic of the Congo")]
mean(p$year < 2022 & p$year > 2017)
# table(p$year)
# table(p$year[p$country_code %in% LIC])
mean(p$hfce/p$mean_welfare, na.rm = T) # 1.44 (20% NA)

# Balanced growth
growth_scenarios <- setNames(c("now", "trend", "trend_pos", "imf", "reg", "none", "average", "strong", "optimistic", "very_optimistic", "sdg8"), # , "bolch"
                             c("2022 Estimate", "Trend (2014--2019)", "Max(Trend, 0)", "IMF forecast", "Autoregressive projection", "0\\% growth", "3\\% growth", "4.5\\% growth", "6\\% growth", "7\\% growth", "7\\% growth since 2015")) # Quadratic model
table_poverty <- cbind(#"scenario" = names(growth_scenarios), 
  "rate2" = 100*sapply(growth_scenarios, function(s) compute_poverty_rate(df = w, threshold = 2.15, growth = s, return = "rate")), 
  "rate4" = 100*sapply(growth_scenarios, function(s) compute_poverty_rate(df = w, threshold = 3.65, growth = s, return = "rate")), 
  "rate7" = 100*sapply(growth_scenarios, function(s) compute_poverty_rate(df = w, threshold = 6.85, growth = s, return = "rate")), 
  "rate18" = 100*sapply(growth_scenarios, function(s) compute_poverty_rate(df = w, threshold = 18.15, growth = s, return = "rate")), 
  "gap2" = 100*sapply(growth_scenarios, function(s) compute_poverty_gap(df = w, threshold = 2.15, unit = '%', growth = s)), 
  "gap4" = 100*sapply(growth_scenarios, function(s) compute_poverty_gap(df = w, threshold = 3.65, unit = '%', growth = s)), 
  "gap7" = 100*sapply(growth_scenarios, function(s) compute_poverty_gap(df = w, threshold = 6.85, unit = '%', growth = s)),
  "gap18" = 100*sapply(growth_scenarios, function(s) compute_poverty_gap(df = w, threshold = 18.15, unit = '%', growth = s))) 
cat(sub("\\toprule\n", "\\toprule Growth scenario & \\multicolumn{4}{c}{Poverty rate (\\%)} & \\multicolumn{4}{c}{Poverty gap (\\% of GDP} \\\\ \n (Poverty line in \\$/day)", 
        paste(kbl(table_poverty, "latex", caption = "Global poverty rates and poverty gaps in 2030 under different growth scenarios. Poverty rates are expressed in \\% of world population and poverty gaps in \\% of world GDP. Poverty lines are in PPP \\$/day.", 
                  row.names = T, position = "h", escape = F, booktabs = T, digits = c(1, 1, 1, 1, 2, 2, 2, 2), label = "poverty_full", linesep = rep("", nrow(table_poverty)-1), 
                  col.names = c("2.15", "3.65", "6.85", "18.15", "2.15", "3.65", "6.85", "18.15")), collapse="\n"), fixed = T), file = "../tables/poverty_full.tex") 
cat(sub("\\toprule\n", "\\toprule Growth scenario & \\multicolumn{4}{c}{Poverty rate (\\%)} & \\multicolumn{4}{c}{Poverty gap (\\% of GDP)} \\\\ \n (Poverty line in \\$/day)", 
        paste(kbl(table_poverty[c(1, 2, 5, 7, 10, 11),], "latex", caption = "Global poverty rates and poverty gaps in 2030 under different growth scenarios. Poverty rates are expressed in \\% of world population and poverty gaps in \\% of world GDP. Poverty lines are in PPP \\$/day.", 
                  row.names = T, position = "h", escape = F, booktabs = T, digits = c(1, 1, 1, 1, 2, 2, 2, 2), label = "poverty", linesep = rep("", nrow(table_poverty)-1), caption.short = "Global poverty (rates and gaps) in 2030 under different growth scenarios.",
                  col.names = c("2.15", "3.65", "6.85", "18.15", "2.15", "3.65", "6.85", "18.15")), collapse="\n"), fixed = T), file = "../tables/poverty.tex") 


# Antipoverty cap
p$y_expropriated_2_average <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "average")
plot_world_map("y_expropriated_2_average", breaks = c(0, 2.15, 7, 13, 30, 60, 300, Inf), sep = " to ", end = "", strict_ineq_lower = T, limits = c(0, Inf),
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T, colors = color(11, rev_color = FALSE)[c(1,3,7:11)])  
sort(setNames(p$y_expropriated_2_average, p$country), decreasing = T)

setNames(((p$gdp_pc_2019/p$gdp_pc_2014)^0.2)[p$mean_y_2022 < 3 & !is.na(p$gdp_pc_2014)]-1, p$country[p$mean_y_2022 < 3 & !is.na(p$gdp_pc_2014)])
s$country[s$mean_y_2022 < 3 & !is.na(s$gdp_pc_2014)]
mean(((p$gdp_pc_2019/p$gdp_pc_2014)^0.2)[p$mean_y_2022 < 3 & !is.na(p$gdp_pc_2014)])-1
max(((p$gdp_pc_2019/p$gdp_pc_2014)^0.2)[p$mean_y_2022 < 3 & !is.na(p$gdp_pc_2014)])-1
mean(((p$gdp_pc_2022/p$gdp_pc_2014)^(1/7))[p$mean_y_2022 < 3 & !is.na(p$gdp_pc_2014)])-1
max(((p$gdp_pc_2022/p$gdp_pc_2014)^(1/7))[p$mean_y_2022 < 3 & !is.na(p$gdp_pc_2014)])-1

p$y_expropriated_2_very_optimistic <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "very_optimistic")
sort(setNames(p$y_expropriated_2_very_optimistic, p$country), decreasing = T)
p$s_y_expropriated_2_very_optimistic <- compute_antipoverty_maximum(df = s, threshold = 2.15, growth = "very_optimistic")
sort(setNames(p$s_y_expropriated_2_very_optimistic, p$country), decreasing = T)
p$y_expropriated_2_sdg8 <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "sdg8")
sort(setNames(p$y_expropriated_2_sdg8, p$country), decreasing = T)

plot_world_map("y_expropriated_2_very_optimistic", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T)  

p$s_y_expropriated_2_average <- compute_antipoverty_maximum(df = s, threshold = 2.15, growth = "average")
plot_world_map("s_y_expropriated_2_average", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, 
               legend = "Daily income above\nwhich all should\nbe expropriated\nto lift all in the country\nabove $2.15/day\n(in $ 2017 PPP)", #fill_na = T,  
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T)  

# Antipoverty taxes
p$antipoverty_2_tax_7_average <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "average")
plot_world_map("antipoverty_2_tax_7_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
sort(setNames(p$antipoverty_2_tax_7_average, p$country), decreasing = T)
wtd.mean(p$antipoverty_2_tax_7_average[p$country_code %in% SSA], p$pop_2030[p$country_code %in% SSA]) # 49%
wtd.mean(p$antipoverty_2_tax_7_average[p$country_code %in% LIC], p$pop_2030[p$country_code %in% LIC]) # 64%
sum(p$antipoverty_2_tax_7_average > 100) # 5

p$antipoverty_2_tax_18_very_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "very_optimistic")
plot_world_map("antipoverty_2_tax_18_very_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
sort(setNames(p$antipoverty_2_tax_18_very_optimistic, p$country), decreasing = T)

p$antipoverty_4_tax_4_bolch <- compute_antipoverty_tax(df = p, exemption_threshold = 3.44, poverty_threshold = 3.44, growth = "bolch")
sum(p$antipoverty_4_tax_4_bolch > 100, na.rm = T) # 30
p$antipoverty_4_tax_22_bolch <- compute_antipoverty_tax(df = p, exemption_threshold = 22.36, poverty_threshold = 3.44, growth = "bolch")
sum(p$antipoverty_4_tax_22_bolch > 100, na.rm = T) # 52
# p$antipoverty_2_tax_13_bolch <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 2, growth = "bolch")
# sum(p$antipoverty_2_tax_13_bolch > 100, na.rm = T) # 34
# p$antipoverty_4_tax_4_now <- compute_antipoverty_tax(df = p, exemption_threshold = 3.44, poverty_threshold = 3.44, growth = "now")
# sum(p$antipoverty_4_tax_4_now > 100) # 11
p$antipoverty_4_tax_18_now <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3.44, growth = "now")
p$antipoverty_4_tax_22_average <- compute_antipoverty_tax(df = p, exemption_threshold = 22.36, poverty_threshold = 3.44, growth = "average")
sum(p$antipoverty_4_tax_22_average > 100) # 34
p$antipoverty_4_tax_4_average <- compute_antipoverty_tax(df = p, exemption_threshold = 3.44, poverty_threshold = 3.44, growth = "average")
sum(p$antipoverty_4_tax_4_average > 100) # 6
# p$s_antipoverty_4_tax_18_now <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 3.44, growth = "now")
# p$s_antipoverty_4_tax_18_average <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 3.44, growth = "average")
# sum(p$s_antipoverty_4_tax_18_now > 100) # 86
# sum(p$s_antipoverty_4_tax_18_average > 100) # 66

p$antipoverty_2_tax_18_trend <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "trend")
p$mean_growth_gdp_pc_14_19[p$country == "India"] # 5.5%
p$antipoverty_2_tax_18_very_optimistic[p$country == "India"] # 10%
p$antipoverty_2_tax_18_trend[p$country == "India"] # 36%
p$antipoverty_2_tax_18_average[p$country == "India"] # 156%

p$antipoverty_2_tax_18_average <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "average")
plot_world_map("antipoverty_2_tax_18_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
sort(setNames(p$antipoverty_2_tax_18_very_optimistic, p$country), decreasing = T)

p$s_antipoverty_2_tax_7_average <- compute_antipoverty_tax(df = s, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "average")
plot_world_map("s_antipoverty_2_tax_7_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
p$s_antipoverty_2_tax_18_very_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "very_optimistic")
plot_world_map("s_antipoverty_2_tax_18_very_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  

sort(setNames(p$bcs, p$country), decreasing = T)
p$antipoverty_bcs_tax_bcs <- compute_antipoverty_tax(df = p11, exemption_threshold = "bcs", poverty_threshold = "bcs", growth = "average") # In 2011PPP, 2.15/3.65/6.85 is 1.9/3.2/5.5 https://documents1.worldbank.org/curated/en/099700509122212929/pdf/IDU05b43a261041c504a5f0bb3405d0ef310b9e1.pdf
package("spatstat")
weighted.median(p$bcs, p$pop_2030, na.rm = T)
sum(p$antipoverty_bcs_tax_bcs > 100, na.rm = T)
sort(setNames(p$antipoverty_bcs_tax_bcs, p$country), decreasing = T)
plot_world_map("antipoverty_bcs_tax_bcs", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove Basic Consumption\nrequired to lift all\nabove Basic Consumption\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  


# Demogrant for given tax
p$demogrant_7__10 <- compute_min_funded(revenues = tax_revenues(df = p, thresholds = 6.85, marginal_rates = 10, return = 'pc', growth = "average", scope_tax = p), var = name_var_growth("average"), df = p)
sort(setNames(p$demogrant_7__10, p$country))
sort(setNames(p$demogrant_7__10, p$country)[p$country_code %in% LIC])
sum(p$demogrant_7__10 < 2.15)
length(LIC)
sum(p$demogrant_7__10 < 2.15 & p$country_code %in% LIC)
plot_world_map("demogrant_7__10", breaks = c(0, 1.5, 2.15, 3, 4, 7, 10, 18, 30, 70, Inf), end = "$", sep = "$ to ",
               legend = "Income floor\nthat can be funded\nwith a 10% tax\nabove $6.85/day\n(in 2017 PPP $/day)", #fill_na = T,  
               save = T, rev_color = F, format = c('png', 'pdf'), legend_x = .07, trim = T)  

p$demogrant_7__10_very_optimistic <- compute_min_funded(revenues = tax_revenues(df = p, thresholds = 6.85, marginal_rates = 10, return = 'pc', growth = "very_optimistic", scope_tax = p), var = name_var_growth("very_optimistic"), df = p)
sum(p$demogrant_7__10_very_optimistic < 2.15)
p$s_demogrant_7__10_very_optimistic <- compute_min_funded(revenues = tax_revenues(df = s, thresholds = 6.85, marginal_rates = 10, return = 'pc', growth = "very_optimistic", scope_tax = p), var = name_var_growth("very_optimistic"), df = s)
sum(p$s_demogrant_7__10_very_optimistic < 2.15)
plot_world_map("demogrant_7__10_very_optimistic", breaks = c(0, 1.5, 2.15, 3, 4, 7, 10, 18, 30, 70, Inf), end = "$", sep = "$ to ",
               legend = "Income floor\nthat can be funded\nwith a 5% tax\nabove $6.85/day\n(in 2017 PPP $/day)", #fill_na = T,  
               save = T, rev_color = F, format = c('png', 'pdf'), legend_x = .07, trim = T)  
p$demogrant_7__10_sdg8 <- compute_min_funded(revenues = tax_revenues(df = p, thresholds = 6.85, marginal_rates = 10, return = 'pc', growth = "sdg8", scope_tax = p), var = name_var_growth("sdg8"), df = p)
sum(p$demogrant_7__10_sdg8 < 2.15)
sort(setNames(p$demogrant_7__10_sdg8, p$country), decreasing = T)

p$antipoverty_2_tax_7_sdg8 <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "sdg8")
sort(setNames(p$antipoverty_2_tax_7_sdg8, p$country))
plot_world_map("antipoverty_2_tax_7_sdg8", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T,  
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  


# Potential of global redistribution
p <- tax_revenues(df = p, thresholds = 6.85, marginal_rates = 10, return = 'df', growth = "average", name_tax = "7__10", scope_tax = p)
w <- compute_world_distribution(var = "Y3_tax_7__10") 
w$Y3_tax_7__10_gini # 59%
compute_poverty_gap(df = w, threshold = 6.85, unit = "%", growth = "Y3_tax_7__10") # 3.7%
w$Y3_gini # 62%
compute_poverty_gap(df = w, threshold = 6.85, unit = "%", growth = "Y3") # 4.5%
p <- tax_revenues(df = p, thresholds = 6.85, marginal_rates = 10, return = 'df', growth = "average", name_tax = "7__10_w", scope_tax = w)
w <- compute_world_distribution(var = "Y3_tax_7__10_w") 
w$Y3_tax_7__10_w_gini # 51%
compute_poverty_gap(df = w, threshold = 6.85, unit = "%", growth = "Y3_tax_7__10_w") # 0
w$Y3_tax_7__10_w_avg_1 # 8.42$

compute_min_funded(revenues = tax_revenues(df = w, thresholds = 100, marginal_rates = 10, return = 'pc', growth = "average", name_tax = "100__10_w", scope_tax = w), var = name_var_growth("average"), df = w) # 4.27
compute_antipoverty_tax(df = w, exemption_threshold = 100, poverty_threshold = 2.15, growth = "average") # 1.2%
compute_antipoverty_tax(df = w, exemption_threshold = 274, poverty_threshold = 2.15, growth = "average") # 8  %
w <- tax_revenues(df = w, thresholds = 100, marginal_rates = 1.18, return = 'df', growth = "average", name_tax = "100__1") # 1% of world GDP, 3.4% transferred
p <- tax_revenues(df = p, thresholds = 100, marginal_rates = 1.18, return = 'df', growth = "average", scope_tax = w, name_tax = "100__1") # 0.14% in international transfers
100*sort(setNames(p$gain_Y3_tax_100__1, p$country))
sum((p$gain_Y3_tax_100__1 * p$pop_2030 * p$mean_Y3)[p$country_code %in% LIC])/sum((p$pop_2030 * p$mean_Y3)[p$country_code %in% LIC]) # 3%


# w$mean_Y3_tax_7__10
# w$mean_Y3
as.numeric(w[,paste0("welfare_avg_", 1:100)])
as.numeric(w[,paste0("Y_avg_", 1:100)])
w[,paste0("Y3_tax_7__10_avg_", 1:100)]
w[,c(paste0("Y3_avg_", 97:100), paste0("Y3_min_", 97:100), paste0("Y3_pop_share_", 97:100))]
# w[,paste0("Y3_tax_7__10_pop_share_", 1:100)]
w <- compute_inequality(df = w, var = "Y3", recompute = T)

# Tax for the extreme poverty gap
compute_antipoverty_tax(df = w, poverty_threshold = 2.15, exemption_threshold = 100, growth = "average") # 1.2%

# Tax for 1% average
w$mean_Y3
w$Y3_avg_100
w$Y3_min_100
w$Y3_top1
compute_poverty_gap(df = w, threshold = 4, unit = '%', growth = "average") # 1%
compute_antipoverty_tax(df = w, poverty_threshold = 4, exemption_threshold = 100, growth = "average") # 8%
compute_antipoverty_maximum(df = w, threshold = 4, growth = "average") # top1
w <- tax_revenues(df = w, thresholds = 100, marginal_rates = 8, return = 'df', growth = "average", name_tax = "100__8") # 1% of world GDP, 23% transferred
p <- tax_revenues(df = p, thresholds = 100, marginal_rates = 8, return = 'df', growth = "average", scope_tax = w, name_tax = "100__8") # 1% in international transfers

# Taxes 1.2 and 15% above $100,000/year
w <- tax_revenues(df = w, thresholds = 1e5/365, marginal_rates = .5, return = 'df', growth = "average", name_tax = "100k__05") # 
p <- tax_revenues(df = p, thresholds = 1e5/365, marginal_rates = .5, return = 'df', growth = "average", scope_tax = w, name_tax = "100k__05") # 
w <- tax_revenues(df = w, thresholds = 1e5/365, marginal_rates = 14, return = 'df', growth = "average", name_tax = "100k__15") # 
p <- tax_revenues(df = p, thresholds = 1e5/365, marginal_rates = 14, return = 'df', growth = "average", scope_tax = w, name_tax = "100k__15") # 4

# Taxes 1.2 and 15% above $100,000/year with HFCE adjustment
ws <- tax_revenues(df = ws, thresholds = 1e5/365, marginal_rates = .5, return = 'df', growth = "average", name_tax = "100k__05") # 
s <- tax_revenues(df = s, thresholds = 1e5/365, marginal_rates = .5, return = 'df', growth = "average", scope_tax = ws, name_tax = "100k__05") # 
ws <- tax_revenues(df = ws, thresholds = 1e5/365, marginal_rates = 14, return = 'df', growth = "average", name_tax = "100k__15") # TODO! too high: 1.03
s <- tax_revenues(df = s, thresholds = 1e5/365, marginal_rates = 14, return = 'df', growth = "average", scope_tax = ws, name_tax = "100k__15") # 

# Tax for 3% GDP
compute_poverty_gap(df = w, threshold = 5.85, unit = '%', growth = "average") # 3%
compute_antipoverty_tax(df = w, poverty_threshold = 5.85, exemption_threshold = 100, growth = "average") # 24% 
compute_antipoverty_maximum(df = ws, threshold = 5.85, growth = "average") # top1
tax_revenues(df = w, thresholds = c(100, 200), marginal_rates = c(20, 33), return = '%', growth = "average") # 3% of world GDP

# Tax for 5% GDP
compute_poverty_gap(df = w, threshold = 7.15, unit = '%', growth = "average") # 5%
compute_antipoverty_tax(df = w, poverty_threshold = 7.15, exemption_threshold = 100, growth = "average") # 40% 
compute_antipoverty_maximum(df = w, threshold = 7.15, growth = "average") # 170
tax_revenues(df = w, thresholds = c(100, 200), marginal_rates = c(25, 75), return = '%', growth = "average") # 5% of world GDP
sum(sapply(1:100, function(i) w[[paste0("Y3_max_", i)]] < 100))
(5000*12/365-100)*0.25/(5000*12/365) # 9.8%
((10000*12/365-200)*0.5+(10000*12/365-100)*0.25)/(10000*12/365)
sum(sapply(1:100, function(i) ws[[paste0("Y3_max_", i)]] < 1e5*12/365))
(1.5e5*12/365-1e5*12/365)*0.15/(1.5e5*12/365)

# With w
compute_antipoverty_tax(df = w, poverty_threshold = 2.15, exemption_threshold = 1e5/365, growth = "average") # 8% => this one

# With ws
compute_antipoverty_tax(df = ws, poverty_threshold = 2.15, exemption_threshold = 1e5/365, growth = "average") # 0.47% => this one
# compute_poverty_gap(df = ws, threshold = 4.4, unit = '%', growth = "average") # 1
# compute_antipoverty_tax(df = ws, poverty_threshold = 4.4, exemption_threshold = 100, growth = "average") # 2.9%
compute_poverty_gap(df = ws, threshold = 6.85, unit = '%', growth = "average") # 3.3%
compute_antipoverty_tax(df = ws, poverty_threshold = 6.85, exemption_threshold = 100, growth = "average") # 9.8% 
compute_antipoverty_tax(df = ws, poverty_threshold = 6.85, exemption_threshold = 1e5/365, growth = "average") # 15% => this one 
# compute_poverty_gap(df = ws, threshold = 250*12/365, unit = '%', growth = "average") # 5.2%
# compute_antipoverty_tax(df = ws, poverty_threshold = 250*12/365, exemption_threshold = 100, growth = "average") # 15% 
ws$Y3_avg_100 # 1033
ws$Y3_min_100 # 311 vs. 274 = 1e5/365

## Appendix tables
selected_countries <- order(p$country)[order(p$country) %in% which(p$pop_2022 > 1e7 & no.na(p$mean_welfare, 0, num_as_char = FALSE) < 6)]

# Table cap
# % TODO! table cap by country for different scenarios (incl. 2.15$ 3%, 7%, with and without HFCE, 3% BCS; $3.44 Bolch, 3%)
table_cap <- cbind("y_expropriated_2_average" = p$y_expropriated_2_average, "y_expropriated_2_very_optimistic" = p$y_expropriated_2_very_optimistic, 
                                   "s_y_expropriated_2_average" = compute_antipoverty_maximum(df = s, threshold = 2.15, growth = "average"), "s_y_expropriated_2_very_optimistic" = p$s_y_expropriated_2_very_optimistic, 
                                    "y_expropriated_bcs_average" = compute_antipoverty_maximum(df = p, threshold = "bcs", growth = "average"), 
                                    "y_expropriated_3_average" = compute_antipoverty_maximum(df = p, threshold = 3.44, growth = "average"), "y_expropriated_3_bolch" = compute_antipoverty_maximum(df = p, threshold = 3.44, growth = "bolch"))
row.names(table_cap) <- p$country
(table_cap <- table_cap[selected_countries,]) # 
cat(sub("toprule", "toprule Poverty line (\\\\$/day) & 2.15 & 2.15 & 2.15 & 2.15 & BCS & 3.44 & 3.44 \\\\\\\\ Growth scenario & 3\\\\% & 7\\\\% & 3\\\\% & 7\\\\% & 3\\\\% & 3\\\\% & BCL \\\\\\\\ National accounts adjustment & & & \\\\checkmark & \\\\checkmark & & & \\\\\\\\  \\\\midrule", 
        gsub("Inf", "$+\\infty$", paste(kbl(table_cap, "latex", caption = "Antipoverty caps for major lower-income countries in 2030.", position = "b", escape = F, booktabs = T, digits = 1, linesep = rep("", nrow(table_cap)-1), longtable = F, label = "cap",
              col.names = NULL), collapse="\n"), fixed = T)), file = "../tables/cap.tex")  # \\\\multicolumn{4}{c}{\\\\$2.15/day} & BCS & \\\\multicolumn{2}{c}{\\\\$3.44/day}

# Table tax
# % TODO! table tax: $2.15 above $6.85 g 3%; $2.15 ab $18.15 g 7%; $2.15 ab $18.15 g 3%; $2.15 ab $18.15 g trend; ($2.15 ab $6.85 g 7%; $2.15 above $6.85 g 3% HFCE; $2.15 ab $18.15 g 7% HFCE)
table_tax <- cbind("antipoverty_2_tax_7_average" = p$antipoverty_2_tax_7_average, "antipoverty_2_tax_18_very_optimistic" = p$antipoverty_2_tax_18_very_optimistic, 
                   "antipoverty_2_tax_18_average" = p$antipoverty_2_tax_18_average, "antipoverty_2_tax_18_trend" = p$antipoverty_2_tax_18_trend, 
                   "antipoverty_2_tax_7_very_optimistic" = compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "very_optimistic"), 
                   "s_antipoverty_2_tax_7_average" = p$s_antipoverty_2_tax_7_average, "s_antipoverty_2_tax_18_very_optimistic" = p$s_antipoverty_2_tax_18_very_optimistic)
row.names(table_tax) <- p$country
(table_tax <- table_tax[selected_countries,]) 
cat(gsub("9999.0", "$>$ 10k", sub("toprule", "toprule Taxation threshold (\\\\$/day) & 6.85 & 18.15 & 18.15 & 18.15 & 6.85 & 6.85 & 18.15 \\\\\\\\ Growth scenario & 3\\\\% & 7\\\\% & 3\\\\% & Trend & 7\\\\% & 3\\\\% & 7\\\\% \\\\\\\\ National accounts adjustment & & & & & & \\\\checkmark & \\\\checkmark \\\\\\\\  \\\\midrule", 
        paste(kbl(pmin(table_tax, 9999), "latex", caption = "Anti-extreme-poverty taxes for major lower-income countries in 2030.", position = "b", escape = F, booktabs = T, digits = 1, linesep = rep("", nrow(table_tax)-1), longtable = F, label = "tax",
                                            col.names = NULL), collapse="\n")), fixed = T), file = "../tables/tax.tex")  # \\\\multicolumn{4}{c}{\\\\$2.15/day} & BCS & \\\\multicolumn{2}{c}{\\\\$3.44/day}

# Table floor
# % TODO! table floor: 10% ab $6.85 g 3%; g 3% with HFCE; g 7%; g 7% since 2016
table_floor <- cbind("demogrant_7__10" = p$demogrant_7__10, "s_demogrant_7__10" = compute_min_funded(revenues = tax_revenues(df = s, thresholds = 6.85, marginal_rates = 10, return = 'pc', growth = "average", scope_tax = p), var = name_var_growth("average"), df = s), 
                     "demogrant_7__10_very_optimistic" = p$demogrant_7__10_very_optimistic, "s_demogrant_7__10_very_optimistic" = p$s_demogrant_7__10_very_optimistic,
                   "demogrant_7__10_sdg8" = p$demogrant_7__10_sdg8)
row.names(table_floor) <- p$country
(table_floor <- table_floor[selected_countries,])
cat(sub("toprule", "toprule Growth scenario over 2022--2030 & 3\\\\% & 3\\\\% & 7\\\\% & 7\\\\% & \\\\makecell{7\\\\% since \\\\\\\\ 2015} \\\\\\\\ National accounts adjustment & & \\\\checkmark & & \\\\checkmark & \\\\\\\\  \\\\midrule", 
                              paste(kbl(table_floor, "latex", caption = "Income floor (in \\$/day) financed by a 10\\% tax above \\$10/day for major lower-income countries in 2030.", position = "b", escape = F, booktabs = T, digits = 1, linesep = rep("", nrow(table_floor)-1), longtable = F, label = "tax",
                                        col.names = NULL), collapse="\n")), file = "../tables/floor.tex")  # \\\\multicolumn{4}{c}{\\\\$2.15/day} & BCS & \\\\multicolumn{2}{c}{\\\\$3.44/day}

# Table net gain
# % TODO! net gain per country 1.2% / 15% tax ab $100/day w & wo HFCE + TODO: global revenue raised (% of global GDP) + global floor + global transfer + global Gini
table_gain <- cbind("mean_Y3_tax_100k__05" = p$mean_Y3_tax_100k__05, "mean_Y3_tax_100k__15" = p$mean_Y3_tax_100k__15, 
                    "s_mean_Y3_tax_100k__05" = s$mean_Y3_tax_100k__05, "s_mean_Y3_tax_100k__15" = s$mean_Y3_tax_100k__15)
row.names(table_gain) <- p$country # TODO! pb chiffres
table_gain <- 100*(table_gain/p$mean_Y3 - 1)
(table_gain <- table_gain[order(p$country)[order(p$country) %in% which(p$pop_2022 > 35e6) & !duplicated(p$country)],]) 
cat(sub("toprule", "toprule Tax rate & 0.5\\\\% & 15\\\\% & 0.5\\\\% & 15\\\\%  \\\\\\\\ National accounts adjustment &  & \\\\checkmark & & \\\\checkmark \\\\\\\\  \\\\midrule", # Taxation threshold (\\\\$/year) & 100k & 100k & 100k & 100k  \\\\\\\\ 
        paste(kbl(table_gain, "latex", caption = "Net gain per country of a global antipoverty tax on income above \\$100,000/year, for most populous countries in 2030 after 3\\% growth.", position = "b", escape = F, booktabs = T, digits = 2, linesep = rep("", nrow(table_gain)-1), longtable = F, label = "tax",
                  col.names = NULL), collapse="\n")), file = "../tables/gain.tex")  # \\\\multicolumn{4}{c}{\\\\$2.15/day} & BCS & \\\\multicolumn{2}{c}{\\\\$3.44/day}

# TODO:
# digits
# % Include mean SSA + LIC in Table + result at global level (w)
# % $3.65 p / 'reg' growth




















