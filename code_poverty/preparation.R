# Running preparation.R lasts ~1h
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
  if (any(exemption_threshold > df[[paste0(y, "_max_100")]], na.rm = T)) warning("exemption_threshold too high, this case is not properly")
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
    if (is.na(revenues)) demogrant <- NA
    else {
      cost <- (df[[paste0(var, "_avg_", 2)]] - df[[paste0(var, "_avg_", 1)]]) * df[[paste0(find_pop_share_var(var, df), 1)]]
      cumulated_pop <- df[[paste0(find_pop_share_var(var, df), 1)]]
      i <- 2
      while (cost < revenues) {
        cumulated_pop <- cumulated_pop + df[[paste0(find_pop_share_var(var, df), i)]]
        cost <- cost + (df[[paste0(var, "_avg_", i+1)]] - df[[paste0(var, "_avg_", i)]]) * cumulated_pop
        i <- i+1
      }
      demogrant <- df[[paste0(var, "_avg_", i)]] - (cost - revenues)/cumulated_pop 
    }
    for (j in 1:(i-1)) df[[paste0(var, "_avg_", j)]] <- demogrant
    if (return == "df") return(df)
    else if (return == "percentile") return(cumulated_pop)
    else return(demogrant)
  } else if (nrow(df) == 1) { return(compute_min_funded(revenues = revenues, var = var, df = df, return = "min")) # Unused. Beware, revenues must be in $/person
  } else return(sapply(1:length(revenues), function(i) compute_min_funded(revenues = revenues[i], var = var, df = df[i,], return = "min")))
}

compute_income_floor <- function(thresholds, marginal_rates, name_tax = "custom", df = p, growth = "optimistic", scope_tax = df) {
  return(compute_min_funded(revenues = tax_revenues(thresholds = thresholds, marginal_rates = marginal_rates, name_tax = name_tax, df = df, growth = growth, return = 'pc', scope_tax = scope_tax), var = name_var_growth(growth), df = df, return = "min"))
}

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
      df[[paste0("gain_", var, "_tax_", name_tax)]] <- rowSums(pmax(0, df[,paste0(var, "_tax_", name_tax,  "_avg_", 1:100)] - df[,paste0(var, "_avg_", 1:100)]) * df[,paste0(find_pop_share_var(var, df), 1:100)])/df[[paste0("mean_", var)]]
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
    if (growth == "trend_pos") df$gdp_pc_2030 <- df$gdp_pc_2022 * (1 + pmax(0, df$mean_growth_gdp_pc_14_19))^8 
    df$growth_gdp_pc_year_30 <- df$gdp_pc_2030/df$gdp_pc_year
    # df$country_code[is.na(df$gdp_pc_year)] # "SSD" "SYR" "VEN" "YEM"
    # df$country_code[is.na(df$growth_gdp_pc_year_30)] # "SSD" "SYR" "TKM" "VEN" "YEM". TKM has grown 7.368 times in 98-22 and .994 in 14-19 https://www.imf.org/external/datamapper/PPPPC@WEO/SYR/VEN/YEM/SSD/TKM
    df$growth_gdp_pc_year_30[is.na(df$growth_gdp_pc_year_30)] <- 1 
    growths <- df$growth_gdp_pc_year_30
  } else if (growth == "sdg8") {
    df$gdp_pc_2030_sdg <- df$gdp_pc_2015 * (1.07^15) 
    df$growth_gdp_pc_year_30_sdg <- df$gdp_pc_2030_sdg/df$gdp_pc_year
    df$growth_gdp_pc_year_30_sdg[is.na(df$growth_gdp_pc_year_30_sdg)] <- 1.07^15
    growths <- df$growth_gdp_pc_year_30_sdg
  } else if (growth == "imf") {
    df$growth_gdp_pc_year_30_imf <- df$gdp_pc_2030_imf/df$gdp_pc_year
    growths <- df$growth_gdp_pc_year_30_imf
  } else if (growth == "reg") {
    df$growth_gdp_pc_year_30_reg <- df$gdp_pc_2030_reg/df$gdp_pc_year
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
      new_line <- df[u, ]
      new_line$reporting_level <- "national"
      df <- rbind(df, new_line)
    }
    quantiles <- sort(unlist(sapply(1:100, function(i) {df[[paste0(y, "_avg_", i)]][df$country_code == c] }))) # Before, was _max_ here in and cdf, but this didn't work (some avg were missing). 
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
compute_world_distribution <- function(var = name_var_growth("optimistic"), df = p, wdf = w, region = df$country_code) {
  df <- df[df$country_code %in% region,]
  wdf$pop_2022 <- sum(df$pop_2022, na.rm = T)
  wdf$pop_2030 <- sum(df$pop_2030, na.rm = T)
  pop_yr <- if (grepl("2022|now", var)) "pop_2022" else "pop_2030"
  wquantiles <- unique(sort(unlist(sapply(1:100, function(i) {df[[paste0(var, "_max_", i)]] }))))
  wcdf <- c() # ~ 1 min
  for (q in wquantiles) wcdf <- c(wcdf, sum(sapply(1:100, function(j) { sum((df[[paste0(var, "_max_", j)]] <= q) * df[[paste0("pop_share_", j)]] * df[[pop_yr]], na.rm = T) })))
  wpop <- wcdf[length(wcdf)]
  wcdf <- wcdf/wpop
  wpercentiles <- findInterval(seq(0, 1, .01), wcdf)[-1] # computes the indices for which the pop_share is lesser or equal to the percentiles.
  wdf[[paste0(var, "_", pop_yr)]] <- wpop
  wdf[[paste0(var, "_max_0")]] <- 0
  for (i in 1:100) {
    wdf[[paste0(var, "_max_", i)]] <- wquantiles[wpercentiles[i]]
    wdf[[paste0(var, "_min_", i)]] <- wdf[[paste0(var, "_max_", i-1)]]
    wdf[[paste0(var, "_pop_share_", i)]] <- sum(sapply(1:100, function(k) { sum(df[[pop_yr]] * df[[paste0("pop_share_", k)]] * (df[[paste0(var, "_avg_", k)]] <= wdf[[paste0(var, "_max_", i)]]) * (df[[paste0(var, "_avg_", k)]] > wdf[[paste0(var, "_max_", i-1)]]), na.rm = T) }))/sum(df[[pop_yr]])
    wdf[[paste0(var, "_avg_", i)]] <- if (wdf[[paste0(var, "_pop_share_", i)]] == 0) wdf[[paste0(var, "_avg_", i-1)]] else (sum(sapply(1:100, function(k) { sum(df[[pop_yr]] * df[[paste0("pop_share_", k)]] * df[[paste0(var, "_avg_", k)]] * (df[[paste0(var, "_avg_", k)]] <= wdf[[paste0(var, "_max_", i)]]) * (df[[paste0(var, "_avg_", k)]] > wdf[[paste0(var, "_max_", i-1)]]), na.rm = T) }))) / (wdf[[paste0(var, "_pop_share_", i)]]*sum(df[[pop_yr]]))
  }
  wdf[[paste0("mean_", var)]] <- rowSums(wdf[,paste0(var, "_avg_", 1:100)] * wdf[,paste0(var, "_pop_share_", 1:100)]) 
  
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
  p$gdp_pc_2022[is.na(p$gdp_pc_2022)] <- pmax(p$gdp_pc_2021, pmax(p$gdp_pc_2020, p$gdp_pc_2019, na.rm = T), na.rm = T)[is.na(p$gdp_pc_2022)]
  p$mean_growth_gdp_pc_14_19 <- (p$gdp_pc_2019/p$gdp_pc_2014)^(1/5)-1 # pmax(0, )
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
  # iso3$country_code[!iso3$country_code %in% p$country_code] 
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
  p <- compute_distribution_2030(growth = "trend_pos", df = p) 
  p <- compute_distribution_2030(growth = "strong", df = p, growth_rate = 1.045, name_var = "Y4")
  p <- compute_distribution_2030(growth = "average", df = p, growth_rate = 1.03, name_var = "Y3")
  p <- compute_distribution_2030(growth = "sdg8", df = p) # The SDG 8.1: sustained 7% growth starting over 2016-30.
  p <- compute_distribution_2030(growth = "none", df = p)
  p <- compute_distribution_2030(growth = "now", df = p)
  # df <- p
  
  p <- p[(!p$country_code %in% c("CHN", "IDN", "IND")) | p$reporting_level == "national",]
  
  return(p)
}

create_world_distribution <- function(df = p17, region = df$country_code) {
  w <- data.frame(country = "World", pop_2022 = sum(df$pop_2022), pop_2030 = sum(df$pop_2030))
  w <- compute_world_distribution(name_var_growth("imf"), df = df, wdf = w, region = region)
  w <- compute_world_distribution(name_var_growth("reg"), df = df, wdf = w, region = region)
  w <- compute_world_distribution(name_var_growth("optimistic"), df = df, wdf = w, region = region)  
  w <- compute_world_distribution("Y7", df = df, wdf = w, region = region)  # ~ 1.5 min
  w <- compute_world_distribution(name_var_growth("trend"), df = df, wdf = w, region = region)
  w <- compute_world_distribution(name_var_growth("trend_pos"), df = df, wdf = w, region = region)
  w <- compute_world_distribution("Y4", df = df, wdf = w, region = region)
  w <- compute_world_distribution("Y3", df = df, wdf = w, region = region)
  w <- compute_world_distribution(name_var_growth("sdg8"), df = df, wdf = w, region = region)
  w <- compute_world_distribution(name_var_growth("none"), df = df, wdf = w, region = region)
  w <- compute_world_distribution(name_var_growth("now"), df = df, wdf = w, region = region)
  w <- compute_world_distribution(name_var_growth("bolch"), df = df, wdf = w, region = region)
  return(w)
}

create_appendix_table <- function(fun, ncol = NULL, poverty_thresholds = NULL, exemption_thresholds = NULL, thresholds = NULL, growths = "average", dfs = list(p), tax_rates = NULL, taxation_thresholds = NULL, regions = c("Sub-Saharan Africa", "Low-Income Countries", "World"), displayed_countries = selected_countries, scopes_tax = NULL, marginal_rates = NULL, return = "table") {
  params <- list("poverty_threshold" = poverty_thresholds, "exemption_threshold" = exemption_thresholds, "threshold" = thresholds, "growth" = growths, "tax_rate" = tax_rates, "taxation_threshold" = taxation_thresholds, "df" = dfs, "thresholds" = thresholds, "marginal_rates" = marginal_rates, "scope_tax" = scopes_tax)
  required_params <- names(params)[names(params) %in% formalArgs(eval(str2expression(fun)))]
  if (is.null(ncol)) ncol <- max(sapply(params, function(k) length(k)))
  for (k in names(params)) if (length(params[[k]]) == 1) params[[k]] <- rep(params[[k]], ncol)
  table <- matrix(NA, nrow = nrow(dfs[[1]]), ncol = 0)
  args <- list()
  for (i in 1:ncol) {
    args[[i]] <- list()
    for (k in required_params) args[[i]] <- c(args[[i]], list(params[[k]][[i]]))
    names(args[[i]]) <- required_params
    table <- cbind(table, do.call(fun, args[[i]]))
  }
  row.names(table) <- dfs[[1]]$country_short
  table <- table[displayed_countries,]
  region_args <- args
  for (r in regions) {
    top_row <- c()
    for (i in 1:ncol) {
      region_args[[i]]$df <- case_when(identical(args[[i]]$df, p) & r == "World" ~ w,
                                       identical(args[[i]]$df, p) & r == "Sub-Saharan Africa" ~ ssa,
                                       identical(args[[i]]$df, p) & r == "Low-Income Countries" ~ lic,
                                       identical(args[[i]]$df, s) & r == "World" ~ ws,
                                       identical(args[[i]]$df, s) & r == "Sub-Saharan Africa" ~ ssas,
                                       identical(args[[i]]$df, s) & r == "Low-Income Countries" ~ lics)
      top_row <- c(top_row, do.call(fun, region_args[[i]]))
    }
    table <- rbind(top_row, table)
    row.names(table)[1] <- r
  }
  for (i in 1:ncol) args[[i]]$df <- if (identical(args[[i]]$df, p)) "p" else "s"
  if (return == 'args') return(args)
  else return(table)
}

##### Create dataset #####
# Load population
pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
pop$PopTotal <- 1e3 * pop$PopTotal
pop <- pop[pop$Time %in% c(sort(unique(p$year)), 2030),] # TODO! p not defined yet. Simply remove?
pop_iso3 <- aggregate(PopTotal ~ Time + ISO3_code, data = pop, FUN = sum)
pop_iso3 <- pop_iso3 %>% pivot_wider(names_from = Time, values_from = PopTotal)
names(pop_iso3) <- c("country_code", paste0("pop_", names(pop_iso3)[-1]))
rm(pop)

pop_rural_urban <- read.csv2("../data/pop_rural_urban.csv") # Last updated 07/05/2023 https://databank.worldbank.org/source/population-estimates-and-projections/preview/on#
LIC <- c("AFG", "BFA", "BDI", "TCD", "COD", "ERI", "ETH", "GMB", "GIN", "GNB", "PRK", "LBR", "MDG", "MWI", "MLI", "MOZ", "NER", "RWA", "SOM", "SRE", "SDN", "SSD", "SYR", "TGO", "UGA", "YEM", "ZMB") # 2023 official classification. LIC: 650M people TODO! remove ZMB, no longer in the list
SSA <- c("SDN", "AGO", "GIN", "GMB", "GNB", "GNQ", "BDI", "BEN", "BFA", "SEN", "BWA", "CAF", "SLE", "SOM", "SSD", "CIV", "CMR", "COD", "COG", "COM", "LBR", "LSO", "RWA", "SWZ", "TCD", "TGO", "MLI", "MDG", "DJI", "ERI", "ESH", "ETH", "MWI", "MUS", "MRT", "MOZ", "TZA", "UGA", "ZMB", "ZWE", "NGA", "NER", "NAM", "GHA", "GAB", "ZAF")
# setdiff(p$country_code[p$country_code %in% LIC & !is.na(p$welfare_avg_1)], SSA) # SYR, YEM
# setdiff(SSA, LIC) # many

start <- Sys.time()
p <- p17 <- create_p()
s <- create_p(rescale = T)
p11 <- create_p(ppp_year = 2011)
w <- create_world_distribution()
ws <- create_world_distribution(df = s)
ssa <- create_world_distribution(region = SSA)
lic <- create_world_distribution(region = LIC)
ssas <- create_world_distribution(region = SSA, df = s)
lics <- create_world_distribution(region = LIC, df = s)
# w11 <- create_world_distribution(df = p11) # 9 min

selected_countries <- order(p$country)[order(p$country) %in% which(p$pop_2022 > 1e7 & no.na(p$mean_welfare, 0, num_as_char = FALSE) < 6)]
p$country_short <- p$country
p$country_short[p$country == "Democratic Republic of the Congo"] <- "D.R. Congo"

print(Sys.time() - start) # 1h
beep()
save.image(".RData")
