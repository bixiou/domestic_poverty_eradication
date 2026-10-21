# Updated versions of all figures and tables of poverty.tex and SM.tex (Fabre 2025, JDS) with the September 2026 PIP data (2021 PPP).
# Run after update.R (uses its checkpoint .RData_update). Outputs: ../figures/update/*.pdf|png and ../tables/update/*.tex, gathered in paper/update_appendix.tex.
# Correspondence of lines and thresholds (2017 PPP -> 2021 PPP): $2.15 -> $3.00, $3.65 -> $4.20, $6.85 -> $8.30, $18.15 unchanged, BCL's $3.44 -> $4.80 (= 3.44 * 3/2.15).
# Kenya example: $4 line and $7 threshold -> $4.20 and $8.30. Growth scenarios start in 2025, after observed growth (cf. update.R).
# The "7% since 2016" (sdg8) scenario is not updated, as it would contradict the use of observed growth until 2025.
load(".RData_update")
LIC <- c(setdiff(LIC, "SRE"), "SLE", "CAF")
dir.create("../figures/update", showWarnings = F)
dir.create("../tables/update", showWarnings = F)
fig <- "../figures/update/"

# Regional distributions (needed for the regional rows of appendix tables)
ssa <- create_world_distribution(region = SSA)
lic <- create_world_distribution(region = LIC)
ssas <- create_world_distribution(region = SSA, df = s)
lics <- create_world_distribution(region = LIC, df = s)
# Major lower-income countries: population above 10M and mean consumption below $8.4/day (= $6 in 2017 PPP * 3/2.15)
selected_countries <- order(p$country)[order(p$country) %in% which(p$pop_2022 > 1e7 & no.na(p$mean_welfare, 0, num_as_char = FALSE) < 6 * 3/2.15)]
s$country_short <- p$country_short

save_map <- function(name, ..., save = T, format = NULL, trim = T) { # Maps are drawn on 13x6.3 inches (1300x630 px), as in paper.R, then cropped
  pdf(NULL, width = 13, height = 6.3)
  plot_world_map(name, ..., save = FALSE)
  dev.off()
  plot <- last_plot() + theme(legend.position = "left", legend.justification = c(0, 0.5), legend.margin = margin(0, 0, 0, 10), legend.title = element_text(size = 12), legend.text = element_text(size = 11)) # legend outside the map, so that it is never clipped
  for (f in c("pdf", "png")) ggsave(paste0(fig, name, ".", f), plot, width = 13, height = 6.3, dpi = 100, bg = "white")
  magick::image_write(magick::image_trim(magick::image_read(paste0(fig, name, ".png"))), paste0(fig, name, ".png"))
}
growth_legend <- "in 2030, after observed\ngrowth to 2025, then 3%."
growth_legend_7 <- "in 2030, after observed\ngrowth to 2025, then 7%."
tax_breaks <- c(0, .1, 1, 5, 10, 25, 50, 100, Inf)


##### Table 1: global poverty #####
file.copy("../tables/poverty_update.tex", "../tables/update/poverty.tex", overwrite = T)


##### Figure 1: idealized redistributive policies (Kenya) #####
p$antipoverty_42_cap_average <- compute_antipoverty_cap(df = p, threshold = 4.2, growth = "average")
p$antipoverty_42_tax_8_average <- compute_antipoverty_tax(df = p, exemption_threshold = 8.3, poverty_threshold = 4.2, growth = "average")
p$floor_83__10 <- compute_income_floor(df = p, thresholds = 8.3, marginal_rates = 10, growth = "average", scope_tax = p)
kenya <- p$country == "Kenya"
distr <- unlist(p[kenya, paste0("Y3_avg_", 1:100)])
cap_k <- p$antipoverty_42_cap_average[kenya]; tax_k <- p$antipoverty_42_tax_8_average[kenya]; floor_k <- p$floor_83__10[kenya]
kenya_numbers <- c(cap = cap_k, tax = tax_k, floor = floor_k)
ylab <- "Consumption (in 2021 PPP $/day)"; xlab <- "Percentile of the consumption distribution in Kenya"
for (f in c("pdf", "png")) {
  dev_open <- function(name) if (f == "pdf") pdf(paste0(fig, name, ".pdf"), width = 5.5, height = 4.5) else png(paste0(fig, name, ".png"), width = 550, height = 450, res = 100)
  dev_open("Kenya_cap")
  par(mar = c(3.1, 3.1, .2, .1), mgp = c(2, 1, 0))
  plot(1:100, distr, type = 'l', lwd = 2, ylab = ylab, xlab = xlab, ylim = c(0, 28))
  lines(1:100, pmax(4.2, pmin(distr, cap_k)), lwd = 2, lty = 6, col = 'red')
  abline(h = c(4.2, round(cap_k, 1)), lty = 9, col = 'grey'); grid(ny = NA)
  dev.off()
  dev_open("Kenya_tax")
  par(mar = c(3.1, 3.1, .2, .1), mgp = c(2, 1, 0))
  plot(1:100, distr, type = 'l', lwd = 2, ylab = ylab, xlab = xlab, ylim = c(0, 28))
  lines(1:100, pmax(4.2, distr - .01 * tax_k * pmax(0, distr - 8.3)), lwd = 2, lty = 8, col = 'blue')
  abline(h = c(4.2, 8.3), lty = 9, col = 'grey'); grid(ny = NA)
  dev.off()
  dev_open("Kenya_floor")
  par(mar = c(3.1, 3.1, .2, .1), mgp = c(2, 1, 0))
  plot(1:100, distr, type = 'l', lwd = 2, ylab = ylab, xlab = xlab, ylim = c(0, 28))
  lines(1:100, pmax(floor_k, distr - 0.1 * pmax(0, distr - 8.3)), lwd = 2, lty = 5, col = 'darkgreen')
  abline(h = c(floor_k, 8.3), lty = 9, col = 'grey'); grid(ny = NA)
  dev.off()
  dev_open("Kenya_policies")
  par(mar = c(3.1, 3.1, .2, .1), mgp = c(2, 1, 0))
  plot(c(0, distr), c(0, distr), type = 'l', lwd = 2, ylab = "Consumption after policy (in 2021 PPP $/day)", xlab = "Current consumption (in 2021 PPP $/day)", xlim = c(0, 30), ylim = c(0, 30))
  lines(distr, pmax(4.2, distr - .01 * tax_k * pmax(0, distr - 8.3)), lty = 8, col = "blue", lwd = 2)
  lines(distr, pmax(4.2, pmin(distr, cap_k)), lwd = 2, lty = 6, col = 'red')
  lines(distr, pmax(floor_k, distr - 0.1 * pmax(0, distr - 8.3)), lwd = 2, lty = 5, col = 'darkgreen')
  abline(h = c(4.2, 8.3), lty = 9, col = 'grey'); grid(ny = NA)
  legend("topleft", lwd = 2, lty = c(6, 8, 5), legend = c("Antipoverty cap", "Antipoverty tax", "Income floor"), col = c("red", "blue", "darkgreen"))
  dev.off()
}


##### Maps (main text and appendix) #####
# Figure 2: antipoverty cap
cap_breaks <- c(0, 3, 8.3, 18.15, 40, 80, 400, Inf)
p$antipoverty_3_cap_average <- compute_antipoverty_cap(df = p, threshold = 3, growth = "average")
save_map("antipoverty_3_cap_average", breaks = cap_breaks, limits = c(0, Inf), sep = " to $", begin = "$", strict_ineq_lower = T,
         legend = paste0("Daily income\nabove which all\nshould be expropriated\nto lift everyone in the country\nabove $3/day\n(in 2021 PPP $)\n", growth_legend),
         save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .08, trim = T, colors = color(11, rev_color = FALSE)[c(1,3,7:11)])
p$antipoverty_3_cap_very_optimistic <- compute_antipoverty_cap(df = p, threshold = 3, growth = "very_optimistic")
save_map("antipoverty_3_cap_very_optimistic", breaks = cap_breaks, limits = c(0, Inf), sep = " to $", begin = "$", strict_ineq_lower = T,
         legend = paste0("Daily income\nabove which all\nshould be expropriated\nto lift everyone in the country\nabove $3/day\n(in 2021 PPP $)\n", growth_legend_7),
         save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .08, trim = T, colors = color(11, rev_color = FALSE)[c(1,3,7:11)])

# Figures 3-4 and appendix: antipoverty taxes
tax_map <- function(name, df, line, threshold, growth, legend_growth, hfce = FALSE) {
  p[[name]] <<- compute_antipoverty_tax(df = df, exemption_threshold = threshold, poverty_threshold = line, growth = growth)
  save_map(name, df = p, breaks = tax_breaks, sep = "% to ", end = "%",
           legend = paste0("Linear tax rate\nabove $", sprintf("%.2f", threshold), "/day\nrequired to lift everyone\nabove $", sprintf("%.2f", line), "/day\n(in 2021 PPP)\n", legend_growth, if (hfce) "\nHFCE-rescaled data." else ""),
           save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)
}
tax_map("antipoverty_3_tax_8_average", p, 3, 8.3, "average", growth_legend)                     # Figure 3
tax_map("antipoverty_3_tax_18_very_optimistic", p, 3, 18.15, "very_optimistic", growth_legend_7) # Figure 4
tax_map("s_antipoverty_3_tax_8_average", s, 3, 8.3, "average", growth_legend, hfce = T)
tax_map("antipoverty_3_tax_8_reg", p, 3, 8.3, "reg", "in 2030, after observed\ngrowth to 2025, then\nprojected growth.")
tax_map("antipoverty_3_tax_18_average", p, 3, 18.15, "average", growth_legend)
tax_map("s_antipoverty_3_tax_18_very_optimistic", s, 3, 18.15, "very_optimistic", growth_legend_7, hfce = T)
tax_map("antipoverty_4_tax_18_average", p, 4.2, 18.15, "average", growth_legend)
tax_map("antipoverty_8_tax_18_average", p, 8.3, 18.15, "average", growth_legend)
tax_map("antipoverty_8_tax_8_average", p, 8.3, 8.3, "average", growth_legend)
# 2035 horizon (new)
extend_to_2035 <- function(df, from, to, factor) {
  for (v in grep(paste0("^", from, "_(avg|max|min)_[0-9]+$"), names(df), value = T)) df[[sub(paste0("^", from), to, v)]] <- df[[v]] * factor
  df[[paste0("mean_", to)]] <- rowSums(df[, paste0(to, "_avg_", 1:100)] * df[, paste0("pop_share_", 1:100)])
  return(df)
}
p <- extend_to_2035(p, "Y3", "Y3_35", 1.03^5)
p <- extend_to_2035(p, "Y7", "Y7_35", 1.07^5)
tax_map("antipoverty_3_tax_8_average_2035", p, 3, 8.3, "Y3_35", "in 2035, after observed\ngrowth to 2025, then 3%.")
tax_map("antipoverty_3_tax_8_very_optimistic_2035", p, 3, 8.3, "Y7_35", "in 2035, after observed\ngrowth to 2025, then 7%.")

# Figure 5 and appendix: income floor
floor_breaks <- c(0, 2, 3, 4.2, 5.5, 8.3, 14, 25, 40, 100, Inf)
floor_map <- function(name, df, growth, legend_growth, hfce = FALSE) {
  p[[name]] <<- compute_income_floor(df = df, thresholds = 8.3, marginal_rates = 10, growth = growth, scope_tax = p)
  save_map(name, df = p, breaks = floor_breaks, begin = "$", sep = " to $", strict_ineq_lower = T,
           legend = paste0("Income floor\nthat can be funded\nwith a 10% tax\nabove $8.30/day\n(in 2021 PPP $/day)\n", legend_growth, if (hfce) "\nHFCE-rescaled data." else ""),
           save = T, rev_color = F, format = c('png', 'pdf'), legend_x = .055, trim = T)
}
floor_map("floor_8__10", p, "average", growth_legend)
floor_map("floor_8__10_very_optimistic", p, "very_optimistic", growth_legend_7)
floor_map("s_floor_8__10", s, "average", growth_legend, hfce = T)
floor_map("floor_8__10_2035", p, "Y3_35", "in 2035, after observed\ngrowth to 2025, then 3%.")
# Mean consumption in 2030 (new, used in the VoxDev column and the website)
save_map("mean_Y3", breaks = c(0, 3, 4.2, 8.3, 18.15, 40, 80, Inf), sep = " to $", begin = "$", strict_ineq_lower = T,
         legend = paste0("Mean consumption\n(in 2021 PPP $/day)\n", growth_legend), save = T, rev_color = F, format = c('png', 'pdf'), legend_x = .07, trim = T)


##### Appendix tables #####
unescape <- function(x) gsub("\\\\", "\\", x, fixed = T) # strings below are written with doubled backslashes (as in paper.R): collapse them
latex_table <- function(table, file, header, caption, caption.short, label, digits = 1, first_country = "Angola") {
  tex <- paste(kbl(table, "latex", caption = unescape(caption), caption.short = unescape(caption.short), position = "h", escape = F, booktabs = T, digits = digits,
                   linesep = rep("", nrow(table)-1), longtable = F, label = label, col.names = NULL), collapse = "\n")
  tex <- sub("\\toprule", paste0("\\toprule ", unescape(header), " \\\\ \n\\midrule"), tex, fixed = T)
  tex <- sub("\\centering", "\\centering\\footnotesize\n\\makebox[\\textwidth][c]{", tex, fixed = T)
  tex <- sub("\\end{tabular}", "\\end{tabular}}", tex, fixed = T)
  tex <- gsub("Inf", "$+\\infty$", tex, fixed = T)
  tex <- gsub("9999.0", "$>$ 10k", tex, fixed = T)
  tex <- sub(paste0("\n", first_country), paste0("\n\\midrule ", first_country), tex, fixed = T)
  cat(tex, file = paste0("../tables/update/", file, ".tex"))
}
regions_rows <- function(table, fun_region) rbind("World" = fun_region(p$country_code), "Low-Income Countries" = fun_region(LIC), "Sub-Saharan Africa" = fun_region(SSA), table)

# Mean income
table_income <- cbind(p$year, p$mean_welfare, p$mean_y_2022, p$mean_y_2025, p$mean_Y3, p$mean_Y7, p$mean_y_reg, p$mean_y, p$hfce/p$mean_welfare)
row.names(table_income) <- p$country_short
income_region <- function(codes) { r <- sapply(1:8, function(j) wtd.mean(table_income[,j], p$pop_2030 * (p$country_code %in% codes), na.rm = T)); c(r, mean(table_income[p$country_code %in% codes, 9], na.rm = T)) }
table_income <- regions_rows(table_income[selected_countries,], income_region)
latex_table(table_income, "income", digits = c(0, 1, 1, 1, 1, 1, 1, 1, 2),
  header = " & Year & \\\\multicolumn{7}{c}{Mean consumption/income (in \\\\$/day)} & HFCE to \\\\\\\\ \n & of & year of & 2022 & 2025 & \\\\multicolumn{4}{c}{2030 estimate} & survey \\\\\\\\ \n & survey & survey & est. & est. & 3\\\\% & 7\\\\% & Projection & Trend & ratio",
  caption = "Mean income in major lower-income countries in various years and growth scenarios, survey years and ratio of HFCE (national accounts) to survey mean (September 2026 PIP data, 2021 PPP).",
  caption.short = "Mean income in different scenarios, survey years and HFCE ratio (update).", label = "income_update")

# Growth and expected poverty
table_trend <- cbind(100 * p$mean_growth_gdp_pc_15_25, 100 * (p$growth_reg_annual - 1),
                     sapply(c(3, 4.2, 8.3), function(l) 100 * compute_poverty_rate(df = p, threshold = l, growth = "average")),
                     sapply(c(3, 4.2, 8.3), function(l) 100 * compute_poverty_gap(df = p, threshold = l, growth = "average", unit = "%")))
row.names(table_trend) <- p$country_short
trend_region <- function(codes) { pop <- p$pop_2030 * (p$country_code %in% codes); c(sapply(1:5, function(j) wtd.mean(table_trend[,j], pop, na.rm = T)), sapply(6:8, function(j) sum(table_trend[,j] * p$mean_Y3 * pop, na.rm = T) / sum((p$mean_Y3 * pop)[!is.na(table_trend[,j])], na.rm = T))) }
table_trend <- regions_rows(table_trend[selected_countries,], trend_region)
latex_table(table_trend, "trend", digits = c(1, 1, 0, 0, 0, 1, 1, 1),
  header = "\\\\makecell{\\\\\\\\Indicator} & \\\\makecell{Growth\\\\\\\\Trend} & \\\\makecell{Growth\\\\\\\\Autoregressive} & \\\\multicolumn{3}{c}{\\\\makecell{Poverty rate\\\\\\\\(in \\\\%)}} & \\\\multicolumn{3}{c}{\\\\makecell{Poverty gap\\\\\\\\(in \\\\% of mean income)}} \\\\\\\\ \n & 2015--2025 & Projection & \\\\$3.00 & \\\\$4.20 & \\\\$8.30 & \\\\$3.00 & \\\\$4.20 & \\\\$8.30",
  caption = "Expected poverty and growth in major lower-income countries: trend and projected annual growth rates, poverty rates and gaps in 2030 after observed growth to 2025, then 3\\\\% growth (2021 PPP).",
  caption.short = "Expected poverty and growth in 2030 in lower-income countries (update).", label = "trend_update")

# Antipoverty caps
table_cap <- create_appendix_table(fun = "compute_antipoverty_cap", thresholds = list(3, 3, 3, 3, 3, 3, "bcs", 4.8, 4.8), growths = c("average", "average", "very_optimistic", "very_optimistic", "reg", "reg", "average", "average", "BCL"), dfs = list(p, s, p, s, p, s, p, p, p))
latex_table(table_cap, "cap",
  header = "Poverty line (\\\\$/day) & 3.00 & 3.00 & 3.00 & 3.00 & 3.00 & 3.00 & BCS & 4.80 & 4.80 \\\\\\\\ \nGrowth scenario & 3\\\\% & 3\\\\% & 7\\\\% & 7\\\\% & \\\\multicolumn{2}{c}{Projection} & 3\\\\% & 3\\\\% & BCL \\\\\\\\ \nHFCE rescaling & & \\\\checkmark & & \\\\checkmark & & \\\\checkmark & & &",
  caption = "Antipoverty caps for major lower-income countries in 2030 (in 2021 PPP \\\\$/day). BCL: survey years of \\\\cite{bolch_arithmetics_2022}.",
  caption.short = "Antipoverty caps for major lower-income countries in 2030 (update).", label = "cap_update")

# Antipoverty taxes
tax_header <- "Taxation threshold (\\\\$/day) & 8.30 & 18.15 & 18.15 & 18.15 & 18.15 & 8.30 & 8.30 & 8.30 & 18.15 \\\\\\\\ \nGrowth scenario & 3\\\\% & 7\\\\% & 3\\\\% & Trend & \\\\multicolumn{2}{c}{Projection} & 7\\\\% & 3\\\\% & 7\\\\% \\\\\\\\ \nHFCE rescaling & & & & & & & & \\\\checkmark & \\\\checkmark"
for (L in list(c(3, "tax", "extreme", "3.00"), c(4.2, "tax4", "severe", "4.20"), c(8.3, "tax8", "acute", "8.30"))) {
  tab <- create_appendix_table(fun = "compute_antipoverty_tax", poverty_thresholds = as.numeric(L[1]), exemption_thresholds = c(8.3, 18.15, 18.15, 18.15, 18.15, 8.3, 8.3, 8.3, 18.15),
                               growths = c("average", "very_optimistic", "average", "trend", "reg", "reg", "very_optimistic", "average", "very_optimistic"), dfs = list(p, p, p, p, p, p, p, s, s))
  latex_table(pmin(tab, 9999), L[2], header = tax_header,
    caption = paste0("Antipoverty tax required to eliminate ", L[3], " poverty (at \\\\$", L[4], "/day) in major lower-income countries in 2030 (marginal rate in \\\\%)."),
    caption.short = paste0("Anti-", L[3], "-poverty taxes for major lower-income countries in 2030 (update)."), label = paste0(L[2], "_update"))
}

# Income floor
table_floor <- create_appendix_table(fun = "compute_income_floor", marginal_rates = list(10), thresholds = list(8.3),
                                     growths = c("average", "trend_ineq", "average", "reg", "reg", "very_optimistic", "very_optimistic"), dfs = list(p, p, s, p, s, p, s))
latex_table(table_floor, "floor_full",
  header = "Growth scenario & 3\\\\% & \\\\makecell{3\\\\%\\\\\\\\unbalanced} & 3\\\\% & \\\\multicolumn{2}{c}{Projection} & 7\\\\% & 7\\\\% \\\\\\\\ \nHFCE rescaling & & & \\\\checkmark & & \\\\checkmark & & \\\\checkmark",
  caption = "Income floor (in 2021 PPP \\\\$/day) financed by a 10\\\\% tax above \\\\$8.30/day for major lower-income countries in 2030.",
  caption.short = "Income floor financed by a 10\\\\% tax above \\\\$8.30/day (update).", label = "floor_update")

# Net gain of global taxes above $100/day
rate_close <- round(as.numeric(sub(".*\\{(.*)\\}", "\\1", grep("upRateHundred\\}", readLines("../tables/update_numbers.tex"), value = T))), 1) # rate closing the extreme poverty gap (survey data)
rates <- c(rate_close, rate_close, 0.3, 0.3, 10, 10)
dfs_gain <- list(p, s, p, s, p, s); ws_gain <- list(w, ws, w, ws, w, ws)
gain_cols <- revenues_gain <- transfer_gain <- gini_gain <- floor_gain <- list()
for (j in 1:6) {
  name <- paste0("100__", j)
  d <- tax_revenues(df = dfs_gain[[j]], thresholds = 100, marginal_rates = rates[j], return = 'df', growth = "average", name_tax = name, scope_tax = ws_gain[[j]])
  gain_cols[[j]] <- d[[paste0("gain_Y3_tax_", name)]]
  pops <- dfs_gain[[j]]$pop_2030
  revenues_gain[[j]] <- tax_revenues(df = ws_gain[[j]], thresholds = 100, marginal_rates = rates[j], return = '%', growth = "average", scope_tax = ws_gain[[j]])
  transfer_gain[[j]] <- sum(pmax(0, d[[paste0("mean_Y3_tax_", name)]] - d$mean_Y3) * pops, na.rm = T) / sum(d$mean_Y3 * pops, na.rm = T)
  wt <- tax_revenues(df = ws_gain[[j]], thresholds = 100, marginal_rates = rates[j], return = 'df', growth = "average", name_tax = name, scope_tax = ws_gain[[j]])
  gini_gain[[j]] <- compute_inequality(df = wt, var = paste0("Y3_tax_", name), return = 'gini')
  floor_gain[[j]] <- compute_income_floor(df = ws_gain[[j]], thresholds = 100, marginal_rates = rates[j], growth = 'average', scope_tax = ws_gain[[j]])
}
table_gain <- do.call(cbind, gain_cols)
row.names(table_gain) <- p$country_short
gain_region <- function(codes) sapply(1:6, function(j) { pop <- p$pop_2030 * (p$country_code %in% codes); sum(table_gain[,j] * p$mean_Y3 * pop, na.rm = T) / sum(p$mean_Y3 * pop, na.rm = T) })
table_gain <- table_gain[order(p$country)[order(p$country) %in% which(p$pop_2030 > 40.5e6) & !duplicated(p$country)],]
table_gain <- rbind("Revenues (\\\\% global income)" = unlist(revenues_gain), "International transfers" = unlist(transfer_gain), "Income floor (\\\\$/day)" = unlist(floor_gain)/100, "Gini" = unlist(gini_gain)/100,
                    "Low-Income Countries" = gain_region(LIC), "Sub-Saharan Africa" = gain_region(SSA), table_gain)
row.names(table_gain) <- unescape(row.names(table_gain))
cat(sub("Low-Income", "\\\\midrule Low-Income", sub("Algeria", "\\\\midrule Algeria",
    sub("toprule", paste0("toprule Tax rate & ", rates[1], "\\\\% & ", rates[2], "\\\\% & 0.3\\\\% & 0.3\\\\% & 10\\\\% & 10\\\\% \\\\\\\\ \nHFCE rescaling &  & \\\\checkmark &  & \\\\checkmark & & \\\\checkmark \\\\\\\\ \n \\\\midrule"),
    paste(sub("\\centering", "\\centering\\footnotesize", kbl(100 * table_gain, "latex", caption = unescape(paste0("Net gain per country of a global antipoverty tax above \\\\$100/day, for most populous countries in 2030 after observed growth to 2025, then 3\\\\% growth (in \\\\% of national income, 2021 PPP). The ", rates[1], "\\\\% rate closes the global \\\\$3.00/day poverty gap with survey data.")),
    digits = 2, linesep = rep("", nrow(table_gain)-1), longtable = F, label = "gain_update", position = "h", escape = F, booktabs = T,
    caption.short = "Net gain per country of global antipoverty taxes (update).", col.names = NULL), fixed = T), collapse = "\n")))), file = "../tables/update/gain.tex")

# Numbers quoted in update_appendix.tex captions
cat(paste0("% Generated by code_poverty/paper_update.R on ", Sys.Date(), "\n",
           "\\newcommand{\\appKenyaCap}{", format(round(cap_k, 1), nsmall = 1), "}\n",
           "\\newcommand{\\appKenyaTax}{", round(tax_k), "}\n",
           "\\newcommand{\\appKenyaFloor}{", format(round(floor_k, 1), nsmall = 1), "}\n"), file = "../tables/update/appendix_numbers.tex")
print(kenya_numbers)
save(p, s, w, ws, ssa, lic, ssas, lics, LIC, SSA, file = ".RData_paper_update")
