# For figures, use a plot window of 1300 x 630 px.

##### Data #####
p$scaling_factor[p$country %in% c("Burundi", "Democratic Republic of the Congo")]
mean(p$year < 2022 & p$year > 2017)
mean(p$hfce/p$mean_welfare, na.rm = T) # 1.44 (20% NA)

#####  Balanced growth ##### 
growth_scenarios <- setNames(c("now", "trend", "trend_pos", "imf", "reg", "none", "average", "strong", "optimistic", "very_optimistic", "sdg8", "trend_ineq"), # , "bolch"
                             c("2022 Estimate", "Trend (2014--2019)", "Max(Trend, 0)", "IMF forecast", "Autoregressive projection", "0\\% growth", "3\\% growth", "4.5\\% growth", "6\\% growth", "7\\% growth", "7\\% growth since 2016", "Unbalanced growth (inequality trend + 3\\% growth)")) # Quadratic model
table_poverty <- cbind(#"scenario" = names(growth_scenarios), 
  "rate2" = 100*sapply(growth_scenarios, function(s) compute_poverty_rate(df = w, threshold = 2.15, growth = s, return = "rate")), 
  "rate4" = 100*sapply(growth_scenarios, function(s) compute_poverty_rate(df = w, threshold = 3.65, growth = s, return = "rate")), 
  "rate7" = 100*sapply(growth_scenarios, function(s) compute_poverty_rate(df = w, threshold = 6.85, growth = s, return = "rate")), 
  "rate18" = 100*sapply(growth_scenarios, function(s) compute_poverty_rate(df = w, threshold = 18.15, growth = s, return = "rate")), 
  "gap2" = 100*sapply(growth_scenarios, function(s) compute_poverty_gap(df = w, threshold = 2.15, unit = '%', growth = s)), 
  "gap4" = 100*sapply(growth_scenarios, function(s) compute_poverty_gap(df = w, threshold = 3.65, unit = '%', growth = s)), 
  "gap7" = 100*sapply(growth_scenarios, function(s) compute_poverty_gap(df = w, threshold = 6.85, unit = '%', growth = s)),
  "gap18" = 100*sapply(growth_scenarios, function(s) compute_poverty_gap(df = w, threshold = 18.15, unit = '%', growth = s))) 
cat(sub("\\toprule\n", "\\toprule Growth scenario & \\multicolumn{4}{c}{Poverty rate (\\%)} & \\multicolumn{4}{c}{Poverty gap (\\% of GDP)} \\\\ \n (Poverty line in \\$/day)", 
        paste(kbl(table_poverty[c(1, 2, 3, 5, 7, 10, 11), ], "latex", 
      caption = "Global poverty rates and poverty gaps in 2030 under different growth scenarios. Poverty rates are expressed in \\% of world population and poverty gaps in \\% of world GDP. Poverty lines are in PPP \\$/day.", 
      row.names = T, position = "h", escape = F, booktabs = T, digits = c(1, 1, 1, 1, 2, 2, 2, 2), label = "poverty", linesep = rep("", nrow(table_poverty)-1), 
      caption.short = "Global poverty (rates and gaps) in 2030 under different growth scenarios.",
      col.names = c("2.15", "3.65", "6.85", "18.15", "2.15", "3.65", "6.85", "18.15")), collapse="\n"), fixed = T), file = "../tables/poverty.tex") 


#####  Antipoverty cap ##### 
p$y_expropriated_2_average <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "average")
plot_world_map("y_expropriated_2_average", breaks = c(0, 2.15, 7, 13, 30, 60, 300, Inf), sep = " to ", end = "", strict_ineq_lower = T, limits = c(0, Inf),
               legend = "Daily income\nabove which all\nshould be expropriated\nto lift everyone in the country\nabove $2.15/day\n(in $ 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .08, trim = T, colors = color(11, rev_color = FALSE)[c(1,3,7:11)])  
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
               legend = "Daily income\nabove which all\nshould be expropriated\nto lift everyone in the country\nabove $2.15/day\n(in $ 2017 PPP)\nin 2030, after 7%\ngrowth since 2022.", 
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .08, trim = T)  

p$s_y_expropriated_2_average <- compute_antipoverty_maximum(df = s, threshold = 2.15, growth = "average")
plot_world_map("s_y_expropriated_2_average", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, 
               legend = "Daily income\nabove which all\nshould be expropriated\nto lift everyone in the country\nabove $2.15/day\n(in $ 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .08, trim = T)  

# Antipoverty taxes
p$antipoverty_2_tax_7_average <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "average")
plot_world_map("antipoverty_2_tax_7_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift everyone\nabove $2.15/day\n(in 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
sort(setNames(p$antipoverty_2_tax_7_average, p$country), decreasing = T)
wtd.mean(p$antipoverty_2_tax_7_average[p$country_code %in% SSA], p$pop_2030[p$country_code %in% SSA]) # 49%
wtd.mean(p$antipoverty_2_tax_7_average[p$country_code %in% LIC], p$pop_2030[p$country_code %in% LIC]) # 64%
sum(p$antipoverty_2_tax_7_average > 100) # 5

p$antipoverty_2_tax_18_very_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "very_optimistic")
plot_world_map("antipoverty_2_tax_18_very_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18/day\nrequired to lift everyone\nabove $2.15/day\n(in 2017 PPP)\nin 2030, after 7%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
sort(setNames(p$antipoverty_2_tax_18_very_optimistic, p$country), decreasing = T)

p$antipoverty_4_tax_4_bolch <- compute_antipoverty_tax(df = p, exemption_threshold = 3.44, poverty_threshold = 3.44, growth = "bolch")
sum(p$antipoverty_4_tax_4_bolch > 100, na.rm = T) # 30
p$antipoverty_4_tax_22_bolch <- compute_antipoverty_tax(df = p, exemption_threshold = 22.36, poverty_threshold = 3.44, growth = "bolch")
sum(p$antipoverty_4_tax_22_bolch > 100, na.rm = T) # 52
p$antipoverty_4_tax_18_now <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 3.44, growth = "now")
p$antipoverty_4_tax_22_average <- compute_antipoverty_tax(df = p, exemption_threshold = 22.36, poverty_threshold = 3.44, growth = "average")
sum(p$antipoverty_4_tax_22_average > 100) # 34
p$antipoverty_4_tax_4_average <- compute_antipoverty_tax(df = p, exemption_threshold = 3.44, poverty_threshold = 3.44, growth = "average")
sum(p$antipoverty_4_tax_4_average > 100) # 6

p$antipoverty_2_tax_18_trend <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "trend")
p$mean_growth_gdp_pc_14_19[p$country == "India"] # 5.5%
p$antipoverty_2_tax_18_very_optimistic[p$country == "India"] # 10%
p$antipoverty_2_tax_18_trend[p$country == "India"] # 36%
p$antipoverty_2_tax_18_average[p$country == "India"] # 156%

p$antipoverty_2_tax_18_average <- compute_antipoverty_tax(df = p, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "average")
plot_world_map("antipoverty_2_tax_18_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18/day\nrequired to lift everyone\nabove $2.15/day\n(in 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
sort(setNames(p$antipoverty_2_tax_18_very_optimistic, p$country), decreasing = T)

p$s_antipoverty_2_tax_7_average <- compute_antipoverty_tax(df = s, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "average")
plot_world_map("s_antipoverty_2_tax_7_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift everyone\nabove $2.15/day\n(in 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  
p$s_antipoverty_2_tax_18_very_optimistic <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 2.15, growth = "very_optimistic")
plot_world_map("s_antipoverty_2_tax_18_very_optimistic", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18/day\nrequired to lift everyone\nabove $2.15/day\n(in 2017 PPP)\nin 2030, after 7%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  

sort(setNames(p$bcs, p$country), decreasing = T)
p$antipoverty_bcs_tax_bcs <- compute_antipoverty_tax(df = p11, exemption_threshold = "bcs", poverty_threshold = "bcs", growth = "average") # In 2011PPP, 2.15/3.65/6.85 is 1.9/3.2/5.5 https://documents1.worldbank.org/curated/en/099700509122212929/pdf/IDU05b43a261041c504a5f0bb3405d0ef310b9e1.pdf
package("spatstat")
weighted.median(p$bcs, p$pop_2030, na.rm = T) # 4.35
sum(p$antipoverty_bcs_tax_bcs > 100, na.rm = T) # 15
sum(p$antipoverty_bcs_tax_bcs > 100, na.rm = T)
sort(setNames(p$antipoverty_bcs_tax_bcs, p$country), decreasing = T)
plot_world_map("antipoverty_bcs_tax_bcs", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove Basic Consumption\nrequired to lift everyone\nabove Basic Consumption\n(in 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  


##### Demogrant (income floor) for a given tax ##### 
p$demogrant_7__10 <- compute_income_floor(df = p, thresholds = 6.85, marginal_rates = 10, growth = "average", scope_tax = p)
sort(setNames(p$demogrant_7__10, p$country))
sort(setNames(p$demogrant_7__10, p$country)[p$country_code %in% LIC])
sum(p$demogrant_7__10 < 2.15) # 23 TODO! check
length(LIC) # 27
sum(p$demogrant_7__10 < 2.15 & p$country_code %in% LIC) # 13
plot_world_map("demogrant_7__10", breaks = c(0, 1.5, 2.15, 3, 4, 7, 10, 18, 30, 70, Inf), end = "$", sep = "$ to ",
               legend = "Income floor\nthat can be funded\nwith a 10% tax\nabove $6.85/day\n(in 2017 PPP $/day)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = F, format = c('png', 'pdf'), legend_x = .055,  trim = T)  

p$demogrant_7__10_very_optimistic <- compute_income_floor(df = p, thresholds = 6.85, marginal_rates = 10, growth = "very_optimistic", scope_tax = p)
sum(p$demogrant_7__10_very_optimistic < 2.15)
p$s_demogrant_7__10_very_optimistic <- compute_income_floor(df = s, thresholds = 6.85, marginal_rates = 10, growth = "very_optimistic", scope_tax = p)
sum(p$s_demogrant_7__10_very_optimistic < 2.15)
plot_world_map("demogrant_7__10_very_optimistic", breaks = c(0, 1.5, 2.15, 3, 4, 7, 10, 18, 30, 70, Inf), end = "$", sep = "$ to ",
               legend = "Income floor\nthat can be funded\nwith a 5% tax\nabove $6.85/day\n(in 2017 PPP $/day)\nin 2030, after 7%\ngrowth since 2022.", 
               save = T, rev_color = F, format = c('png', 'pdf'), legend_x = .055, trim = T)  
p$demogrant_7__10_sdg8 <- compute_income_floor(df = p, thresholds = 6.85, marginal_rates = 10, growth = "sdg8", scope_tax = p)
sum(p$demogrant_7__10_sdg8 < 2.15)
sort(setNames(p$demogrant_7__10_sdg8, p$country), decreasing = T)

p$antipoverty_2_tax_7_sdg8 <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "sdg8")
sort(setNames(p$antipoverty_2_tax_7_sdg8, p$country))
plot_world_map("antipoverty_2_tax_7_sdg8", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)\nin 2030, after 7%\ngrowth since 2016.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  


##### Potential of global redistribution #####
compute_income_floor(df = w, thresholds = 6.85, marginal_rates = 10, growth = 'average', name_tax = "7__10", scope_tax = w) # 8.6

compute_poverty_rate(df = ws, threshold = 100, growth = 'average') # 95%

p <- tax_revenues(df = p, thresholds = 100, marginal_rates = 1.2, return = 'df', growth = "average", name_tax = "100__1", scope_tax = w) # .14% closes 2.15$ pg
s <- tax_revenues(df = s, thresholds = 100, marginal_rates = 1.2, return = 'df', growth = "average", name_tax = "100__1", scope_tax = ws) # .31% 
p <- tax_revenues(df = p, thresholds = 100, marginal_rates = 0.3, return = 'df', growth = "average", name_tax = "100__03", scope_tax = w) # int'l transfer: .04%
s <- tax_revenues(df = s, thresholds = 100, marginal_rates = 0.3, return = 'df', growth = "average", name_tax = "100__03", scope_tax = ws) # .09% closes 2.15$ pg
p <- tax_revenues(df = p, thresholds = 100, marginal_rates = 10, return = 'df', growth = "average", name_tax = "100__10", scope_tax = w) # 1.2%
s <- tax_revenues(df = s, thresholds = 100, marginal_rates = 10, return = 'df', growth = "average", name_tax = "100__10", scope_tax = ws) # 2.3% closes 6.85$ pg

w <- tax_revenues(df = w, thresholds = 100, marginal_rates = 1.2, return = 'df', growth = "average", name_tax = "100__1", scope_tax = w) # .15% closes 2.15$ pg
ws <- tax_revenues(df = ws, thresholds = 100, marginal_rates = 1.2, return = 'df', growth = "average", name_tax = "100__1", scope_tax = ws) # .41% 
w <- tax_revenues(df = w, thresholds = 100, marginal_rates = 0.3, return = 'df', growth = "average", name_tax = "100__03", scope_tax = w) # int'l transfer: .04%
ws <- tax_revenues(df = ws, thresholds = 100, marginal_rates = 0.3, return = 'df', growth = "average", name_tax = "100__03", scope_tax = ws) # .1% closes 2.15$ pg
w <- tax_revenues(df = w, thresholds = 100, marginal_rates = 10, return = 'df', growth = "average", name_tax = "100__10", scope_tax = w) # 1.2%
ws <- tax_revenues(df = ws, thresholds = 100, marginal_rates = 10, return = 'df', growth = "average", name_tax = "100__10", scope_tax = ws) # 3.4% closes 6.85$ pg

revenues_gain <- c(.0015, .0041, .0004, .001, 0.012, 0.0342)
transfer_gain <- c(.0014, .0031, .0004, .0009, 0.0117, 0.0232)
gini_gain <- c(compute_inequality(df = w, var = "Y3_tax_100__1", return = 'gini'),
               compute_inequality(df = ws, var = "Y3_tax_100__1", return = 'gini'),
               compute_inequality(df = w, var = "Y3_tax_100__03", return = 'gini'),
               compute_inequality(df = ws, var = "Y3_tax_100__03", return = 'gini'),
               compute_inequality(df = w, var = "Y3_tax_100__10", return = 'gini'),
               compute_inequality(df = ws, var = "Y3_tax_100__10", return = 'gini'))
floor_gain <- c(compute_income_floor(df = w, thresholds = 100, marginal_rates = 1.2, growth = 'average', scope_tax = w),
                compute_income_floor(df = ws, thresholds = 100, marginal_rates = 1.2, growth = 'average', scope_tax = w),
                compute_income_floor(df = w, thresholds = 100, marginal_rates = 0.3, growth = 'average', scope_tax = w),
                compute_income_floor(df = ws, thresholds = 100, marginal_rates = 0.3, growth = 'average', scope_tax = w),
                compute_income_floor(df = w, thresholds = 100, marginal_rates = 10, growth = 'average', scope_tax = w),
                compute_income_floor(df = ws, thresholds = 100, marginal_rates = 10, growth = 'average', scope_tax = w))


##### Appendix figures ##### 
### Figures already plotted above:
## Main text figures:
# y_expropriated_2_average
# antipoverty_2_tax_7_average
# antipoverty_2_tax_18_very_optimistic
# demogrant_7__10

## Appendix figures:
# s_antipoverty_2_tax_7_average
# antipoverty_2_tax_18_average
# s_antipoverty_2_tax_18_very_optimistic
# demogrant_7__10_very_optimistic
# s_demogrant_7__10

### New Appendix figures:
# antipoverty_2_tax_7_reg
# antipoverty_4_tax_18_average
# antipoverty_7_tax_18_average
# antipoverty_7_tax_7_average
p$antipoverty_2_tax_7_reg <- compute_antipoverty_tax(df = s, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "reg")
plot_world_map("antipoverty_2_tax_7_reg", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $6.85/day\nrequired to lift everyone\nabove $2.15/day\n(in 2017 PPP)\nin 2030, after\npredicted growth.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  

p$antipoverty_4_tax_18_average <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 3.65, growth = "average")
plot_world_map("antipoverty_4_tax_18_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18.15/day\nrequired to lift everyone\nabove $3.65/day\n(in 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  

p$antipoverty_7_tax_18_average <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 6.85, growth = "average")
plot_world_map("antipoverty_7_tax_18_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18.15/day\nrequired to lift everyone\nabove $6.85/day\n(in 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  

p$antipoverty_7_tax_7_average <- compute_antipoverty_tax(df = s, exemption_threshold = 18.15, poverty_threshold = 6.85, growth = "average")
plot_world_map("antipoverty_7_tax_7_average", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               legend = "Linear tax rate\nabove $18.15/day\nrequired to lift everyone\nabove $6.85/day\n(in 2017 PPP)\nin 2030, after 3%\ngrowth since 2022.", 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)  



##### Appendix tables ##### 
selected_countries <- order(p$country)[order(p$country) %in% which(p$pop_2022 > 1e7 & no.na(p$mean_welfare, 0, num_as_char = FALSE) < 6)]
p$country_short <- p$country
p$country_short[p$country == "Democratic Republic of the Congo"] <- "D.R. Congo"

table_income <- cbind(p$year, p$mean_welfare, p$mean_y_2022, p$mean_Y3, p$mean_Y7, p$mean_y_reg, p$mean_y, p$bolch_year_original, p$hfce/p$mean_welfare) 
world_income <- lic_income <- ssa_income <- table_income[1,]
for (j in 1:8) world_income[j] <- wtd.mean(table_income[,j], p$pop_2030, na.rm = T)
for (j in 1:8) lic_income[j] <- wtd.mean(table_income[,j], p$pop_2030 * p$country_code %in% LIC, na.rm = T)
for (j in 1:8) ssa_income[j] <- wtd.mean(table_income[,j], p$pop_2030 * p$country_code %in% SSA, na.rm = T)
world_income[9] <- mean(table_income[,9], na.rm = T)
lic_income[9] <- wtd.mean(table_income[,9], p$country_code %in% LIC, na.rm = T)
ssa_income[9] <- wtd.mean(table_income[,9], p$country_code %in% SSA, na.rm = T)
row.names(table_income) <- p$country_short
(table_income <- table_income[selected_countries,]) # [order(p$country)[order(p$country) %in% which(p$pop_2022 > 35e6) & !duplicated(p$country)],]) # 
table_income <- rbind("World" = world_income, "Low-Income Countries" = lic_income, "Sub-Saharan Africa" = ssa_income, table_income)
cat(sub("Angola", "\\\\midrule Angola", 
    sub("toprule", "toprule  & Year & \\\\multicolumn{6}{c}{Mean consumption/income (in \\\\$/day)} & BCL & HFCE to  \\\\\\\\ \n Indicator & of & year of & 2022 & \\\\multicolumn{4}{c}{2030 estimate} & survey & survey \\\\\\\\ \n & Survey & survey & est. & 3\\\\% & 7\\\\% & Projection & Trend & year & ratio \\\\\\\\ \n\\\\midrule", # 
    sub("end{tabular}", "end{tabular}}", sub("centering", "makebox[\\\\textwidth][c]{", paste(kbl(table_income, "latex", 
    caption = "Mean income in major lower-income countries in various years and growth scenarios, survey years and factor used to rescale incomes to national accounts (in countries with HFCE to survey ratio above 1).", 
    position = "b", align = "c", escape = F, booktabs = T, digits = c(0, 1, 1, 1, 1, 1, 1, 0, 2), linesep = rep("", nrow(table_income)-1), longtable = F, label = "tax", 
    caption.short = "Mean income in different scenarios, survey years and HFCE rescaling factor.", col.names = NULL), collapse="\n")), fixed = T))), file = "../tables/income.tex")  


table_trend <- cbind("trend" = 100 * ((p$gdp_pc_2019/p$gdp_pc_2014)^(1/5)-1), # p$mean_growth_gdp_pc_14_19, 
                     "reg" = 100 * (p$gdp_pc_2030_reg/p$gdp_pc_2022)^(1/8) - 100, #"scaling_factor" = p$hfce/p$mean_welfare, # p$scaling_factor,
                     "poverty_rate_2" = 100 * compute_poverty_rate(df = p, threshold = 2.15, growth = "average"), 
                     "poverty_rate_4" = 100 * compute_poverty_rate(df = p, threshold = 3.65, growth = "average"), 
                     "poverty_rate_7" = 100 * compute_poverty_rate(df = p, threshold = 6.85, growth = "average"),
                     "poverty_gap_2" = 100 * compute_poverty_gap(df = p, threshold = 2.15, growth = "average", unit = "%"), 
                     "poverty_gap_4" = 100 * compute_poverty_gap(df = p, threshold = 3.65, growth = "average", unit = "%"), 
                     "poverty_gap_7" = 100 * compute_poverty_gap(df = p, threshold = 6.85, growth = "average", unit = "%"))
world_trend <- lic_trend <- ssa_trend <- table_trend[1,]
for (j in 1:5) world_trend[j] <- wtd.mean(table_trend[,j], p$pop_2030, na.rm = T)
for (j in 1:5) lic_trend[j] <- wtd.mean(table_trend[,j], p$pop_2030 * p$country_code %in% LIC, na.rm = T)
for (j in 1:5) ssa_trend[j] <- wtd.mean(table_trend[,j], p$pop_2030 * p$country_code %in% SSA, na.rm = T)
for (j in 6:8) world_trend[j] <- wtd.mean(table_trend[,j] * p$mean_Y3, p$pop_2030) / w$mean_Y3
for (j in 6:8) lic_trend[j] <- wtd.mean(table_trend[,j] * p$mean_Y3, p$pop_2030 * p$country_code %in% LIC, na.rm = T) / lic$mean_Y3
for (j in 6:8) ssa_trend[j] <- wtd.mean(table_trend[,j] * p$mean_Y3, p$pop_2030 * p$country_code %in% SSA, na.rm = T) / ssa$mean_Y3
row.names(table_trend) <- p$country_short
(table_trend <- table_trend[selected_countries,]) # [order(p$country)[order(p$country) %in% which(p$pop_2022 > 35e6) & !duplicated(p$country)],])
table_trend <- rbind("World" = world_trend, "Low-Income Countries" = lic_trend, "Sub-Saharan Africa" = ssa_trend, table_trend)
cat(sub("Angola", "\\\\midrule Angola", 
    sub("toprule", "toprule \\\\makecell{\\\\\\\\Indicator} & \\\\makecell{Growth\\\\\\\\Trend} & \\\\makecell{Growth\\\\\\\\Autoregressive} & \\\\multicolumn{3}{c}{\\\\makecell{Poverty rate\\\\\\\\(in \\\\%)}} & \\\\multicolumn{3}{c}{\\\\makecell{Poverty gap\\\\\\\\(in \\\\% of mean income)}}  \\\\\\\\ \n& 2014--2019 & Projection & \\\\$2.15 & \\\\$3.65 & \\\\$6.85 & \\\\$2.15 & \\\\$3.65 & \\\\$6.85 \\\\\\\\ \n\\\\midrule", # 
    sub("end{tabular}", "end{tabular}}", sub("centering", "makebox[\\\\textwidth][c]{", paste(kbl(table_trend, "latex", 
    caption = "Expected poverty and growth in major lower-income countries: trend and projected growth rate, poverty rates and gaps at \\$2.15 and \\$6.85/day in 2030 after 3\\%\ngrowth since 2022.", 
    position = "b", align = "c", escape = F, booktabs = T, digits = c(1, 1, 0, 0, 0, 1, 1, 1), linesep = rep("", nrow(table_trend)-1), longtable = F, label = "tax", 
    caption.short = "Expected poverty and growth in 2030 in lower-income countries.", col.names = NULL), collapse="\n")), fixed = T))), file = "../tables/trend.tex")  


(table_cap <- create_appendix_table(fun = "compute_antipoverty_maximum", thresholds = list(2.15, 2.15, 2.15, 2.15, 2.15, 2.15, "bcs", 3.44, 3.44), growths = c("average", "average", "very_optimistic", "very_optimistic", "reg", "reg", "average", "average", "bolch"), dfs = list(p, s, p, s, p, s, p, p, p)))
cat(sub("Angola", "\\\\midrule Angola", 
    sub("toprule", "toprule Poverty line (\\\\$/day) & 2.15 & 2.15 & 2.15 & 2.15 & 2.15 & 2.15 & BCS & 3.44 & 3.44 \\\\\\\\ \nGrowth scenario & 3\\\\% & 3\\\\% & 7\\\\% & 7\\\\% & \\\\multicolumn{2}{c}{Projection} & 3\\\\% & 3\\\\% & BCL \\\\\\\\ \nHFCE rescaling & & \\\\checkmark & & \\\\checkmark & & \\\\checkmark & & & \\\\\\\\ \n \\\\midrule", 
    gsub("Inf", "$+\\\\infty$", sub("end{tabular}", "end{tabular}}", sub("centering", "makebox[\\textwidth][c]{", paste(kbl(table_cap, "latex", 
    caption = "Antipoverty caps for major lower-income countries in 2030.", 
    position = "b", escape = F, booktabs = T, digits = 1, linesep = rep("", nrow(table_cap)-1), longtable = F, label = "cap",
    col.names = NULL), collapse="\n"), fixed = T), fixed = T)))), file = "../tables/cap.tex")  #


(table_tax <- create_appendix_table(fun = "compute_antipoverty_tax", poverty_thresholds = 2.15, exemption_thresholds = c(6.85, 18.15, 18.15, 18.15, 18.15, 6.85, 6.85, 6.85, 18.15), 
                                    growths = c("average", "very_optimistic", "average", "trend", "reg", "reg", "very_optimistic", "average", "very_optimistic"), 
                                    dfs = list(p, p, p, p, p, p, p, s, s)))
cat(sub("Angola", "\\\\midrule Angola", gsub("9999.0", "$>$ 10k", 
    sub("toprule", "toprule Taxation threshold (\\\\$/day) & 6.85 & 18.15 & 18.15 & 18.15 & 18.15 & 6.85 & 6.85 & 6.85 & 18.15 \\\\\\\\ \nGrowth scenario & 3\\\\% & 7\\\\% & 3\\\\% & Trend & \\\\multicolumn{2}{c}{Projection} & 7\\\\% & 3\\\\% & 7\\\\% \\\\\\\\ \nHFCE rescaling & & & & & & & & \\\\checkmark & \\\\checkmark \\\\\\\\ \n \\\\midrule", 
    sub("end{tabular}", "end{tabular}}", sub("centering", "makebox[\\\\textwidth][c]{", paste(kbl(pmin(table_tax, 9999), "latex", 
    caption = "Antipoverty tax required to eliminate extreme poverty (at \\$2.15/day) in major lower-income countries in 2030 (marginal rate in \\%).", 
    caption.short = "Anti-extrme-poverty taxes for major lower-income countries in 2030.", position = "b", escape = F, booktabs = T, 
    digits = 1, linesep = rep("", nrow(table_tax)-1), longtable = F, label = "tax",
    col.names = NULL), collapse="\n")), fixed = T)), fixed = T)), file = "../tables/tax.tex")  # 


(table_tax4 <- create_appendix_table(fun = "compute_antipoverty_tax", poverty_thresholds = 3.65, exemption_thresholds = c(6.85, 18.15, 18.15, 18.15, 18.15, 6.85, 6.85, 6.85, 18.15), 
                                     growths = c("average", "very_optimistic", "average", "trend", "reg", "reg", "very_optimistic", "average", "very_optimistic"), 
                                     dfs = list(p, p, p, p, p, p, p, s, s)))
cat(sub("Angola", "\\\\midrule Angola", gsub("9999.0", "$>$ 10k", 
    sub("toprule", "toprule Taxation threshold (\\\\$/day) & 6.85 & 18.15 & 18.15 & 18.15 & 18.15 & 6.85 & 6.85 & 6.85 & 18.15 \\\\\\\\ \nGrowth scenario & 3\\\\% & 7\\\\% & 3\\\\% & Trend & \\\\multicolumn{2}{c}{Projection} & 7\\\\% & 3\\\\% & 7\\\\% \\\\\\\\ \nHFCE rescaling & & & & & & & & \\\\checkmark & \\\\checkmark \\\\\\\\ \n \\\\midrule", 
    sub("end{tabular}", "end{tabular}}", sub("centering", "makebox[\\\\textwidth][c]{", paste(kbl(pmin(table_tax4, 9999), "latex", caption = "Antipoverty tax required to eliminate severe poverty (at \\$3.65/day) in major lower-income countries in 2030 (marginal rate in \\%).", 
    caption.short = "Anti-severe-poverty taxes for major lower-income countries in 2030.", position = "b", escape = F, booktabs = T, 
    digits = 1, linesep = rep("", nrow(table_tax4)-1), longtable = F, label = "tax4",
    col.names = NULL), collapse="\n")), fixed = T)), fixed = T)), file = "../tables/tax4.tex")  # 


(table_tax7 <- create_appendix_table(fun = "compute_antipoverty_tax", poverty_thresholds = 6.85, exemption_thresholds = c(6.85, 18.15, 18.15, 18.15, 18.15, 6.85, 6.85, 6.85, 18.15), 
                                     growths = c("average", "very_optimistic", "average", "trend", "reg", "reg", "very_optimistic", "average", "very_optimistic"), 
                                     dfs = list(p, p, p, p, p, p, p, s, s)))
cat(sub("Angola", "\\\\midrule Angola", gsub("9999.0", "$>$ 10k", 
    sub("toprule", "toprule Taxation threshold (\\\\$/day) & 6.85 & 18.15 & 18.15 & 18.15 & 18.15 & 6.85 & 6.85 & 6.85 & 18.15 \\\\\\\\ \nGrowth scenario & 3\\\\% & 7\\\\% & 3\\\\% & Trend & \\\\multicolumn{2}{c}{Projection} & 7\\\\% & 3\\\\% & 7\\\\% \\\\\\\\ \nHFCE rescaling & & & & & & & & \\\\checkmark & \\\\checkmark \\\\\\\\ \n \\\\midrule", 
    sub("end{tabular}", "end{tabular}}", sub("centering", "makebox[\\\\textwidth][c]{", paste(kbl(pmin(table_tax7, 9999), "latex", 
    caption = "Antipoverty tax required to eliminate acute poverty (at \\$6.85/day) in major lower-income countries in 2030 (marginal rate in \\%).", 
    caption.short = "Anti-acute-poverty taxes for major lower-income countries in 2030.", 
    position = "b", escape = F, booktabs = T, digits = 1, linesep = rep("", nrow(table_tax7)-1), longtable = F, label = "tax7",
    col.names = NULL), collapse="\n")), fixed = T)), fixed = T)), file = "../tables/tax7.tex")  # 


(table_floor <- create_appendix_table(fun = "compute_income_floor", marginal_rates = list(10), thresholds = list(6.85), 
                                      growths = c("average", "average", "reg", "reg", "very_optimistic", "very_optimistic", "sdg8"), dfs = list(p, s, p, s, p, s, p)))
cat(sub("Angola", "\\\\midrule Angola", 
    sub("toprule", "toprule Growth scenario over 2022--2030 & 3\\\\% & 3\\\\% & \\\\multicolumn{2}{c}{Projection} & 7\\\\% & 7\\\\% & \\\\makecell{7\\\\% since \\\\\\\\2015} \\\\\\\\ \nHFCE rescaling & & \\\\checkmark & & \\\\checkmark & & \\\\checkmark & \\\\\\\\ \n \\\\midrule", 
    sub("end{tabular}", "end{tabular}}", sub("centering", "makebox[\\\\textwidth][c]{", paste(kbl(table_floor, "latex", 
    caption = "Income floor (in \\$/day) financed by a 10\\% tax above \\$10/day for major lower-income countries in 2030.", position = "b", escape = F, booktabs = T, 
    digits = 1, linesep = rep("", nrow(table_floor)-1), longtable = F, label = "tax",
    caption.short = "Income floor (in \\$/day) financed by a 10\\% tax above \\$10/day.", col.names = NULL), collapse="\n")), fixed = T))), file = "../tables/floor.tex")  # 

(table_floor_full <- create_appendix_table(fun = "compute_income_floor", marginal_rates = list(10), thresholds = list(6.85), 
                                      growths = c("average", "trend_ineq", "average", "reg", "reg", "very_optimistic", "very_optimistic", "sdg8"), dfs = list(p, p, s, p, s, p, s, p)))
cat(sub("Angola", "\\\\midrule Angola", 
        sub("toprule", "toprule Growth scenario over 2022--2030 & 3\\\\% & \\\\makecell{3\\\\% + trend \\\\\\\\inequality} & 3\\\\% & \\\\multicolumn{2}{c}{Projection} & 7\\\\% & 7\\\\% & \\\\makecell{7\\\\% since \\\\\\\\2015} \\\\\\\\ \nHFCE rescaling & & & \\\\checkmark & & \\\\checkmark & & \\\\checkmark & \\\\\\\\ \n \\\\midrule", 
            sub("end{tabular}", "end{tabular}}", sub("centering", "makebox[\\\\textwidth][c]{", paste(kbl(table_floor_full, "latex", 
    caption = "Income floor (in \\$/day) financed by a 10\\% tax above \\$10/day for major lower-income countries in 2030.", position = "b", escape = F, booktabs = T, 
    digits = 1, linesep = rep("", nrow(table_floor_full)-1), longtable = F, label = "tax",
    caption.short = "Income floor (in \\$/day) financed by a 10\\% tax above \\$10/day.", col.names = NULL), collapse="\n")), fixed = T))), file = "../tables/floor_full.tex")  # 


##### Robustness: unbalanced growth #####
# Quantiles / SD of growth_share_; avg growth_share_ by quintile; evol of Gini, etc.; poverty rates & gaps; discrepancies in income floor
table_poverty[c(7, 12), ]
quantile(p$year - p$year_ante, c(.05, .1, .25, .5, .75, .9, .95), na.rm = T) 
mean(p$year - p$year_ante == 1, na.rm = T)
quantile(p[, grepl("growth_share_", names(p))], c(.05, .1, .25, .5, .75, .9, .95), na.rm = T) 
# 92% of yearly variation in shares is within [-5%; +5%] and 47% within [-1%; +1%] of their last share
mean(abs(p[, grepl("growth_share_", names(p))]) < .01, na.rm = T) # 47%
mean(abs(p[, grepl("growth_share_", names(p))]) < .05, na.rm = T) # 92%
sd(unlist(p[, grepl("growth_share_", names(p))]), na.rm = T) # .07 (very high, driven by outliers)
for (i in 1:5) p[[paste0("growth_q", i)]] <- rowMeans(p[, paste0("growth_share_", (i-1)*20+c(1:20))])
mean(p$growth_q1, na.rm = T) # .001
mean(p$growth_q2, na.rm = T) # .005
mean(p$growth_q3, na.rm = T) # .005
mean(p$growth_q4, na.rm = T) # .0004
mean(p$growth_q5, na.rm = T) # -.003
mean(p$growth_q1 > 0, na.rm = T) # 58% (between 40% and 60% > 0 for each quintile)
mean(p$growth_q5 > 0, na.rm = T) # 40%

compute_inequality(df = w, var = "Y3", return = 'gini') # .62
compute_inequality(df = w, var = "Y3_ineq", return = 'gini', recompute = T) # .62

p$demogrant_7__10_ineq <- compute_income_floor(df = p, thresholds = 6.85, marginal_rates = 10, growth = "trend_ineq", scope_tax = p)
sum(p$demogrant_7__10 < 2.15, na.rm = T)
sum(p$demogrant_7__10_ineq < 2.15, na.rm = T)
setdiff(p$country[p$demogrant_7__10 < 2.15 | p$demogrant_7__10_ineq < 2.15], p$country[p$demogrant_7__10 < 2.15 & p$demogrant_7__10_ineq < 2.15])
quantile(p$demogrant_7__10_ineq/p$demogrant_7__10-1, c(.05, .1, .25, .5, .75, .9, .95), na.rm = T)
mean(p$demogrant_7__10_ineq > p$demogrant_7__10-1, na.rm = T) # 79%
mean(p$demogrant_7__10_ineq/p$demogrant_7__10-1 < .2, na.rm = T) # 82%
mean(p$demogrant_7__10_ineq/p$demogrant_7__10-1 < .1, na.rm = T) # 68%
mean(abs(p$demogrant_7__10_ineq/p$demogrant_7__10-1) < .1, na.rm = T) # 44%
mean(abs(p$demogrant_7__10_ineq/p$demogrant_7__10-1) < .33, na.rm = T) # 82%

View(p[,grepl("Y3|code", names(p))])

# save.image(".RData")