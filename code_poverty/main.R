# AF: tout ce qui est avant consigne peut être mis dans un fichier nommé ".Rprofile" pour que les paquets se chargent à l'ouverture du projet.
# AF: je ne vois aucune fonction dans votre code. Une bonne façon de coder c'est d'utiliser des fonctions (ma_fonction <- function(argument1) { ... } )

# Docs:
# En, 2p https://docs.google.com/document/d/12_JPK2F3Kj2CmRdMpL8hSPBPK-tImSLQc84Ordzz3tM/edit
# Fr https://docs.google.com/document/d/1u41m1U0FGlvt6aGKzZET3MWr0ulz3tORPw1xVRSKek0/edit?usp=sharing

#CONSIGNES
# Pour tous pays récupérer la dernière année des donnéeSdispo 
# estimer taux crissance  entre cette année kla et 2030 (cf tx croissance observée sur BM en $ 2017 PPP)
# imputer un croissance entre maintennat et 2030 (soit trouver une institiution tq BM ou GMS qui le fait soit prendre la croissance observée dans les 5 dernières années observ&es hors covid pour avoir un tx de recalibrage avec lequel on va multiplier le revenu total du pays)
# reproduire le travail du papier
# regarder le poverty gap à 2,15$ lvl indiv puis pays l'exprimer en $/personne (poverty  gap moyen) 
# faire une Hy=g entre 2030 va être la même pour chaque personne daNs chaque pays
# calculer: trouver valeur au delà de laquelle il faut tout expropier pour combler pgap avec transferts internes
# autre indicateurs: au delà de 2,15$ impôt linéaire; quel tx appliquer pour combler pvgap et regarder tx linéaire de 2,15`$, 6,95$, 13$`
# calculer Gini sans redistribution, et avec redistribution linéaire vs. expropriative => indicateur: réduction de Gini nécessaire pour éradiquer pauvreté.
# autre indicateur: quel y minimum on peut assurer en expropriant tout au-delà de $13/day? $7?

# Ajouter aux calculs la basic needs poverty line de Moatsos, cf. data https://clio-infra.eu/Indicators/GlobalExtremePovertyCostofBasicNeeds.html and https://clio-infra.eu/Indicators/GlobalExtremePovertyDollaraDay.html
# Try first with Moatsos 21 (more recent estimates). If results are not satisfactory, use Moatsos 16 (better methodology).
# Cite Ortiz et al. (18), computing the costs of an UBI at the national poverty line (Figure 2, 3).

# Other costing of extreme poverty eradication: UNCTAD (21, p. 15: growth needed), Vorisek & Yu (20, lite review), SDSN (19, excellent: talk about ODA, wealth & carbon taxes, estimate domestic resources, e.g. Table 4), Moyer & Hedden (20), 

# Data fetch
# PIP/PovcalNet data is *per capita* (without adjustment for household composition).
data <- read.csv("../data/Povcalnet 2017.csv") # AF: faut toujours mettre des chemins de fichiers relatifs, pas absolu
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

# Estimate future GDP pc
gdp_pc <- read_excel("../data/gdp_pc.xls")
colnames(gdp_pc)[-1] <- paste0("gdp_pc_", colnames(gdp_pc)[-1])
p <- merge(p, gdp_pc)
# p$gdp_pc_2019_over_2014 <- p$gdp_pc_2019/p$gdp_pc_2014
p$mean_growth_gdp_pc_14_19 <- pmax(0, (p$gdp_pc_2019/p$gdp_pc_2014)^(1/5)-1)
p$gdp_pc_2030 <- p$gdp_pc_2021 * (1 + p$mean_growth_gdp_pc_14_19)^9 # TODO: check whether this hypothesis makes sense (or whether we should use gdp projections / pop projection instead)
p$gdp_pc_year <- sapply(1:nrow(p), function(c) { p[[paste0("gdp_pc_", min(2021, p$year[c]))]][c] }) # TODO: for IDN, year = 2022, so we overestimate its growth by one year
p$growth_gdp_pc_year_30 <- p$gdp_pc_2030/p$gdp_pc_year
# p$country_code[is.na(p$gdp_pc_year)] # "SSD" "SYR" "VEN" "YEM"
# p$country_code[is.na(p$growth_gdp_pc_year_30)] # "SSD" "SYR" "TKM" "VEN" "YEM"
p$growth_gdp_pc_year_30[is.na(p$growth_gdp_pc_year_30)] <- 1 # TODO: improve this assumption
for (i in 1:100) p[[paste0("y_avg_", i)]] <- p[[paste0("avg_welfare_", i)]] * p$growth_gdp_pc_year_30
for (i in 1:100) p[[paste0("y_max_", i)]] <- p[[paste0("quantile_", i)]] * p$growth_gdp_pc_year_30
for (i in 2:100) p[[paste0("y_min_", i)]] <- p[[paste0("quantile_", i-1)]] * p$growth_gdp_pc_year_30
p$y_min_1 <- p$y_max_0 <- 0
p$y_max_100[is.na(p$y_max_100)] <- p$y_avg_100[is.na(p$y_max_100)]
p$mean_y <- rowMeans(p[,which(names(p)=="y_avg_1"):which(names(p)=="y_avg_100")])

# TODO! Use 7% growth: the SDG 8.1. Compute the antipoverty_tax, etc. that would have been needed with a 7% growth starting in 2016.
# y makes the assumption of constant growth while Y assumes 6% growth after 2021
p$gdp_pc_max_2030 <- p$gdp_pc_2021 * 1.07^9 # 1.08^9 = 1.999, 1.07^9 = 1.84, 1.06^9 = 1.7, CN 99-07: 1.095^9 = 2.26. Beyond 6.3%, RDC antipoverty_2_tax_7 < 100%
p$growth_gdp_pc_max_year_30 <- p$gdp_pc_max_2030/p$gdp_pc_year
p$growth_gdp_pc_max_year_30[is.na(p$growth_gdp_pc_max_year_30)] <- 1.1^(2030 - p$year[is.na(p$growth_gdp_pc_max_year_30)])
for (i in 1:100) p[[paste0("Y_avg_", i)]] <- p[[paste0("avg_welfare_", i)]] * p$growth_gdp_pc_max_year_30
for (i in 1:100) p[[paste0("Y_max_", i)]] <- p[[paste0("quantile_", i)]] * p$growth_gdp_pc_max_year_30
for (i in 2:100) p[[paste0("Y_min_", i)]] <- p[[paste0("quantile_", i-1)]] * p$growth_gdp_pc_max_year_30
p$Y_min_1 <- p$Y_max_0 <- 0
p$Y_max_100[is.na(p$Y_max_100)] <- p$Y_avg_100[is.na(p$Y_max_100)]
p$mean_Y <- rowMeans(p[,which(names(p)=="Y_avg_1"):which(names(p)=="Y_avg_100")])
sort(setNames(p$mean_growth_gdp_pc_14_19, p$country), decreasing = T) # max 14-19: CN = 6.15%, max 10-19:  CN 6.7%

# Add population
pop <- read.csv("../data/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
pop <- pop[pop$Time %in% c(sort(unique(p$year)), 2030),]
pop_iso3 <- aggregate(PopTotal ~ Time + ISO3_code, data = pop, FUN = sum)
pop_iso3 <- pop_iso3 %>% pivot_wider(names_from = Time, values_from = PopTotal)
names(pop_iso3) <- c("country_code", paste0("pop_", names(pop_iso3)[-1]))
p <- merge(p, pop_iso3)
p$pop_year <- sapply(1:nrow(p), function(c) { p[[paste0("pop_", p$year[c])]][c] }) # in thousands
# p.bak <- p
# p <- p.bak

# Manage CN, IA, ID rural/urban (/!\ in ARG, SUR, there is only urban as reporting_level)
pop_rurb <- read.csv2("../data/pop_rural_urban.csv")
for (c in c("CHN", "IDN", "IND")) {
  u <- p$country_code == c & p$reporting_level == "urban"
  r <- p$country_code == c & p$reporting_level == "rural"
  frac_urb_2030 <- pop_rurb$yr_2030[pop_rurb$country_code == c & pop_rurb$reporting_level == "urban"]/pop_rurb$yr_2030[pop_rurb$reporting_level == "national" & pop_rurb$country_code == c]
  for (v in names(p)[grepl("pop_share_", names(p))]) p[[v]][u] <- p[[v]][u] * frac_urb_2030
  for (v in names(p)[grepl("pop_share_", names(p))]) p[[v]][r] <- p[[v]][r] * (1 - frac_urb_2030)
  quantiles <- sort(unlist(sapply(1:100, function(i) {p[[paste0("y_max_", i)]][p$country_code == c] })))
  cdf <- c()
  for (q in quantiles) cdf <- c(cdf, sum(sapply(1:100, function(j) { (p[[paste0("y_max_", j)]][u] <= q) * p[[paste0("pop_share_", j)]][u] + (p[[paste0("y_max_", j)]][r] <= q) * p[[paste0("pop_share_", j)]][r] })))
  percentiles <- findInterval(seq(0, 1, .01), cdf, left.open = T)[-1] # computes the indices for which the pop_share is lesser or equal to the percentiles.
  new_line <- p[u, ] # TODO? Create new_line with all NAs instead?
  new_line$reporting_level <- "national"
  p <- rbind(p, new_line)
  for (i in 1:100) {
    p[[paste0("y_max_", i)]][p$country_code == c & p$reporting_level == "national"] <- quantiles[percentiles[i]]
    if (i == 1) p$pop_share_1[p$country_code == c & p$reporting_level == "national"] <- cdf[percentiles[1]] 
    else p[[paste0("pop_share_", i)]][p$country_code == c & p$reporting_level == "national"] <- cdf[percentiles[i]] - cdf[percentiles[i-1]]
    p[[paste0("y_min_", i)]][p$country_code == c & p$reporting_level == "national"] <- p[[paste0("y_max_", i-1)]][p$country_code == c & p$reporting_level == "national"]
    p[[paste0("y_avg_", i)]][p$country_code == c & p$reporting_level == "national"] <- (
      sum(sapply(1:100, function(k) { p[[paste0("pop_share_", k)]][u] * p[[paste0("y_avg_", k)]][u] * (p[[paste0("y_max_", k)]][u] <= p[[paste0("y_max_", i)]][u]) * (p[[paste0("y_max_", k)]][u] > p[[paste0("y_max_", i-1)]][u]) })) +
      sum(sapply(1:100,function(k){p[[paste0("pop_share_", k)]][r] * p[[paste0("y_avg_", k)]][r] * (p[[paste0("y_max_", k)]][r] <= p[[paste0("y_max_", i)]][r]) * (p[[paste0("y_max_", k)]][r] > p[[paste0("y_max_", i-1)]][r]) }))) / (
        sum(sapply(1:100, function(k) { p[[paste0("pop_share_", k)]][u] * (p[[paste0("y_max_", k)]][u] <= p[[paste0("y_max_", i)]][u]) * (p[[paste0("y_max_", k)]][u] > p[[paste0("y_max_", i-1)]][u]) })) +
        sum(sapply(1:100,function(k){p[[paste0("pop_share_", k)]][r] * (p[[paste0("y_max_", k)]][r] <= p[[paste0("y_max_", i)]][r]) * (p[[paste0("y_max_", k)]][r] > p[[paste0("y_max_", i-1)]][r]) })) )
  }
  p <- p[p$country_code != c | p$reporting_level == "national",]
}


# Add country name
iso3 <- read.csv("../data/country_iso3.csv")
iso3$country_code[!iso3$country_code %in% p$country_code] # TODO: many countries absent! AFG, CUB, LBY...
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

# #Tableau de l'average welfare par percentile
# data_pivot <- data %>%
#   filter(!is.na(avg_welfare)) %>%
#   group_by(country_code, percentile) %>%
#   summarize(mean_avg_welfare = mean(avg_welfare)) # AF: ça sert à quoi cette ligne (enfin ces 4 lignes)?
# str(data_pivot)
# data_avgwelf <- data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_avg_welfare)
# str(data_avgwelf)
# data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_avg_welfare, values_fill = 0) %>%
#   head()
# 
# #Tableau du welfare share par percentile
# data_pivot <- data %>%
#   filter(!is.na(welfare_share)) %>%
#   group_by(country_code, percentile) %>%
#   summarize(mean_welfare_share = mean(welfare_share))
# str(data_pivot)
# data_welfshare <- data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_welfare_share)
# str(data_welfshare)
# data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_welfare_share, values_fill = 0) %>%
#   head()
# 
# #Tableau de la pop share par percentile
# data_pivot <- data %>%
#   filter(!is.na(pop_share)) %>%
#   group_by(country_code, percentile) %>%
#   summarize(mean_pop_share = mean(pop_share))
# str(data_pivot)
# data_pop_share <- data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_pop_share)
# str(data_pop_share)
# data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_pop_share, values_fill = 0) %>%
#   head()
# 
# #Tableau du quantile par percentile
# data_pivot <- data %>%
#   filter(!is.na(quantile)) %>%
#   group_by(country_code, percentile) %>%
#   summarize(mean_quantile = mean(quantile))
# str(data_pivot)
# data_quantile <- data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_quantile)
# str(data_quantile)
# data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_quantile, values_fill = 0) %>%
#   head()
# 
# #Tableau des types de welfare
# data_pivot <- data %>%
#   filter(!is.na(welfare_type)) %>%
#   group_by(country_code, percentile) %>%
#   summarize(mean_welfare_type = welfare_type)
# str(data_pivot)
# data_welfare_type <- data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_welfare_type)
# str(data_welfare_type)
# data_pivot %>%
#   pivot_wider(names_from = percentile, values_from = mean_welfare_type, values_fill = 0) %>%
#   head()

#Reglages sur les noms des colonnes des percentiles
# AF: pourquoi vous faites ça avec welfare_type ? Y a des pays pour lesquels le welfare_type dépend du percentile ?
# country_code <- colnames(data_welfare_type)[1]
# welftype <- paste0("welftype", 1:(ncol(data_welfare_type)-1))
# colnames(data_welfare_type)[-1] <- welftype
# colnames(data_welfare_type)[1] <- country_code
# 
# country_code <- colnames(data_avgwelf)[1]
# avgwelf <- paste0("avgwelf", 1:(ncol(data_avgwelf)-1))
# colnames(data_avgwelf)[-1] <- avgwelf
# 
# colnames(data_avgwelf)[1] <- country_code
# 
# country_code <- colnames(data_pop_share)[1]
# popshare <- paste0("popshare", 1:(ncol(data_pop_share)-1))
# colnames(data_pop_share)[-1] <- popshare
# colnames(data_pop_share)[1] <- country_code
# 
# country_code <- colnames(data_quantile)[1]
# quant <- paste0("quant", 1:(ncol(data_quantile)-1))
# colnames(data_quantile)[-1] <- quant
# colnames(data_quantile)[1] <- country_code
# 
# country_code <- colnames(data_welfshare)[1]
# welfshare <- paste0("welfshare", 1:(ncol(data_welfshare)-1))
# colnames(data_welfshare)[-1] <- welfshare
# colnames(data_welfshare)[1] <- country_code

#Grand tableau
# Merge_1 <- merge(data_welfare_type, data_avgwelf)
# Merge_2 <- merge(Merge_1,data_pop_share)
# Merge_3 <- merge(Merge_2, data_welfshare)
# Merge_4 <- merge(Merge_3,data_quantile)

#Calcul du Poverty Gap individuel et national

compute_poverty_gap <- function(df = p, threshold = 2.15, type = "sum") {
  pg <- rowSums(sapply(1:100, function(i) { pmax(0, threshold - df[[paste0("avg_welfare_", i)]]) }))
  if (type == "mean") pg <- pg/100
  return(pg)
}
p$poverty_gap_2 <- compute_poverty_gap()
p$poverty_gap_4 <- compute_poverty_gap(threshold = 3.65)
p$poverty_gap_7 <- compute_poverty_gap(threshold = 6.85)

# Seuil <- 2.15
# Pov_gap <- data_avgwelf
# Pov_gap[, -1] <- apply(data_avgwelf[, -1], 2, function(x) ifelse(Seuil-x<0, 0, Seuil-x))
# country_code <- colnames(data_avgwelf)[1]
# Pov_gap_of_p <- paste0("Pov_gap_of_p", 1:(ncol(data_avgwelf)-1))
# colnames(Pov_gap)[-1] <- Pov_gap_of_p
# colnames(Pov_gap)[1] <- country_code
# Pov_gap$Somme_pov_gap <- rowSums(Pov_gap[, -1], na.rm = TRUE)

#Tri données PIB/capita



# PIB_capita<-read_xls("PIB_capita.xls")
# Merge_9 <- merge(PIB_capita, data_quantile , by="country_code") # AF: ça veut dire quoi "tri" ?
# PIB_capita_non_tri <- Merge_9[, 1:33]
# colonne_year_max <- data$country_code[!duplicated(data$country_code)]
# data_year_max <- data[!duplicated(data$country_code), ]
# data_year_max <- data_year_max[,c("country_code","year")]
# PIB_capita_non_tri <- merge(data_year_max,PIB_capita_non_tri)
# PIB_capita_non_tri$year[PIB_capita_non_tri$year == 2020] <- 2019 # AF: c'est pour quoi ça ?

# PIB_capita_tri <- data.frame(
#   country_code = PIB_capita_non_tri$country_code,
#   focus_year = PIB_capita_non_tri$year,
#   PIB = numeric(length(PIB_capita_non_tri$country_code))
# )
# 
# for (i in 1:nrow(PIB_capita_non_tri)) { # AF: et ça, c'est pour quoi ?
#   focus_year <- PIB_capita_non_tri$year[i]
#   if (focus_year < 2022) {
#     year_column <- colnames(PIB_capita_non_tri)[which(colnames(PIB_capita_non_tri) == paste0(focus_year))]
#     PIB_capita_tri$PIB[i] <- PIB_capita_non_tri[i, year_column]
#   }
#   else PIB_capita_tri$PIB[i] = NA
# }
# 
# 
# # Ratio des PIB en 2017 PPP pour trouver les taux de croissance en 2017 PPP
# new_PIB2017 <- PIB_capita
# new_PIB2017 <- new_PIB2017[-c(1), ]
# colnames(new_PIB2017)[1] <- country_code
# colnames(new_PIB2017)[2] <- c("X2015")
# colnames(new_PIB2017)[3] <- c("X2016")
# colnames(new_PIB2017)[4] <- c("X2017")
# colnames(new_PIB2017)[5] <- c("X2018")
# colnames(new_PIB2017)[6] <- c("X2019")
# colnames(new_PIB2017)[7] <- c("X2021")
# 
# G_PIB2017 <- merge(new_PIB2017, PIB_capita_tri)
# 
# G_PIB2017$X2015 <- as.numeric(G_PIB2017$X2015)
# G_PIB2017$X2016 <- as.numeric(G_PIB2017$X2016)
# G_PIB2017$X2017 <- as.numeric(G_PIB2017$X2017)
# G_PIB2017$X2018 <- as.numeric(G_PIB2017$X2018)
# G_PIB2017$X2019 <- as.numeric(G_PIB2017$X2019)
# G_PIB2017$X2021 <- as.numeric(G_PIB2017$X2021)
# G_PIB2017 <- G_PIB2017[, -10]
# G_PIB2017$Growth_rate1 <- NA
# for(i in 1:length(G_PIB2017$`X2015`)) for(i in 1:length(G_PIB2017$`X2016`)) G_PIB2017$Growth_rate1[i] <- (G_PIB2017$`X2016`[i]/G_PIB2017$`X2015`[i])
# G_PIB2017$Growth_rate2 <- NA
# for(i in 1:length(G_PIB2017$`X2016`)) for(i in 1:length(G_PIB2017$`X2017`)) G_PIB2017$Growth_rate2[i] <- (G_PIB2017$`X2017`[i]/G_PIB2017$`X2016`[i])
# G_PIB2017$Growth_rate3 <- NA
# for(i in 1:length(G_PIB2017$`X2017`)) for(i in 1:length(G_PIB2017$`X2018`)) G_PIB2017$Growth_rate3[i] <- (G_PIB2017$`X2018`[i]/G_PIB2017$`X2017`[i])
# G_PIB2017$Growth_rate4 <- NA
# for(i in 1:length(G_PIB2017$`X2018`)) for(i in 1:length(G_PIB2017$`X2019`)) G_PIB2017$Growth_rate4[i] <- (G_PIB2017$`X2019`[i]/G_PIB2017$`X2018`[i])
# G_PIB2017$Growth_rate5 <- NA
# for(i in 1:length(G_PIB2017$`X2019`)) for(i in 1:length(G_PIB2017$`X2021`)) G_PIB2017$Growth_rate5[i] <- (G_PIB2017$`X2021`[i]/G_PIB2017$`X2019`[i]) # AF: y a un problème là non ? pourquoi 2019-21 ?
# G_PIB2017 <- G_PIB2017[, -2]
# G_PIB2017 <- G_PIB2017[, -3]
# G_PIB2017 <- G_PIB2017[, -7]
# G_PIB2017 <- G_PIB2017[, -4]
# G_PIB2017 <- G_PIB2017[, -2]
# G_PIB2017 <- G_PIB2017[, -2]
# G_PIB2017 <- G_PIB2017[, -2]
# G_PIB2017 <- G_PIB2017[, -1]
# 
# # Moyenne sur les 5 dernières années avec ces tx pour trouver le g en 2017 PPP
# G_PIB2017$g_moyen <-rowMeans(G_PIB2017)
# 
# # calcul des projections de PIB de 2022 à 2030
# 
# new_PIB2017bis <- merge(new_PIB2017,data_avgwelf, by=country_code)
# G_PIB2017_final <- cbind(new_PIB2017bis$country_code, new_PIB2017bis$X2021, G_PIB2017)
# colnames(G_PIB2017_final)[1:2] <- c("country_code","PIB_2021")
# 
# G_PIB2017_final$PIB_2021 <- as.numeric(G_PIB2017_final$PIB_2021)
# G_PIB2017_final$PIBproj_2022 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2022[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^9*(G_PIB2017_final$`PIB_2021`[i])
# G_PIB2017_final$PIBproj_2023 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2023[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^8*(G_PIB2017_final$`PIBproj_2022`[i])
# G_PIB2017_final$PIBproj_2024 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2024[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^7*(G_PIB2017_final$`PIBproj_2023`[i])
# G_PIB2017_final$PIBproj_2025 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2025[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^6*(G_PIB2017_final$`PIBproj_2024`[i])
# G_PIB2017_final$PIBproj_2026 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2026[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^5*(G_PIB2017_final$`PIBproj_2025`[i])
# G_PIB2017_final$PIBproj_2027 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2027[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^4*(G_PIB2017_final$`PIBproj_2026`[i])
# G_PIB2017_final$PIBproj_2028 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2028[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^3*(G_PIB2017_final$`PIBproj_2027`[i])
# G_PIB2017_final$PIBproj_2029 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2029[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^2*(G_PIB2017_final$`PIBproj_2028`[i])
# G_PIB2017_final$PIBproj_2030 <- NA
# for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2030[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^1*(G_PIB2017_final$`PIBproj_2029`[i])

# #A REFAIRE
# PIB_pourcalcul <- PIB_capita_non_tri[ , - c(2:33)]
# Merge_10 <- merge(PIB_capita_tri, PIB_pourcalcul, by="country_code")
# colnames(Merge_10)[4] <- c("X2021")
# Merge_10$Ratio_PIB <- NA
# for(i in 1:length(Merge_10$`PIB`)) for(j in 1:length(Merge_10$`X2021`)) Merge_10$Ratio_PIB[i] <- (Merge_10$`X2021`[i]/Merge_10$`PIB`[i])
# colnames(Croissance_Pays3)[1] <- c("country_code")
# Merge_11 <- merge(Merge_10, Croissance_Pays3)
# 
# Merge_11$Tx_Calage <- NA
# for(i in 1:length(Merge_11$`Ratio_PIB`)) for(j in 1:length(Merge_11$`Moyenne_croissance$RowMean`)) Merge_11$Tx_Calage[i] <- (Merge_11$`Ratio_PIB`[i]*(1+Merge_11$`Moyenne_croissance$RowMean`[j]/100)^9)
# 
# #IL FAUT JUSTE  CHANGER MERGE_11
# data_calage <- merge( Merge_11, data_avgwelf)
# data_calage <- data_calage[-c(2:13)]
# calcul_calage <- data_calage
# calcul_calage[,3:101] <- calcul_calage[,3:101]*calcul_calage[,2]
# avgwelf_calage_of_p <- paste0("avgwelf_calage_of_p", 1:(ncol(calcul_calage)-2))
# colnames(calcul_calage)[3:102] <- avgwelf_calage_of_p

#calculer pour chaque pays le "anti-poverty maximum"=le seuil à partir duquel il faut exproprier tous les revenus pour éradiquer la pauvreté au seuil de 2.15$ 
#calculer le "anti-poverty tax", i.e. taux de taxation (au-delà de, disons, 6.85$) nécessaire pour combler le poverty gap

#Peut on combler le poverty gap avec seulement des échanges internes?
# Anti_pov_gap_nv_seuil <- merge(Pov_gap, data_quantile)
# Anti_pov_gap_nv_seuil <- Anti_pov_gap_nv_seuil[-c(2:101)]
# Anti_pov_gap_avg_welf <- merge(Pov_gap, data_avgwelf)
# Anti_pov_gap_avg_welf <- Anti_pov_gap_avg_welf[-c(2:101)]
# Anti_pov_gap <- data.frame(country_code = Anti_pov_gap_nv_seuil$country_code, Pov_gap_nat = Anti_pov_gap_nv_seuil$Somme_pov_gap )
# Anti_pov_gap$funded <- 0
# funded <- Anti_pov_gap$funded
# i <- 102
# Anti_pov_gap$percentile_expropriated <- NA
# for (row in 1:nrow(Anti_pov_gap)) {
#   funded <- Anti_pov_gap[row, "funded"]
#   i <- 102
#   while (funded < Anti_pov_gap$Pov_gap_nat[row] && i >= 3) {
#     percentile_expropriated_seuil <- colnames(Anti_pov_gap_nv_seuil)[i-1]
#     percentile_expropriated_welf <- colnames(Anti_pov_gap_avg_welf)[i]
#     funded <- funded + Anti_pov_gap_avg_welf[row, percentile_expropriated_welf] - Anti_pov_gap_nv_seuil[row, percentile_expropriated_seuil]
#     i <- i - 1
#   }
#   if (i <= 3 || funded > Anti_pov_gap$Pov_gap_nat[row]) {
#     Anti_pov_gap$percentile_expropriated[row] <- as.numeric(gsub("avgwelf", "", percentile_expropriated_welf))
#     Anti_pov_gap[row, "funded"] <- funded
#   }
# }

# Percentile above which we expropriate all y to fill the poverty gap
compute_antipoverty_maximum <- function(df = p, threshold = 2.15, return = "y", growth = "realistic") {
  y <- if (growth == "realistic") "y" else if (growth == "optimistic") "Y"
  df$poverty_gap <- compute_poverty_gap(df = df, threshold = threshold, type = "sum")
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
  else return(df$y_expropriated)
}
p$percentile_expropriated_2 <- compute_antipoverty_maximum(df = p, threshold = 2.15, return = "percentile")
p$percentile_expropriated_4 <- compute_antipoverty_maximum(df = p, threshold = 3.65, return = "percentile")
p$percentile_expropriated_7 <- compute_antipoverty_maximum(df = p, threshold = 6.85, return = "percentile")
p$percentile_expropriated_13 <- compute_antipoverty_maximum(df = p, threshold = 13, return = "percentile")

p$y_expropriated_2 <- compute_antipoverty_maximum(df = p, threshold = 2.15)
p$y_expropriated_4 <- compute_antipoverty_maximum(df = p, threshold = 3.65)
p$y_expropriated_7 <- compute_antipoverty_maximum(df = p, threshold = 6.85)
p$y_expropriated_13 <- compute_antipoverty_maximum(df = p, threshold = 13)

p$percentile_expropriated_2_optimistic <- compute_antipoverty_maximum(df = p, threshold = 2.15, return = "percentile", growth = "optimistic")
p$percentile_expropriated_4_optimistic <- compute_antipoverty_maximum(df = p, threshold = 3.65, return = "percentile", growth = "optimistic")
p$percentile_expropriated_7_optimistic <- compute_antipoverty_maximum(df = p, threshold = 6.85, return = "percentile", growth = "optimistic")
p$percentile_expropriated_13_optimistic <- compute_antipoverty_maximum(df = p, threshold = 13, return = "percentile", growth = "optimistic")

p$y_expropriated_2_optimistic <- compute_antipoverty_maximum(df = p, threshold = 2.15, growth = "optimistic")
p$y_expropriated_4_optimistic <- compute_antipoverty_maximum(df = p, threshold = 3.65, growth = "optimistic")
p$y_expropriated_7_optimistic <- compute_antipoverty_maximum(df = p, threshold = 6.85, growth = "optimistic")
p$y_expropriated_13_optimistic <- compute_antipoverty_maximum(df = p, threshold = 13, growth = "optimistic")

#Calcul de la base taxable
# exemption_threshold <- 7
# result_table <- data.frame(matrix(ncol = ncol(data_avgwelf), nrow = nrow(data_avgwelf)))
# result_table[, 1] <- data_avgwelf[, 1]
# 
# calcul_taxable_base <- function(avg_welfare, exemption_threshold) {
#   taxable_base <- sum(pmin(0, avg_welfare - exemption_threshold))
#   return(taxable_base)
# }
# 
# for (row in 1:nrow(data_avgwelf)) {
#   for (col in names(data_avgwelf)[-1]) {
#     result_taxable_base[row, col] <- calcul_taxable_base(data_avgwelf[row, col], exemption_threshold)
#   }
# }

compute_antipoverty_tax <- function(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, return = "tax", growth = "realistic") {
  y <- if (growth == "realistic") "y" else if (growth == "optimistic") "Y"
  df$taxable_base <- rowSums(sapply(1:100, function(i) { pmax(0, df[[paste0(y, "_avg_", i)]] - exemption_threshold) }))
  df$antipoverty_tax <- 100 * compute_poverty_gap(df = df, threshold = poverty_threshold) / df$taxable_base
  if (return == "base") return(df$taxable_base)
  else return(df$antipoverty_tax)
}
p$antipoverty_2_tax_13 <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 2.15)
p$antipoverty_2_tax_7 <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15)
p$antipoverty_2_tax_4 <- compute_antipoverty_tax(df = p, exemption_threshold = 3.65, poverty_threshold = 2.15)
p$antipoverty_2_tax_2 <- compute_antipoverty_tax(df = p, exemption_threshold = 2.15, poverty_threshold = 2.15)
p$antipoverty_4_tax_7 <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 3.65)
p$antipoverty_4_tax_13 <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 3.65)
p$antipoverty_7_tax_13 <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 6.85)
p$antipoverty_13_tax_13 <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 13)

p$antipoverty_2_tax_13_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_7_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_4_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 3.65, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_2_tax_2_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 2.15, poverty_threshold = 2.15, growth = "optimistic")
p$antipoverty_4_tax_7_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85, poverty_threshold = 3.65, growth = "optimistic")
p$antipoverty_4_tax_13_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 3.65, growth = "optimistic")
p$antipoverty_7_tax_13_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 6.85, growth = "optimistic")
p$antipoverty_13_tax_13_optimistic <- compute_antipoverty_tax(df = p, exemption_threshold = 13, poverty_threshold = 13, growth = "optimistic")

# Results
sort(setNames(p$poverty_gap_2, p$country))
sort(setNames(p$antipoverty_2_tax_2, p$country))
sort(setNames(p$antipoverty_2_tax_7_optimistic, p$country))
sort(setNames(p$y_expropriated_2, p$country), decreasing = T)
sum(p$y_expropriated_2 < 13) # 19
sum(p$pop_2030[p$y_expropriated_2 < 13]) # 700M
sum(p$pop_2022[p$y_expropriated_2 < 13]) # 571M
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


# /!\ PROBLEM: huge discrepancies between PovcalNet and GDP pc data, e.g. for MDG:
p$mean_y[p$country == "Madagascar"]
p$gdp_pc_2030[p$country == "Madagascar"]/365 # GDP pc from World Bank
# TODO: compute poverty gap in 2019 and compare with World Bank https://data.worldbank.org/indicator/SI.POV.GAPS
# TODO: search for explanation in literature


# Maps
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
plot_world_map("antipoverty_2_tax_7", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf), 
               # labels = sub("≤", "<", agg_thresholds(c(0), c(0, .1, 1, 5, 10, 25, 50, 100, Inf), sep = "to", return = "levels")),
               legend = "Taux de taxe linéaire \nau-dessus de $6.85/jour\nnécessaire pour éradiquer\nl'extrême pauvreté à $2.15/jour\n(en 2017 PPA)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T) 
plot_world_map("antipoverty_2_tax_2", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf),
               legend = "Taux de taxe linéaire \nau-dessus de $2.15/jour\nnécessaire pour éradiquer\nl'extrême pauvreté à $2.15/jour\n(en 2017 PPA)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T) 
plot_world_map("antipoverty_2_tax_13", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf),
               legend = "Taux de taxe linéaire\n au-dessus de $13/jour\nnécessaire pour éradiquer\nl'extrême pauvreté à $2.15/jour\n(en 2017 PPA)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T) 
# plot_world_map("percentile_expropriated_2", breaks = c(-Inf, 0, 50, 90, 95, 99, 100), sep = " to ", end = "", strict_ineq_lower = T,
#                legend = "Percentile above which\nall should be expropriated\nto lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T, 
#                save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T) 
# plot_world_map("percentile_expropriated_7", breaks = c(-Inf, 0, 50, 90, 95, 99, 100), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf
#                legend = "Percentile above which\nall should be expropriated\nto lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T, 
#                save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .07, trim = T) 
plot_world_map("y_expropriated_2", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T,
               legend = "Revenu journalier au\ndessus duquel tout devrait\nêtre exproprié pour élever \ntous les habitants du pays\nau-dessus de $2.15/jour\n(en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)
plot_world_map("y_expropriated_7", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf
               legend = "Revenu journalier au\ndessus duquel tout devrait\nêtre exproprié pour élever \ntous les habitants du pays\nau-dessus de $6.85/jour\n(en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)
plot_world_map("y_expropriated_13", breaks = c(0, 2.15, 4, 7, 13, 20, 40, 100, Inf), sep = " to ", end = "", strict_ineq_lower = T, # svg, pdf
               legend = "Revenu journalier au\ndessus duquel tout devrait\nêtre exproprié pour élever \ntous les habitants du pays\nau-dessus de $13/jour\n(en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = FALSE, format = c('png', 'pdf'), legend_x = .05, trim = T)
plot_world_map("poverty_gap_2", breaks = c(0, 2, 10, 20, 40, 60, 100), sep = " to ", end = "", strict_ineq_lower = FALSE, # svg, pdf
               legend = "Écart de pauvreté (en %)\nà $2.15/jour (en $ 2017 PPA)", #fill_na = T,
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .05, trim = T)
