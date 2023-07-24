library(dplyr)

consumption_countries <- p %>%
  filter(welfare_type == "consumption") %>%
  pull(country_code)

income_countries <- p %>%
  filter(welfare_type == "income") %>%
  pull(country_code)

install.packages("mapdata")
install.packages("maps")
install.packages("ggplot2")
library(mapdata)
library(maps)
library(ggplot2)

install.packages("countrycode")
library(countrycode)
world_map <- map_data("world")
country_names_conso <- countrycode(consumption_countries, "iso3c", "country.name")
country_names_income <- countrycode(income_countries, "iso3c", "country.name")
country_names_income <- append(country_names_income, "USA")
world_map <- world_map[world_map$region != "Antarctica",] #
world_map <- world_map[!world_map$region %in% c("Antarctica", "American Samoa", "Micronesia", "Guam", "Niue", "Pitcairn Islands", "Cook Islands", "Tonga", "Kiribati", "Marshall Islands", "French Polynesia", "Fiji", "Samoa", "Wallis and Futuna", "Vanuatu"),]
world_map$couleur <- "blanc"
world_map$couleur[world_map$region %in% country_names] <- "rouge"
world_map$couleur[world_map$region %in% country_names2] <- "bleu"
consumption_income_map <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region, fill = couleur),
           color = "black") +
  scale_fill_manual(values = c("rouge" = "red", "bleu" = "blue", "gris"= "grey"),
                    guide = FALSE) +
  labs(fill = "Welfare type used by countries") +  
  
  theme_void()
print(consumption_income_map)
ggsave("consumption_income_map.pdf", pilot = consumption_income_map)