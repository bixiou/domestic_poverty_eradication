# Figure for the VoxDev column: antipoverty tax required in 2030 (3% growth) at price-equivalent and new poverty lines.
# Run after update.R (uses .RData_update).
library(ggplot2)
load(".RData_update")
p$tax_eq <- compute_antipoverty_tax(df = p, exemption_threshold = 6.85 * 2.46/2.15, poverty_threshold = 2.46, growth = "average")
p$tax_new <- compute_antipoverty_tax(df = p, exemption_threshold = 8.3, poverty_threshold = 3, growth = "average")
d <- p[!is.na(p$tax_new) & p$tax_new >= 40, c("country_short", "tax_eq", "tax_new")]
d <- d[order(d$tax_new), ]
write.csv(d, "../figures/voxdev_tax.csv", row.names = FALSE)
cap <- 250
long <- rbind(data.frame(country = d$country_short, line = "$2.15 converted to 2021 prices ($2.46; tax above $7.84)", tax = d$tax_eq),
              data.frame(country = d$country_short, line = "New international line ($3.00; tax above $8.30)", tax = d$tax_new))
long$country <- factor(long$country, levels = d$country_short)
long$line <- factor(long$line, levels = unique(long$line)[2:1])
long$shown <- pmin(long$tax, cap)
long$label <- ifelse(long$tax > cap, paste0(round(long$tax), "%"), "")
g <- ggplot(long, aes(x = shown, y = country, fill = line)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72, colour = "#fcfcfb", linewidth = 0.5) +
  geom_vline(xintercept = 100, colour = "#52514e", linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 103, y = 0.55, label = "Beyond 100%: even an income cap\nat the threshold falls short", hjust = 0, vjust = 0, size = 3.2, colour = "#52514e") +
  geom_text(aes(label = label), position = position_dodge(width = 0.8), hjust = 1.1, size = 3, colour = "white") +
  scale_fill_manual(values = c("#eb6834", "#2a78d6"), breaks = levels(long$line)[2:1], name = NULL) +
  scale_x_continuous(limits = c(0, cap), breaks = seq(0, 250, 50), labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Tax rate on consumption above the threshold needed to close the poverty gap", y = NULL,
       title = "Domestic redistribution needed to eradicate extreme poverty in 2030",
       subtitle = "Assuming 3% annual growth over 2022–2030. Countries with a rate of at least 40% at the new line.",
       caption = "Source: Fabre (2025), updated with World Bank PIP data (September 2026, 2021 PPP). Bars truncated at 250%.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", legend.direction = "vertical", legend.justification = "left", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3), plot.title.position = "plot", plot.caption.position = "plot",
        plot.background = element_rect(fill = "#fcfcfb", colour = NA), axis.text = element_text(colour = "#52514e"), plot.caption = element_text(colour = "#52514e", hjust = 0), plot.margin = margin(8, 16, 8, 8))
ggsave("../figures/voxdev_tax.png", g, width = 8, height = 0.32 * nrow(d) + 2.2, dpi = 200)
ggsave("../figures/voxdev_tax.pdf", g, width = 8, height = 0.32 * nrow(d) + 2.2)
# Variant without title, subtitle and caption, for the papers (update.tex, eb.tex)
g_paper <- g + labs(title = NULL, subtitle = NULL, caption = NULL)
ggsave("../figures/tax_by_country.pdf", g_paper, width = 8, height = 0.32 * nrow(d) + 1.4)
ggsave("../figures/tax_by_country.png", g_paper, width = 8, height = 0.32 * nrow(d) + 1.4, dpi = 200)
print(d)
