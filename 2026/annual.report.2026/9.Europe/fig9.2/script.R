# ------------------------------------------------------------------------------
# AQLI 2026 – Europe PM2.5 Trends (Eastern vs Western Europe)
# Description : Computes and visualizes population-weighted PM2.5 trends for 
#               Eastern and Western Europe (1998–2024), highlighting divergence.
#
# Developed By: Purushottam Gupta
# Role        : Data Architect & Data Analyst, AQLI (University of Chicago)
# Contact     : guptap@uchicago.edu
# Dependencies: dplyr, tidyr, ggplot2, stringr, ggthemes
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load Dependencies & Source Files
# ------------------------------------------------------------------------------
source("R/july.2025.helper.script.R")

# ------------------------------------------------------------------------------
# Data Preparation: Europe Dataset
# ------------------------------------------------------------------------------

# exclude the following countries to keep the map less wide and to show a stark 
# difference between eastern and western Europe
exclude_countries <- c(
  "Russia", "Turkey", "Sweden", "Finland", "Norway",
  "Kazakhstan", "Iceland", "Georgia", "Azerbaijan",
  "Armenia", "Cyprus", "Northern Cyprus",
  "Svalbard and Jan Mayen"
)

countries_filtered <- setdiff(europe_countries, exclude_countries)

# create a Europe AQLI dataset
gadm2_aqli_2024_europe <- gadm2_aqli_2024 %>%
  filter(country %in% countries_filtered) %>%
  mutate(
    region = if_else(
      country %in% western_european_countries,
      "Western Europe",
      "Eastern Europe"
    )
  )

# ------------------------------------------------------------------------------
# Data Preparation: Population-weighted PM2.5 Trends
# ------------------------------------------------------------------------------
europe_trend_data <- gadm2_aqli_2024_europe %>%
  group_by(region) %>%
  filter(!is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2024_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)


)
europe_fs_fig4_data$region = factor(europe_fs_fig4_data$region, levels = c("Eastern Europe", "Western Europe"))


# ------------------------------------------------------------------------------
# Data Preparation: Gap (Connected Segments)
# ------------------------------------------------------------------------------
connected_data <- europe_fs_fig4_data %>%
  select(years, region, pop_weighted_avg_pm2.5) %>%
  pivot_wider(names_from = region, values_from = pop_weighted_avg_pm2.5) %>%
  drop_na()
# ------------------------------------------------------------------------------
# Visualization
# ------------------------------------------------------------------------------
europe_trend_plot <- ggplot() +
  geom_segment(data = connected_data,
               aes(x = years, xend = years,
                   y = `Western Europe`, yend = `Eastern Europe`),
               color = "lightgrey",
               linetype = "solid",
               linewidth = 0.5) +
  geom_line(data = europe_fs_fig4_data,
            mapping = aes(x = years, y = pop_weighted_avg_pm2.5,
                          color = interaction(region),
                          linetype = interaction(region)), lwd = 1.3) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted", color = "lightgrey") +
  geom_hline(mapping = aes(yintercept = 10), lwd = 0.8, linetype = "dotted", color = "darkgrey") +
  annotate("text", x = 1999, y = 21.5, label = "Difference in gain in \nlife expectancy from \nmeeting WHO guideline\nbetween Eastern \nand Western Europe \n(1998): 4.5 months") +
  annotate("text", x = 2022.5, y = 17.5, label = "Difference in gain in \nlife expectancy from \nmeeting WHO guideline\nbetween Eastern \nand Western Europe \n(2024): 6.3 months") +
  scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 25)) +
  scale_x_continuous(breaks = c(seq(1998, 2023, 2), 2024)) +
  scale_color_manual(values = c("Eastern Europe" = "#3f8dac",
                                "Western Europe" = "#b7ebf1")) +
  scale_linetype_manual(values = c("Eastern Europe" = "solid",
                                   "Western Europe" = "solid")) +
  geom_text(aes(x = 2002.75, y = 5.7,
                label = "WHO~PM[2.5]~Guideline~(last~updated:~2021):~5~µg/m^3"),
            parse = TRUE, size = 4.5) +
  geom_text(aes(x = 2002, y = 10.7,
                label = "European~Union~PM[2.5]~2030~targets:~10~µg/m^3"),
            parse = TRUE, size = 4.5)+
  labs(x = "Year",
       y = expression("Annual Average " ~ PM[2.5] ~ " Concentration (in µg/m³)"),
       title = "") +
  ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 0.6, unit = "cm")),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.line = element_line(),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(2, "cm"))


# ------------------------------------------------------------------------------
# Export Plot
# ------------------------------------------------------------------------------
ggsave(
  filename = "~/Desktop/Final plots aqli/Europe/fig9.2/ar_europe_fig9.2.png",
  plot = europe_trend_plot,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)
ggsave(
  filename = "~/Desktop/Final plots aqli/Europe/fig9.2/ar_europe_fig9.2.svg",
  plot = europe_trend_plot,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)