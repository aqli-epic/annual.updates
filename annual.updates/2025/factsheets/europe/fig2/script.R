# read in the helper file
source("R/july.2025.helper.script.R")

# Figure 2: Annual average PM2.5 concentration in EU, non-EU and Europe 1998-2023 ------
# create a europe AQLI dataset
gadm2_aqli_2023_europe <- gadm2_aqli_2023 %>%
  filter(country %in% unlist(european_countries)) %>%
  mutate(region = if_else(country %in% eu_countries, "European Union Member", "Not European Union Member"))

# creating region wise average PM2.5 data from 1998 to 2023
europe_fs_fig2_regions <- gadm2_aqli_2023_europe %>%
  group_by(region) %>%
  filter(!is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)

europe_fs_fig2_data <- gadm2_aqli_2023_europe %>%
  filter(!is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "Europe") %>%
  select(years, region, pop_weighted_avg_pm2.5)

europe_fs_fig2_dataset <- bind_rows(europe_fs_fig2_regions, europe_fs_fig2_data)

europe_fs_fig2_dataset$region = factor(europe_fs_fig2_dataset$region, levels = c("Not European Union Member", "Europe", "European Union Member"))

# fig 2
europe_fs_fig2 <- europe_fs_fig2_dataset %>%
  ggplot() +
  geom_line(mapping = aes(x = years, y = pop_weighted_avg_pm2.5,
                          color = interaction(region),
                          linetype = interaction(region)), lwd = 1.3) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted", color = "lightgrey") +
  geom_hline(mapping = aes(yintercept = 10), lwd = 0.8, linetype = "dotted", color = "darkgrey") +
  scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 20)) +
  scale_x_continuous(breaks = c(seq(1998, 2021, 2), 2023)) +
  scale_color_manual(values = c("Europe" = "#b7ebf1",
                                "Not European Union Member" = "#8fd8e4",
                                "European Union Member" = "#b7ebf1")) +
  scale_linetype_manual(values = c("Europe" = "solid",
                                   "Not European Union Member" = "dashed",
                                   "European Union Member" = "dotted")) +
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
        legend.key.width = unit(2, "cm")) +
  geom_text(aes(x = 2002.75, y = 5.7,
                label = "WHO~PM[2.5]~Guideline~(last~updated:~2021):~5~µg/m^3"),
            parse = TRUE, size = 4.5) +
  geom_text(aes(x = 2002, y = 10.7,
                label = "European~Union~PM[2.5]~2030~targets:~10~µg/m^3"),
            parse = TRUE, size = 4.5)
