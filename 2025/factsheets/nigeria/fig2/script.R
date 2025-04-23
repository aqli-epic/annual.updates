# read in the helper file
source("R/july.2025.helper.script.R")

# Figure 2: Average PM2.5 concentration in Nigeria from 1998 to 2023 ------
# adding a niger river delta/rest of nigeria region column
niger_river_delta <- c("Rivers", "Delta", "Akwa Ibom", "Imo", "Edo", "Ondo",
                       "Cross River", "Abia", "Bayelsa")

nigeria_fs_fig2_data_part1 <- gadm2_aqli_2023 %>%
  filter(country == "Nigeria", !is.na(population)) %>%
  mutate(region = ifelse(name_1 %in% niger_river_delta, "Niger River Delta", "All other regions (excluding Niger River Delta)"))

# creating Nigeria's region wise average PM2.5 data from 1998 to 2023
nigeria_fs_fig2_data_part1 <- nigeria_fs_fig2_data_part1 %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)

# creating a national average PM2.5 trendlines data from 1998 to 2023
nigeria_fs_fig2_data_part2 <- gadm2_aqli_2023 %>%
  filter(country == "Nigeria") %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# Nigeria factsheet figure 2 dataset
nigeria_fs_fig2_data <- bind_rows(nigeria_fs_fig2_data_part1, nigeria_fs_fig2_data_part2)

# Nigeria factsheet figure 2
nigeria_fs_fig2_data$region <- factor(nigeria_fs_fig2_data$region,
                                      levels = c("Niger River Delta",
                                                 "National Average",
                                                 "All other regions (excluding Niger River Delta)"))

nigeria_fs_fig2 <- ggplot(nigeria_fs_fig2_data) +
  geom_line(mapping = aes(x = years, y = pop_weighted_avg_pm2.5,
                          color = interaction(region),
                          linetype = interaction(region)), lwd = 1.3) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted", color = "lightgrey") +
  geom_hline(mapping = aes(yintercept = 20), lwd = 0.8, linetype = "dotted", color = "darkgrey") +
  scale_y_continuous(breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_x_continuous(breaks = c(seq(1998, 2021, 2), 2023)) +
  scale_color_manual(values = c("Niger River Delta" = "#3db1c8",
                                "National Average" =  "#66c4d6",
                                "All other regions (excluding Niger River Delta)" = "#66c4d6"),
                     name = "legend") +
  scale_linetype_manual(values = c("Niger River Delta" = "dashed",
                                   "National Average" = "solid",
                                   "All other regions (excluding Niger River Delta)" = "dotted"),
                        name = "legend")  +
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
  geom_text(x = 2002.5, y = 7.6, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 4.5) +
  geom_text(x = 2001.5, y = 22, label = expression("Nigeria National" ~ PM[2.5] ~ "Standard: 20 µg/m³"), size = 4.5)
