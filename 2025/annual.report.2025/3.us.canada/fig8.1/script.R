# read in the helper file
source("R/july.2025.helper.script.R")

# US, Canada, Europe figure 8.1 ============
# US + Canada trendlines data
us_can_trendlines_fig8.1 <- gadm2_aqli_2023 %>%
  filter(!is.na(population), country %in% c("United States", "Canada")) %>%
  group_by(country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted , names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(country, years, pop_weighted_avg_pm2.5)

# set country as factor for correct legend order
us_can_trendlines_fig8.1$country <- factor(us_can_trendlines_fig8.1$country, levels = c("United States", "Canada"))

# plot
ar_us_can_fig8.1 <- us_can_trendlines_fig8.1 %>%
  ungroup() %>%
  ggplot() +
  geom_line(mapping = aes(x = years, y = pop_weighted_avg_pm2.5,
                          color = country,
                          linetype = country),
            lwd = 2) +
  labs(x = "Year", y = expression("Annual Average " ~ PM[2.5] ~ " concentrations (in µg/m³)"),
       color = "") +
  ggthemes::theme_fivethirtyeight() +
  scale_color_manual(values = c("Canada" = "#800026", "United States" = "#f29e37"),
                     name = "legend") +
  scale_linetype_manual(values = c("Canada" = "dashed", "United States" = "dashed"),
                        name = "legend") +
  scale_y_continuous(breaks = seq(0, 15, 3), limits = c(0, 15)) +
  scale_x_continuous(breaks = c(seq(1998, 2023, 1))) +
  themes_aqli_base +
  theme(axis.title.x = element_text(size = 20, margin = margin(r = 0.3, unit = "cm")),
        axis.title.y = element_text(size = 20, margin = margin(r = 0.3, unit = "cm")),
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        plot.background = element_rect(fill = "white", color = "white"))
