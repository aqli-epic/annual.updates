# read in the helper file
source("~/R/july.2025.helper.script.R")


#Figure 3: Annual average PM2.5 concentrations in Bangladesh, 1998-2023-------------------------------

# bangladesh figure 3 data
bangladesh_fs_fig3_dataset <- gadm2_aqli_2023 %>%
  filter(country == "Bangladesh", !is.na(population)) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# bangladesh figure 3
bangladesh_fs_fig3 <- bangladesh_fs_fig3_dataset %>%
  ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
            color = "#1a1638") +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dashed") +
  geom_hline(mapping = aes(yintercept = 35), lwd = 0.8, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_x_continuous(breaks = c(seq(1998, 2020, 2), 2023)) +
  scale_color_manual(values = c("National Average" = "#416891")) +
  ggthemes::theme_clean() +
  themes_aqli_base +
  labs(x = "Year",
       y = expression("Annual Average  " ~ PM[2.5] ~ " Concentration (in µg/m³)"),
       title = "") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 24, color="#222222"),
        axis.line = element_line(),
        axis.text = element_text(size = 20, color="#222222"),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
        plot.background = element_rect(fill = "white", color = "white")) +
  geom_text(x = 2004.8, y = 6.7, label =  expression("WHO " ~ PM[2.5] ~ " Guideline (last updated in 2021): 5 µg/m³"), size = 7.5) +
  geom_text(x = 2003.2, y = 36.7, label = expression("Bangladesh National" ~ PM[2.5] ~ "Standard: 35 µg/m³"), size = 7.5)