# read in the helper file
source("~/R/july.2025.helper.script.R")

# Figure 4: Annual average PM2.5 concentrations in LATAM, including 5 countries
latam_fs_fig4_dataset <- gadm2_aqli_2023 %>%
  filter(country %in% latin_america_countries_vec, !is.na(population)) %>%
  mutate(pop_weights = population / sum(population, na.rm = TRUE)) %>%
  mutate(across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(str_extract(years, "\\d+")),
         region = "Latin America") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# Compute country-specific data for the 5 countries
latam_fs_country_data <- gadm2_aqli_2023 %>%
  filter(country %in% c("Bolivia", "Honduras", "El Salvador", "Guatemala", "Peru"), !is.na(population)) %>%
  group_by(country) %>%
  mutate(pop_weights = population / sum(population, na.rm = TRUE)) %>%
  mutate(across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted")) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(str_extract(years, "\\d+")),
         region = country) %>%
  select(years, region, pop_weighted_avg_pm2.5)

# Combine both datasets
final_latam_data <- bind_rows(latam_fs_fig4_dataset, latam_fs_country_data)

# Plot the data
# Corrected Plotting Snippet
latam_fs_fig4 <- final_latam_data %>%
  ggplot() +
  geom_line(aes(x = years, y = pop_weighted_avg_pm2.5, color = region, linetype=region), lwd = 1.1) +
  geom_hline(yintercept = 5, lwd = 0.8, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) +
  scale_x_continuous(breaks = c(seq(1998, 2020, 2), 2023)) +
  scale_color_manual(values = c("Latin America" = "#8fd8e4",
                                "Bolivia" = "#3db1c8",
                                "Honduras" = "#66c4d6",
                                "El Salvador" = "grey",
                                "Guatemala" =  "lightgrey",
                                "Peru" = "cyan"), name = "legend") +
  scale_linetype_manual(
    values = c("Latin America"  = "solid", "Bolivia" = "dashed", "Honduras" = "longdash", "El Salvador" = "dotted", "Guatemala" = "dashed", "Peru" = "dashed"), name = "legend") +
  ggthemes::theme_clean() +
  themes_aqli_base +
  labs(x = "Year",
       y = expression("Annual Average  " ~ PM[2.5] ~ " Concentration (µg/m³)"),
       title = "") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 24, color="#222222"),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        axis.line = element_line(),
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"),
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 20, color="#222222"),
        plot.background = element_rect(color = "white", fill = "white"),
        axis.ticks = element_blank()) +
  geom_text(x = 2003.54, y = 6.1, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 7.5)