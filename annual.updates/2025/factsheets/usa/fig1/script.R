# read in the helper file
source("R/july.2025.helper.script.R")

# Figure 1: Annual average PM2.5 levels in US and it's most polluted counties, 1998-2023 ------
# get data
us_fs_fig1_data_pt1 <- gadm2_aqli_2023 %>%
  filter(!is.na(population), country %in% c("United States")) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted , names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "United States") %>%
  select(region, years, pop_weighted_avg_pm2.5)

us_fs_fig1_data_pt2 <- gadm2_aqli_2023 %>%
  filter(!is.na(population), country %in% c("United States"), name_2 != "Fairbanks North Star") %>%
  slice_max(pm2023, n = 10) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted , names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "10 most polluted counties of 2023") %>%
  select(region, years, pop_weighted_avg_pm2.5)

us_fs_fig1_data_pt3 <- gadm2_aqli_2023 %>%
  filter(!is.na(population), country %in% c("United States"), name_2 %in% c("Fairbanks North Star", "Kern")) %>%
  pivot_longer(cols = pm1998:pm2023 , names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  rename("region" = "name_2") %>%
  select(region, years, pop_weighted_avg_pm2.5)

# US figure 1 data
us_fs_fig1_data = bind_rows(us_fs_fig1_data_pt1, us_fs_fig1_data_pt2, us_fs_fig1_data_pt3)

us_fs_fig1_data$region <- factor(us_fs_fig1_data$region, levels = c("United States",
                                                                    "Fairbanks North Star",
                                                                    "Kern",
                                                                    "10 most polluted counties of 2023"))
# plot
us_fs_fig1 <- us_fs_fig1_data %>%
  ungroup() %>%
  ggplot() +
  geom_line(mapping = aes(x = years, y = pop_weighted_avg_pm2.5,
                          color = region,
                          linetype = region),
            lwd = 1.3) +
  labs(x = "Year", y = expression("Annual Average " ~ PM[2.5] ~ " concentrations (in µg/m³)"),
       color = "") +
  ggthemes::theme_fivethirtyeight() +
  scale_color_manual(values = c("United States" = "#b7ebf1",
                                "Fairbanks North Star" = "#8fd8e4",
                                "Kern" = "#aae5ec",
                                "10 most polluted counties of 2023" = "#9ddee8"),
                     name = "legend") +
  scale_linetype_manual(values = c("United States" = "solid",
                                   "Fairbanks North Star" = "dotted",
                                   "Kern" = "dotdash",
                                   "10 most polluted counties of 2023" = "dashed"),
                        name = "legend") +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted", color = "lightgrey") +
  geom_hline(mapping = aes(yintercept = 9), lwd = 0.8, linetype = "dotted", color = "darkgrey") +
  scale_y_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) +
  scale_x_continuous(breaks = c(seq(1998, 2021, 2), 2023)) +
  themes_aqli_base +
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 0.6, unit = "cm")),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.line = element_line(),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(2, "cm")) +
  geom_text(aes(x = 2002.65, y = 5.7,
                label = "WHO~PM[2.5]~Guideline~(last~updated:~2021):~5~µg/m^3"),
            parse = TRUE, size = 4.5) +
  geom_text(aes(x = 2001.25, y = 9.7,
                label = "United~States~PM[2.5]~standard:~10~µg/m^3"),
            parse = TRUE, size = 4.5)
