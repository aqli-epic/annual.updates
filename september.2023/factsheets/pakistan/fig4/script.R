# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")

# read and filter AQLI data
pak_aqli_2021 <- gadm2_aqli_2021 %>%
  filter(country == "Pakistan") %>%
  mutate(region = case_when(
    name_1 == "Islamabad" ~ "Islamabad",
    name_1 == "Punjab" ~ "Punjab",
    name_1 != "Islamabad"| name_1 != "Punjab" ~ "All other regions (excluding Islamabad and Punjab)"))

# pakistan factsheet figure 4 dataset
pak_region_avg_ts <- pak_aqli_2021 %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)

pak_nat_avg_ts <- pak_aqli_2021 %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

pak_fs_fig4_dataset <- rbind(pak_region_avg_ts, pak_nat_avg_ts)

pak_fs_fig4_dataset$region = factor(pak_fs_fig4_dataset$region, levels = c('Islamabad', 'Punjab', 'National Average', 'All other regions (excluding Islamabad and Punjab)'))

# pakistan factsheet figure 4
pak_fs_fig4 <- pak_fs_fig4_dataset %>%
  ggplot() +
  geom_line(mapping = aes(x = as.integer(years), 
                          y = as.double(pop_weighted_avg_pm2.5),
                          colour = region, linetype = region), 
            lwd = 1.1) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 60)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2021))  +
  scale_color_manual(values = c("Islamabad" = "#3c456f", "Punjab" = "#3c456f", "National Average" = "#4e5e8b", "All other regions (excluding Islamabad and Punjab)" = "#5f7aa5")) +
  scale_linetype_manual(values = c("Islamabad" = "dashed", "Punjab" = "dashed", "National Average" = "solid", "All other regions (excluding Islamabad and Punjab)" = "dotted")) +
  ggthemes::theme_tufte() +
  labs(x = "Year", 
       y = expression("Annual Average" ~ PM[2.5] ~ "Concentration (in µg/m³)")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 13, margin = margin(r = 0.6, unit = "cm")), 
        axis.title.x = element_text(size = 13, margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.line = element_line(), 
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")), 
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"), 
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 12), 
        plot.background = element_rect(color = "white"), 
        axis.ticks = element_blank()) +
  geom_text(x = 2000.6, y = 6.8, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 4.5)
