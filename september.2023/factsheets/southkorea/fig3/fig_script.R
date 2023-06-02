
# read in the helper file (look for appPublic folder in the root of the repository)
source("./appPublic/aqli.data.explorer.helper.script.R")


# plot 3: Trendlines: 1998 to 2021

# southkorea factsheet figure 3 dataset
southkorea_fs_fig3_dataset <- gadm2_aqli_2021 %>%
  filter(country == "South Korea") %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))), 
         region = "National Average") %>% 
  select(years, region, pop_weighted_avg_pm2.5)


southkorea_fs_fig3_dataset %>%
   write_csv("./september.2023/factsheets/southkorea/fig3/sk_fs_fig3_data.csv")


  southkorea_fs_fig3 <- southkorea_fs_fig3_dataset %>%
    ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1, 
            color = "darkred") +
    geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dashed") +
    geom_hline(mapping = aes(yintercept = 15), lwd = 0.8, linetype = "dashed") + 
  scale_y_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) +
  scale_x_continuous(breaks = c(seq(1998, 2020, 2), 2021)) +
    scale_color_manual(values = c("National Average" = "darkgrey")) +
  ggthemes::theme_clean() +
  labs(x = "Years", 
       y = expression("Annual average " ~ PM[2.5] ~ "concentrations (in µg/m³)")) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size = 7), 
        axis.title.y = element_text(size = 9), 
        axis.title.x = element_text(size = 9)) +  
    geom_text(x = 2001.8, y = 6.1, label =  expression("WHO " ~ PM[2.5] ~ "Guideline (last updated in 2021): 5 µg/m³"), size = 5) +
    geom_text(x = 2001.8, y = 16.1, label = expression("South Korea National" ~ PM[2.5] ~ "Guideline: 15 µg/m³"), size = 5) +
    themes_aqli_base


