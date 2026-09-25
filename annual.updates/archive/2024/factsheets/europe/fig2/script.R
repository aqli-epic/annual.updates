# read in the helper file
source("R/july.2024.helper.script.R")

#Figure 2:  Potential gain in life expectancy from reducing PM2.5 from 2022 levels to the WHO guideline in the 10 most populous regions of Europe

# create a europe AQLI dataset
gadm2_aqli_2022_europe <- gadm2_aqli_2022 %>%
  filter(country %in% european_countries$country)

# europe pop
europe_pop <- sum(gadm2_aqli_2022_europe$population, na.rm =  TRUE)

# pop living above who guideline
europe_above_who <- gadm2_aqli_2022_europe %>%
  filter(pm2022 > 10) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE))


# country wise average and how many are above WHO
europe_country_wise <- gadm2_aqli_2022_europe %>%
  gadm_level_summary(c("country"), c(2022), 10)


# exclude the following countries to keep the map less wide and to show a stark difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", "Armenia", "Cyprus", "Northern Cyprus", "Svalbard and Jan Mayen")


countries_except_excluded <- european_countries %>%
  filter(country %notin% exclude_countries)


# europe figure 2 dataset
 europe_fs_fig2_dataset <- gadm2_aqli_2022_europe %>%
  mutate(name_1_country = str_c(name_1, " (", country, ")")) %>%
  group_by(name_1_country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2022_pop_weighted = pop_weights*pm2022) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2022_pop_weighted, na.rm = TRUE),
            gain_in_le = (avg_pm2.5_pop_weighted - 5) * 0.098,
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_pop, n = 10)
 # Convert name_1_country to character
 europe_fs_fig2_dataset$name_1_country <- as.character(europe_fs_fig2_dataset$name_1_country)

 # Convert gain_in_le to numeric
 europe_fs_fig2_dataset$gain_in_le <- as.numeric(europe_fs_fig2_dataset$gain_in_le)

# dataset
europe_fs_fig2_dataset <-  europe_fs_fig2_dataset %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "gain_in_le")

# plt
europe_fs_fig2 <- europe_fs_fig2_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "name_1_country", y_var = "gain_in_le", title = "", subtitle = "", x_label = "Region", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") +
  themes_aqli_base +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 20, color="#222222"),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        axis.line = element_line(),
        legend.text = element_text(size = 20, color="#222222"),
        legend.title = element_text(size = 20, color="#222222"),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),axis.ticks.y = element_blank()) +
   scale_y_continuous(breaks = seq(0, 2, 0.5)) +
   guides(fill = guide_legend(nrow = 1))

