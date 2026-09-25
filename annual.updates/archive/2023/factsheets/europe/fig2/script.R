# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# create a europe AQLI dataset
gadm2_aqli_2021_europe <- gadm2_aqli_2021 %>%
  filter(country %in% european_countries$Country)


# europe pop
europe_pop <- sum(gadm2_aqli_2021_europe$population, na.rm =  TRUE)

# pop living above who guideline
europe_above_who <- gadm2_aqli_2021_europe %>%
  filter(pm2021 > 10) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE))


# country wise average and how many are above WHO
europe_country_wise <- gadm2_aqli_2021_europe %>%
  gadm_level_summary(c("country"), c(2021), 10)


# exclude the following countries to keep the map less wide and to show a stark difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", "Armenia", "Cyprus", "Northern Cyprus", "Svalbard and Jan Mayen")


countries_except_excluded <- european_countries %>% 
  filter(Country %notin% exclude_countries)




# europe figure 2 dataset
 europe_fs_fig2_dataset <- gadm2_aqli_2021_europe %>%
  mutate(name_1_country = str_c(name_1, " (", country, ")")) %>%
  group_by(name_1_country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         pm2021_pop_weighted = pop_weights*pm2021) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2021_pop_weighted, na.rm = TRUE), 
            gain_in_le = (avg_pm2.5_pop_weighted - 5) * 0.098, 
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_pop, n = 10) 
 

# dataset
europe_fs_fig2_dataset <-  europe_fs_fig2_dataset %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "gain_in_le") 

# plt
europe_fs_fig2 <- europe_fs_fig2_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "name_1_country", y_var = "gain_in_le", title = "", subtitle = "", x_label = "Region", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") +
  theme(plot.background = element_rect(fill = "white", color = "white"))


