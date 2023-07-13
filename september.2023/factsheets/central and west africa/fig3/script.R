# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# central africa definition
central_african_countries <- c("Angola", "Burundi", "Cameroon", 
                                        "Central African Republic", "Chad", 
                                        "Republic of the Congo",
                                        "Democratic Republic of the Congo", 
                                        "Equatorial Guinea", "Gabon",
                                        "São Tomé and Príncipe", 
                                        "Rwanda")
# west africa definition
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde", 
                            "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                            "Côte d'Ivoire", "Liberia", "Mali", "Mauritania", 
                            "Niger", "Nigeria", "Senegal", "Sierra Leone", 
                            "Togo")

# central and west africa countries definition, combine in a single vector
central_and_west_african_countries <- c(central_african_countries, west_african_countries)

# central and west african countries
gadm2_aqli_2021_cw_african <- gadm2_aqli_2021 %>%
  filter(country %in% central_and_west_african_countries) %>%
  mutate(name_1_country = str_c(name_1, " (", country, ")", sep = ""), 
         region = ifelse(country %in% central_african_countries, "Central African", 
                         "West African")) 


# central and west africa figure 3 dataset
 cwafrica_fs_fig3_dataset <- gadm2_aqli_2021_cw_african %>%
  filter(!is.na(population)) %>%
  group_by(name_1_country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         pm2021_pop_weighted = pop_weights*pm2021) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_pop_weighted = sum(pm2021_pop_weighted, na.rm = TRUE), 
            gain_in_le = (avg_pm2.5_pop_weighted - 5) * 0.098, 
            gain_in_le = ifelse(gain_in_le < 0, 0, gain_in_le)) %>%
  ungroup() %>%
  slice_max(tot_pop, n = 10) 
 

cwafrica_fs_fig3_dataset <- cwafrica_fs_fig3_dataset %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "gain_in_le") 

cwafrica_fs_fig3 <- cwafrica_fs_fig3_dataset %>%
  mutate(name_1_country = str_replace(name_1_country, "Democratic Republic of the Congo", "DR Congo")) %>%
  aqli_bar(scale_type = "lyl", x_var = "name_1_country", y_var = "gain_in_le", title = "", subtitle = "", x_label = "Region", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") + 
  theme(plot.background = element_rect(fill = "white", color = "white"))

