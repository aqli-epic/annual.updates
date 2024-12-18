# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


#> plot 3----------------------------------------------

# calculate the x most populous states
x_most_populous_us_states <- gadm2_aqli_2021 %>%
  filter(country == "United States") %>%
  gadm_level_summary(c("country", "name_1"), c(2021), 10) %>%
  slice_max(population, n = 11) %>%
  select(name_1) %>%
  unlist() %>%
  as.vector()
 
# calculate pollution in the above states for the year 2021
us_fs_fig3_dataset_part1 <- gadm2_aqli_2021 %>%
  filter(country == "United States") %>%
  gadm_level_summary(c("country", "name_1"), c(2021), 10) %>%
  filter(name_1 %in% x_most_populous_us_states) %>%
  select(country, name_1, pm2021)

# calculate pollution in the above states for the year 1970
us_fs_fig3_dataset_part2 <- us_1970_calc_results_cleaned %>%
  gadm_level_summary(c("country", "name_1"), c(1970), 10) %>%
  filter(name_1 %in% x_most_populous_us_states) %>%
  select(country, name_1, pm1970)

# final data set for figure 3
us_fs_fig3_final_data <- us_fs_fig3_dataset_part1 %>%
  left_join(us_fs_fig3_dataset_part2, by = c("country", "name_1")) %>%
  mutate(lyg_1970_2021 = round((pm1970 - pm2021)*0.098, 2)) %>%
  add_aqli_color_scale_buckets(scale_type = "lyldiff", "lyg_1970_2021")

# figure 3
us_fs_fig3 <- us_fs_fig3_final_data %>%
  filter(name_1 != "Georgia") %>%
   ggplot() +
      geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol("name_1"), !!as.symbol("lyg_1970_2021")), y = !!as.symbol("lyg_1970_2021"), fill = lyldiff_bucket)) +
      labs(x = "State", y = "Life years gained", title = "", subtitle = "", caption = "", fill = "Life years gained") +   
  themes_aqli_base + 
   scale_fill_manual(values = c("< -2" = "#4575b4", 
                               "-2 to (< -0.5)" = "#74add1", 
                               "-0.5 to (< -0.1)" = "#abd9e9", 
                               "-0.1 to (< 0)" = "#e0f3f8", 
                               "0 to (< 0.1)" = "#fee090", 
                               "0.1 to (< 0.5)" = "#fdae61", 
                               "0.5 to (< 2)" = "#f46d43", 
                               ">= 2" = "#d73027")) +
      coord_flip() +
  theme(legend.position = "bottom", 
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) 
  


