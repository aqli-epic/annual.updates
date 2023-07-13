# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


#> plot 2: Gain in LE in reducing 2021 pollution to WHO guideline in top 10 most populous regions

# fig2 dataset
southkorea_fs_fig2_dataset <- gadm2_aqli_2021 %>%
  filter(country == "South Korea") %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "llpp_who_2021") %>%
  slice_max(population, n = 10)


# fig 2

south_korea_fs_fig2 <- southkorea_fs_fig2_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "name_2", y_var = "llpp_who_2021", title = "", subtitle = "", x_label = "Region", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") +
  theme(plot.background = element_rect(fill = "white", color = "white"))
 

