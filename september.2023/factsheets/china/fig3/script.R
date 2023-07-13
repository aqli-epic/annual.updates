# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")

# fig3 code

china_fs_fig3_dataset <- gadm2_aqli_2021 %>%
  filter(country == "China") %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "llpp_who_2021") %>%
  slice_max(population, n = 10)

china_fs_fig3 <- china_fs_fig3_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "name_2", y_var = "llpp_who_2021", title = "", subtitle = "", x_label = "Prefecture", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") + 
  theme(plot.background = element_rect(fill = "white", color = "white"))



