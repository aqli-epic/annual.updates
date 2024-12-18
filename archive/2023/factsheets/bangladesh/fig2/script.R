# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")

#> plot 2: Potential Gain in Life Expectancy from Reducing PM2.5 from 2021 to the WHO Guideline in 10 most populous regions of Bangladesh -----------------------------------------

bangladesh_fs_fig2_dataset <- gadm2_aqli_2021 %>%
  filter(country == "Bangladesh") %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "llpp_who_2021") %>%
  slice_max(population, n = 10)

bangladesh_fs_fig2 <- bangladesh_fs_fig2_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "name_2", y_var = "llpp_who_2021", title = "", subtitle = "", x_label = "District", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") +
  theme(plot.background = element_rect(fill = "white", color = "white"))




