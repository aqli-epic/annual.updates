
# read in the helper file (look for appPublic folder in the root of the repository)
source("./appPublic/aqli.data.explorer.helper.script.R")

#> plot 2: Gain in LE in reducing 2021 pollution to WHO guideline in top 10 most populous regions

# fig2 dataset
southkorea_fs_fig2_dataset <- gadm2_aqli_2021 %>%
  filter(country == "South Korea") %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "llpp_who_2021") %>%
  slice_max(population, n = 10)

southkorea_fs_fig2_dataset %>%
   write_csv("./september.2023/factsheets/southkorea/fig2/sk_fs_fig2_data.csv")


# fig 2

plt <- southkorea_fs_fig2_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "name_2", y_var = "llpp_who_2021", title = expression("Potential Gain in Life Expectancy from Reducing 2021 " ~ PM[2.5] ~ "concentrations to the WHO Guideline"), subtitle = "(Top 10 most populous regions of South Korea)", x_label = "Region", y_label = "Average Life Expectancy Gains (years)", legend_title = "LE gains (years)", caption = "")


