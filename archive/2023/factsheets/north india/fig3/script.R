# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# Filtering color dataset for India
color_2021_india <- gadm2_aqli_2021 %>%
  filter(country == "India") 

# adding a north India/rest of india region column (specifically Indo gangetic plain)
north_india <- c("West Bengal", "Uttar Pradesh", "Punjab", "Haryana", 
                 "Chandigarh", "Bihar", "NCT of Delhi")

# adding North India column
color_2021_india <- color_2021_india %>%
  mutate(region = ifelse(name_1 %in% north_india, "Northern plains of India", "All other regions (excluding Northern plains of India)"))


  
#> plot 3: potential gain in life expectancy in 10 most populous districts of the Indo-Gangetic plains----------------

# fig3 dataset
north_india_fs_fig3_dataset <- gadm2_aqli_2021 %>%
  filter(country == "India", name_1 %in% north_india) %>%
  add_aqli_color_scale_buckets(scale_type = "lyl", col_name = "llpp_who_2021") %>%
  slice_max(population, n = 10)


# fig 3

north_india_fs_fig3 <- north_india_fs_fig3_dataset %>%
  aqli_bar(scale_type = "lyl", x_var = "name_2", y_var = "llpp_who_2021", title = "", subtitle = "", x_label = "District", y_label = "Potential Gain in Life Expectancy (Years)", legend_title = "Potential gain in life expectancy (Years)", caption = "") +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))
 
