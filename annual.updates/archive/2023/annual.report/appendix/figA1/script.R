# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# appendix section graph (Fig A.1)====================================

pm_weighted_col_start <- "pm1998_weighted"
pm_weighted_col_end <- "pm2021_weighted"
fig_a1_plt_part1 <- trendlines_aqli_data <- gadm2_aqli_2021 %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::mutate(country = "foo") %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "National Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5) %>%
  mutate(ref_dataset = "2021 dataset")


pm_weighted_col_end <- "pm2020_weighted"
fig_a1_plt_part2 <- trendlines_aqli_data <- color_2020 %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::mutate(country = "foo") %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "National Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5) %>%
  mutate(ref_dataset = "2020 dataset")

pm_weighted_col_end <- "pm2019_weighted"
fig_a1_plt_part3 <- trendlines_aqli_data <- color_2019 %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::mutate(country = "foo") %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "National Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5) %>%
  mutate(ref_dataset = "2019 dataset")

pm_weighted_col_end <- "pm2016_weighted"
fig_a1_plt_part4 <- trendlines_aqli_data <- color_2016 %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::mutate(country = "foo") %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "National Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5) %>%
  mutate(ref_dataset = "2016 dataset")



final_fig_a1_dataset <- rbind(fig_a1_plt_part1, fig_a1_plt_part2, fig_a1_plt_part3, fig_a1_plt_part4)

plt <- final_fig_a1_dataset %>%
  ggplot(mapping = aes(x = years, y = pop_weighted_avg_pm2.5, color = ref_dataset)) +
  geom_line( size = 1.5) +
  scale_y_continuous(breaks = seq(0, 40, 5), limits = c(0, 40)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2021)) +
  themes_aqli_base +
  scale_color_viridis(discrete = TRUE) +
  # scale_color_manual(values = c("2016 dataset" = "#7197be",
  #                               "2019 dataset" =  "#5f7aa5", 
  #                               "2020 dataset" = "#7197be", 
  #                               "2021 dataset" = "#7197be")) +
  scale_linetype_manual(values = c("2016 dataset" = "dashed",
                                "2019 dataset" =  "dashed",
                                "2020 dataset" = "dashed",
                                "2021 dataset" = "solid")) +
  labs(x = "Year", y = expression("Annual Average " ~ PM[2.5] ~ " Concentration (in µg/m³)"), color = "") +
  theme(legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(1, "cm"), 
        plot.background = element_rect(color = "white", fill = "white")) 

