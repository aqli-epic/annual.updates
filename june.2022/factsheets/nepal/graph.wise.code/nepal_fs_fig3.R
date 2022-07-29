# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

#> Nepal factsheet figure 3: GBD

# filtering the color dataset for Nepal
color_2020_nepal <- color_2020 %>%
  filter(country == "Nepal")

# filtering gbd results for Nepal
gbd_results_nepal <- gbd_results %>%
  filter(location == "Nepal")

# filtering out those causes of death that are sort of* covered under PM2.5 in some broad way
gbd_results_nepal_filter <- gbd_results_nepal %>%
  filter(cause_of_death %notin% c("Cardiovascular diseases", "Neoplasms", "Tobacco",
                                  "Household air pollution from solid fuels"))

nepal_fs_fig3_dataset <- gbd_results_nepal_filter %>%
  slice_max(est_le_diff_vs_actual_aggregate, n = 6)

# nepal factsheet figure 3
nepal_fs_fig3 <- nepal_fs_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, est_le_diff_vs_actual_aggregate), y = est_le_diff_vs_actual_aggregate, fill = cause_of_death), width = 0.5) +
  labs(x = "Cause of Death", y = "Years Lost") +
  scale_fill_manual(values = c("royalblue", "royalblue",
                               "royalblue", "royalblue", "darkred", "royalblue")) +
  ggthemes::theme_clean() +
  theme(legend.position = "none") +
  coord_flip()

# save nepal fs figure 3
ggsave("./june.2022/factsheets/nepal/graphs/nepal_fs_fig3.png", nepal_fs_fig3)
