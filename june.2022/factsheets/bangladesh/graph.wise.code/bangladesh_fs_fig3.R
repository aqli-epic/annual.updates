# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Bangladesh factsheet figure 3: GBD----------------------------

# filtering color dataset and keeping only bangladesh
color_2020_bangladesh <- color_2020 %>%
  filter(country == "Bangladesh")

# filtering out the master GBD file for Bnagladesh
gbd_results_bangladesh <- gbd_results %>%
  filter(location  == "Bangladesh")

# remove categories that are sort of* already covered under the PM2.5 category
gbd_results_bangladesh_filter <- gbd_results_bangladesh %>%
  filter(cause_of_death %notin% c("Cardiovascular diseases", "Neoplasms", "Tobacco",
                                  "Household air pollution from solid fuels"))

# bangladesh factsheet fig3 dataset
bangladesh_fs_fig3_dataset <- gbd_results_bangladesh_filter %>%
  slice_max(est_le_diff_vs_actual_aggregate, n = 6)

# bangladesh factsheet figure 3
bangladesh_fs_fig3 <- ggplot(bangladesh_fs_fig3_dataset) +
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, est_le_diff_vs_actual_aggregate), y = est_le_diff_vs_actual_aggregate, fill = cause_of_death), width = 0.5) +
  labs(x = "Cause of Death", y = "Years Lost") +
  scale_fill_manual(values = c("royalblue", "royalblue",
                               "royalblue", "royalblue", "darkred", "royalblue")) +
  scale_y_continuous(breaks = seq(0, 8, 1)) +
  ggthemes::theme_clean() +
  theme(legend.position = "none") +
  coord_flip()

# save bangladesh factsheet figure 3
ggsave("./june.2022/factsheets/bangladesh/graphs/bangladesh_fs_figure3.png", bangladesh_fs_fig3)



