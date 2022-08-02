# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> India factsheet figure 2: PM2.5 comparison with other causes of death (GBD)----------

# GBD results for India
gbd_india <- gbd_results %>%
  filter(location == "India")

# filtering out some of the causes of deaths that are similar to PM2.5
gbd_india_filter <- gbd_india %>%
  filter(cause_of_death %notin% c("Cardiovascular diseases", "Neoplasms", "Tobacco"))

# figure 3 dataset: choosing top 6 causes of deaths from the remaining causes of deaths
india_fs_fig3_dataset <- gbd_india_filter %>%
  slice_max(est_le_diff_vs_actual_aggregate, n = 6)

# figure 3 plot
india_fs_fig3 <- india_fs_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, est_le_diff_vs_actual_aggregate), y = est_le_diff_vs_actual_aggregate, fill = cause_of_death), width = 0.5) +
  labs(x = "Cause of Death", y = "Years Lost") +
  scale_fill_manual(values = c("royalblue", "royalblue",
                               "royalblue", "royalblue", "darkred", "royalblue")) +
  ggthemes::theme_clean() +
  theme(legend.position = "none") +
  coord_flip()

# save india fs figure 3
ggsave("./june.2022/factsheets/india/graphs/india_fs_fig3.png", india_fs_fig3)
