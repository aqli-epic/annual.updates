# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Pakistan factsheet figure 3: PM2.5 comparison with other causes of death (Global Burden of Disease)----------

# Filter GBD master dataset for Pakistan
gbd_pakistan <- gbd_results %>%
  filter(location == "Pakistan")

# filtering out some causes of death that are not so different from PM2.5
gbd_pakistan_filter <- gbd_pakistan %>%
  filter(cause_of_death %notin% c("Cardiovascular diseases", "Neoplasms", "Tobacco"))

# keeping only top 6 causes and creating the pakistan figure 3 dataset
pak_fs_fig3_dataset <- gbd_pakistan_filter %>%
  slice_max(est_le_diff_vs_actual_aggregate, n = 6)

# pakistan figure 3
pak_fs_fig3 <- ggplot(pak_fs_fig3_dataset) +
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, est_le_diff_vs_actual_aggregate), y = est_le_diff_vs_actual_aggregate, fill = cause_of_death), width = 0.5) +
  labs(x = "Cause of Death", y = "Years Lost") +
  scale_fill_manual(values = c("royalblue", "royalblue",
                               "royalblue", "royalblue", "darkred", "royalblue")) +
  ggthemes::theme_clean() +
  theme(legend.position = "none") +
  coord_flip()

# save pakistan factsheet figure 3
ggsave("./june.2022/factsheets/pakistan/graphs/pak_fs_fig_3.png", pak_fs_fig3)
