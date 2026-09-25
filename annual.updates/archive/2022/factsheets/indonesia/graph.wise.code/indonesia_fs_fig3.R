# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Indonesia factsheet figure 3: PM2.5 comparison with other causes of deaths (GBD)------------------------------

# filtering master GBD file for Indonesia
gbd_indonesia <- gbd_results %>%
  filter(location == "indonesia")

# filtering out some of the GBD causes of deaths that are sort of* covered in PM2.5
gbd_indonesia_filter <- gbd_indonesia %>%
  filter(cause_of_death %notin% c("Cardiovascular diseases", "Neoplasms", "Tobacco"))

# indonesia factsheet figure3 dataset: filtering out top 6 causes of deaths (in terms of years of life lost)
indonesia_fs_fig3_dataset <- gbd_indonesia_filter %>%
  slice_max(est_le_diff_vs_actual_aggregate, n = 6)

# indonesia factsheet figure 3
indonesia_fs_fig3 <- indonesia_fs_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, est_le_diff_vs_actual_aggregate), y = est_le_diff_vs_actual_aggregate, fill = cause_of_death), width = 0.5) +
  labs(x = "Cause of Death", y = "Years Lost") +
  scale_fill_manual(values = c("royalblue", "royalblue",
                               "royalblue", "royalblue", "darkred", "royalblue")) +
  ggthemes::theme_clean() +
  theme(legend.position = "none") +
  coord_flip()

# save figure 3
ggsave("./june.2022/factsheets/indonesia/graphs/indonesia_fs_fig3.png", indonesia_fs_fig3)



