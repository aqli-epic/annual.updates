# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Nigeria factsheet figure 3: GBD---------------------------------------

# No National Standard for Nigeria
nat_stan_nigeria <- "No National Standard"

# filtering the color file for Nigeria
color_2020_nigeria <- color_2020 %>%
  filter(country == "Nigeria") %>%
  mutate(region = ifelse(name_1 %in% c("Ekiti", "Lagos", "Ogun", "Ondo",
                                       "Osun", "Oyo"), "South-West Nigeria",
                         "All other regions (excluding South-West Nigeria)"))

# getting gbd result for Nigeria from the master file
gbd_nigeria <- gbd_results %>%
  filter(location == "nigeria")

# filter out some of the diseases that are already in some* (need to discuss this) way covered in the PM2.5 category
gbd_nigeria_filter <- gbd_nigeria %>%
  filter(cause_of_death %notin% c("Cardiovascular diseases", "Neoplasms", "Tobacco",
                                  "Lower respiratory infections"))

# choosing the top 6 causes of death, based on "est_le_diff_vs_actual_aggregate" variable
nigeria_fs_fig3_dataset <- gbd_nigeria_filter %>%
  slice_max(est_le_diff_vs_actual_aggregate, n = 6)

# nigeria fs figure 3
nigeria_fs_fig3 <- nigeria_fs_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(cause_of_death, est_le_diff_vs_actual_aggregate), y = est_le_diff_vs_actual_aggregate, fill = cause_of_death), width = 0.5)
# save nigeria factsheet figure 3
ggsave("./june.2022/factsheets/nigeria/graphs/nigeria_fs_fig3.png", nigeria_fs_fig3)
