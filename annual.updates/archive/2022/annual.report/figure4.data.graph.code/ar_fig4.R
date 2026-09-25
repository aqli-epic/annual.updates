# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

# AR figure 4 code--------------------------------------------------------------

#> Figure 4: Life Expectancy Impact of PM2.5 and Unassociated Causes/Risks of Death, Global

# prepare the gbd figure dataset keeping relevant causes of death and filtering out others
ar_fig4_dataset <- gbd_results %>%
  filter(location == "global",
         cause_of_death %in% c("PM2.5 relative to WHO", "Smoking",
                               "Alcohol use", "Unsafe water, sanitation, and handwashing",
                               "Road injuries", "HIV/AIDS",
                               "Malaria", "Conflict and terrorism")) %>%
  select(cause_of_death, est_le_diff_vs_actual_aggregate)

# plot the gbd figure
ar_fig4 <- ggplot(gbd_fig_4_dataset, mapping = aes(x = fct_reorder(cause_of_death, est_le_diff_vs_actual_aggregate, .desc = TRUE), y = est_le_diff_vs_actual_aggregate)) +
  geom_col(mapping = aes(fill = est_le_diff_vs_actual_aggregate), width = 0.5) +
  theme_classic() +
  labs(x = "", y = "Life Years Lost") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0.90),
        legend.position = "none") +
  ggtitle(str_c("Life Expectany Impact of Mortality Causes & Risks:", "Global", sep = " ")) +
  scale_fill_gradient(low = "darkgoldenrod1", high = "darkorange")

# save the plot in the appropriate folder
ggsave("./june.2022/annual.report/figure4.data.graph/ar_fig4.png", ar_fig4)

# save the dataset that was reduced to generate the figure
write_csv(ar_fig4_dataset , "./june.2022/annual.report/figure4.data.graph/ar_fig4_data.csv")

