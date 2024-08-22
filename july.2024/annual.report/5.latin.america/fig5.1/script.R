# read in the helper file
source("R/july.2024.helper.script.R")

# GBD figure 5.1 -----------------------------------------

diseases_list <- c("Child and maternal malnutrition","PM2.5 relative to WHO guideline",
                   "Self-harm and interpersonal violence","Tobacco")

country_list <- c("Bolivia", "Brazil", "Colombia","Guatemala", "Mexico")

# GBD results filtered for relevant cause of death (given Europe) and countries
ar_fig5.1_data <- gbd_results_master_2022 %>%
  filter(cause_of_death %in% diseases_list, country %in% country_list)

# making the 'location' column of type factor
ar_fig5.1_data$country <- factor(ar_fig5.1_data$country,
                                 levels = country_list)

# Converting 'cause_of_death' to type factor
ar_fig5.1_data$cause_of_death <- factor(ar_fig5.1_data$cause_of_death, levels = diseases_list)

# wrapping x-axis labels text
levels(ar_fig5.1_data$cause_of_death) <- str_wrap(levels(ar_fig5.1_data$cause_of_death), 30)

# reorder within each location as per the total life years lost column
ar_fig5.1_data <- ar_fig5.1_data %>%
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column and save the cwafrica factsheet figure 2
ar_fig5.1_data <- ar_fig5.1_data %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))

# plot
ar_fig5.1 <- ar_fig5.1_data %>%
  ggplot(mapping = aes(x = cause_of_death, y = lyl)) +
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  facet_wrap(~factor(country, levels = country_list), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("#5e92a9", "#7f152c" ,"#f29e37", "#8ea75b")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "",
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
  theme(axis.text.x = element_blank(), legend.position = "bottom", axis.ticks = element_blank(),
        strip.text = element_text(size = 14),
        plot.background = element_rect(fill = "white", color = "white"))
