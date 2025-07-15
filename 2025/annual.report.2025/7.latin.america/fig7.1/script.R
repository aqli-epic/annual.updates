# read in the helper file
source("~/R/july.2025.helper.script.R")

# Latin America figure 7.1 ===========
# GBD results filtered for relevant cause of death and countries
country_list <-c("Bolivia", "Honduras", "El Salvador", "Brazil", "Mexico")
diseases_list <- c("Tobacco",
                   "Child and maternal malnutrition",
                   "Self-harm and interpersonal violence",
                   "PM2.5 relative to WHO guideline")
ar_latam_fig7.1_data <- gbd_results_master_2023 %>%
  filter(cause_of_death %in% diseases_list,
         country %in% country_list)

# making the 'location' column of type factor
ar_latam_fig7.1_data$country <- factor(ar_latam_fig7.1_data$country,
                                       levels = country_list)

# Converting 'cause_of_death' to type factor
ar_latam_fig7.1_data$cause_of_death <- factor(ar_latam_fig7.1_data$cause_of_death, levels = diseases_list)

# wrapping x-axis labels text
levels(ar_latam_fig7.1_data$cause_of_death) <- str_wrap(levels(ar_latam_fig7.1_data$cause_of_death), 30)

# reorder within each location as per the total life years lost column
ar_latam_fig7.1_data <- ar_latam_fig7.1_data %>%
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column
ar_latam_fig7.1_data <- ar_latam_fig7.1_data %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))

# plot
ar_latam_fig7.1 <- ar_latam_fig7.1_data %>%
  ggplot(mapping = aes(x = reorder_within(cause_of_death, lyl, country), y = lyl)) +
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  ylim(0,3) +
  facet_wrap(~factor(country, levels = country_list), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("#5e92a9", "#7f152c" , "#8ea75b","#f29e37")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "",
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
  theme(axis.text.x = element_blank(), legend.position = "bottom", axis.ticks = element_blank(),
        axis.title.y = element_text(size = 20, margin = margin(r = 0.3, unit = "cm")),
        axis.text = element_text(size = 17),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 17),
        strip.text = element_text(size = 20),
        plot.background = element_rect(fill = "white", color = "white"))
