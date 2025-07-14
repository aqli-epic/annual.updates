# read in the helper file
source("C:/Users/HP/Downloads/annual.updates/R/july.2025.helper.script.R")

# Latin America figure 9.1 ===========
# GBD results filtered for relevant cause of death and countries
country_list <-c("Australia", "Papua New Guinea","New Zealand","Fiji","Solomon Islands")
diseases_list <- c("Tobacco","Child and maternal malnutrition", "Self-harm and interpersonal violence","PM2.5 relative to WHO guideline")
ar_oceania_fig9.1_data <- gbd_results_master_2025 %>%
  filter(cause_of_death %in% diseases_list, 
         country %in% country_list)

# making the 'location' column of type factor
ar_oceania_fig9.1_data$country <- factor(ar_oceania_fig9.1_data$country,
                                       levels = country_list)

# Converting 'cause_of_death' to type factor
ar_oceania_fig9.1_data$cause_of_death <- factor(ar_oceania_fig9.1_data$cause_of_death, levels = diseases_list)

# wrapping x-axis labels text
levels(ar_oceania_fig9.1_data$cause_of_death) <- str_wrap(levels(ar_oceania_fig9.1_data$cause_of_death), 30)

# reorder within each location as per the total life years lost column
ar_oceania_fig9.1_data <- ar_oceania_fig9.1_data %>%
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column 
ar_oceania_fig9.1_data <- ar_oceania_fig9.1_data %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))

# plot
ar_oceania_fig9.1 <- ar_oceania_fig9.1_data %>%
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
        strip.text = element_text(size = 14),
        plot.background = element_rect(fill = "white", color = "white"))
ggsave("C:/Users/HP/Downloads/annual.report.2025/9.Oceania/fig9.1/ar_oceania_fig9.1.png", ar_oceania_fig9.1, width = 15, height = 10)
svglite("ar_oceania_fig9.1")
ggsave("C:/Users/HP/Downloads/annual.report.2025/9.Oceania/fig9.1/ar_oceania_fig9.1.svg", width = 15, height = 10)