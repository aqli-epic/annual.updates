# read in the helper file
source("C:/Users/HP/Downloads/annual.updates/R/july.2025.helper.script.R")

# southeast asia definition
se_asia_def <-  c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia",
                  "Laos", "Malaysia", "Philippines", "Singapore", "Thailand",
                  "Vietnam")

# figure 5.2 ------------

#filter for relevant countries
country_list <- c("Indonesia", "Malaysia", "Myanmar", "Thailand", "Vietnam")
# filter for relevant cause of death
diseases_list <- c("High fasting plasma glucose","PM2.5 relative to WHO guideline",
                   "Tobacco", "Transport injuries")

ar_se_asia_fig5.2_data <- gbd_results_master_2025 %>%
  filter(cause_of_death %in% diseases_list, country %in% country_list)

# making the 'location' column of type factor
ar_se_asia_fig5.2_data$country <- factor(ar_se_asia_fig5.2_data$country,
                                         levels = country_list)

# Converting 'cause_of_death' to type factor
ar_se_asia_fig5.2_data$cause_of_death <- factor(ar_se_asia_fig5.2_data$cause_of_death, levels = diseases_list)

# wrapping x-axis labels text
levels(ar_se_asia_fig5.2_data$cause_of_death) <- str_wrap(levels(ar_se_asia_fig5.2_data$cause_of_death), 30)

# reorder within each location as per the total life years lost column
ar_se_asia_fig5.2_data <- ar_se_asia_fig5.2_data %>%
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column
ar_se_asia_fig5.2_data <- ar_se_asia_fig5.2_data %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))

# southeast asia figure 5.2: GBD
ar_fig5.2 <- ar_se_asia_fig5.2_data %>%
  ggplot(mapping = aes(x = cause_of_death, y = lyl)) +
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  facet_wrap(~factor(country, levels = country_list), scales = "free_x", ncol = 5) +
  scale_fill_manual(labels = ~ stringr::str_wrap(.x, width = 30),
                    values = c("#5e92a9", "#7f152c" , "#8ea75b","#f29e37")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "",
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 14),
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_y_continuous(breaks = seq(0, 7, 0.5))
ggsave("C:/Users/HP/Downloads/annual.report.2025/5.southeast.asia/fig5.2/ar__se_asia_fig5.2_1.png", ar_fig5.2, width = 15, height = 10)

