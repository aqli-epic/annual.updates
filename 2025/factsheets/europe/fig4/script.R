# read in the helper file
source("R/july.2025.helper.script.R")

# Figure 3: GBD in the 5 most populous countries in Europe ------
# europe fig4
diseases_list <- c("PM2.5 relative to WHO guideline",
                   "Diabetes and kidney diseases",
                   "Tobacco",
                   "Transport injuries")
country_list <- c("Bosnia and Herzegovina", "Armenia", "Turkey", "Serbia", "Bulgaria") # five most polluted countries

# GBD results filtered for relevant cause of death (given Europe) and countries
gbd_results_europe_fig4 <- gbd_results_master_2023 %>%
  filter(cause_of_death %in% diseases_list, country %in% country_list)

# making the 'location' column of type factor
gbd_results_europe_fig4$country <- factor(gbd_results_europe_fig4$country, levels = country_list)

# Converting 'cause_of_death' to type factor
gbd_results_europe_fig4$cause_of_death <- factor(gbd_results_europe_fig4$cause_of_death, levels = diseases_list)

# wrapping x-axis labels text
levels(gbd_results_europe_fig4$cause_of_death) <- str_wrap(levels(gbd_results_europe_fig4$cause_of_death), 30)

# reorder within each location as per the total life years lost column
gbd_results_europe_fig4 <- gbd_results_europe_fig4 %>%
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column
gbd_results_europe_fig4 <- gbd_results_europe_fig4 %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))

# plot
europe_fs_fig4 <- gbd_results_europe_fig4 %>%
  ggplot(mapping = aes(x = reorder_within(cause_of_death, lyl, country), y = lyl)) +
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  facet_wrap(~factor(country, levels = country_list), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("#5e92a9", "#8F3931", "#8ea75b", "#f29e37")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "",
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
  theme(legend.position = "bottom",
        legend.justification = c(0.5, 3),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 15, color="#222222"),
        legend.title = element_text(size = 17, color="#222222"),
        axis.title.y = element_text(size = 20, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 20, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 20),
        plot.background = element_rect(fill = "white", color = "white"))
