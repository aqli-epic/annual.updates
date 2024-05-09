# read in the helper file
source("R/july.2024.helper.script.R")

# MENA figure 8.2 ============

# GBD results filtered for relevant cause of death and countries 
gbd_results_mena_fig8.2 <- gbd_results_master_2022 %>%
  filter(cause_of_death %in% c("Neoplasms", "PM2.5 relative to WHO guideline", 
                               "Tobacco", "Transport injuries"), 
         country %in% c("Algeria", "Egypt", "Iraq", "Qatar", "Saudi Arabia"))

# making the 'location' column of type factor
gbd_results_mena_fig8.2$country <- factor(gbd_results_mena_fig8.2$country, 
                                          levels = c("Algeria", "Egypt", "Iraq", "Qatar", "Saudi Arabia"))

# Converting 'cause_of_death' to type factor
gbd_results_mena_fig8.2$cause_of_death <- as.factor(gbd_results_mena_fig8.2$cause_of_death)

# Rearranging 'cause of death' levels
levels(gbd_results_mena_fig8.2$cause_of_death) <- c("Neoplasms", "PM2.5 relative to WHO guideline", "Tobacco", "Transport injuries")

# wrapping x-axis labels text 
levels(gbd_results_mena_fig8.2$cause_of_death) <- str_wrap(levels(gbd_results_mena_fig8.2$cause_of_death), 30)

# getting country wise population
country_wise_population <- gadm2_aqli_2022 %>%
  filter(country %in% mena_countries) %>%
  gadm_level_summary(c("country"), c(2022), 10) %>%
  filter(country %in% c("Algeria", "Egypt", "Iraq", "Qatar", "Saudi Arabia")) %>%
  arrange(desc(population))

# reorder within each location as per the total life years lost column
gbd_results_mena_fig8.2 <- gbd_results_mena_fig8.2 %>% 
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column and save the cwafrica factsheet figure 2
gbd_results_mena_fig8.2 <- gbd_results_mena_fig8.2 %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))

# plot 
ar_mena_fig8.2 <- gbd_results_mena_fig8.2 %>%
  ggplot(mapping = aes(x = reorder_within(cause_of_death, lyl, country), y = lyl)) + 
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  facet_wrap(~factor(country, levels = c("Algeria", "Egypt", "Iraq", "Qatar", 
                                         "Saudi Arabia")), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("#D3D9E0", "#8F3931", "#7BC1D9",
                               "#808A94")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "", 
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
  theme(axis.text.x = element_blank(), legend.position = "bottom", axis.ticks = element_blank(), 
        strip.text = element_text(size = 14), 
        plot.background = element_rect(fill = "white", color = "white")) 
