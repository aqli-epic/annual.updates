# read in the helper file
source("~/R/july.2026.helper.script.R")

# Middle East and North Africa figure 4.2 ============
# GBD results filtered for relevant cause of death and countries 
gbd_results_mena_fig <- gbd_results_master_2026 %>%
  filter(cause_of_death %in% c("Child and maternal malnutrition", 
                               "PM2.5 relative to WHO guideline", 
                               "Tobacco", 
                               "Transport injuries"), 
         country %in% c("Kuwait", "Iraq", "Qatar", "Saudi Arabia", "United Arab Emirates"))

# making the 'location' column of type factor
gbd_results_mena_fig$country <- factor(gbd_results_mena_fig$country, 
                                          levels = c("Kuwait", "Iraq", "Qatar", "Saudi Arabia", "United Arab Emirates"))

# Converting 'cause_of_death' to type factor
gbd_results_mena_fig$cause_of_death <- as.factor(gbd_results_mena_fig$cause_of_death)

# Rearranging 'cause of death' levels
levels(gbd_results_mena_fig$cause_of_death) <- c("Child and maternal malnutrition",  
                                                    "PM2.5 relative to WHO guideline", 
                                                    "Tobacco", 
                                                    "Transport injuries")

# wrapping x-axis labels text 
levels(gbd_results_mena_fig$cause_of_death) <- str_wrap(levels(gbd_results_mena_fig$cause_of_death), 30)

# getting country wise population
country_wise_population <- gadm2_aqli_2024 %>%
  filter(country %in% mena_countries) %>%
  gadm_level_summary(c("country"), c(2024), 10) %>%
  filter(country %in% c("Kuwait", "Iraq", "Qatar", "Saudi Arabia", "United Arab Emirates")) %>%
  arrange(desc(population))

# reorder within each location as per the total life years lost column
gbd_results_mena_fig <- gbd_results_mena_fig %>% 
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column 
gbd_results_mena_fig <- gbd_results_mena_fig %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))

# plot 
ar_mena_fig <- gbd_results_mena_fig %>%
  ggplot(mapping = aes(x = reorder_within(cause_of_death, lyl, country), y = lyl)) + 
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  ylim(0,6) +
  facet_wrap(~factor(country, levels = c("Kuwait", "Iraq", "Qatar", "Saudi Arabia", "United Arab Emirates")), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("#5e92a9", "#7f152c" , "#8ea75b","#f29e37")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "", 
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
  theme(axis.text.x = element_blank(), legend.position = "bottom", axis.ticks = element_blank(), 
        strip.text = element_text(size = 14), 
        plot.background = element_rect(fill = "white", color = "white")) 