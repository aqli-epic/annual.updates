# read in the helper file2
source("~/R/july.2026.helper.script.R")

# Central and West Africa figure 3.2 ============
gbd_results_cwafrica_fig <- gbd_results_master_2026 %>%
  filter(cause_of_death %in% c("PM2.5 relative to WHO guideline",
                               "Neglected tropical diseases and malaria", 
                               "Unsafe water, sanitation, and handwashing", 
                               "HIV/AIDS and sexually transmitted infections"), 
         country %in% c("Cameroon", "Rwanda","Burundi", "Republic of the Congo",
                        "Democratic Republic of the Congo"))

# making the 'location' column of type factor
gbd_results_cwafrica_fig$country <- factor(gbd_results_cwafrica_fig$country, 
                                              levels = c("Cameroon", "Rwanda","Burundi", "Republic of the Congo",
                                                         "Democratic Republic of the Congo"))

# Rename Democratic Republic of the Congo to DRC
#gbd_results_cwafrica_fig$country <-  str_replace(gbd_results_cwafrica_fig$country, 
#                                                    "Democratic Republic of the Congo", "DR Congo")
gbd_results_cwafrica_fig <- gbd_results_cwafrica_fig %>%
  mutate(country = case_when(
    country %in% c("Congo, Democratic Republic of the",
                   "Democratic Republic of Congo",
                   "DR Congo") ~ "Democratic Republic of the Congo",
    TRUE ~ country
  ))
# Converting 'cause_of_death' to type factor
gbd_results_cwafrica_fig$cause_of_death <- as.factor(gbd_results_cwafrica_fig$cause_of_death)

# Rearranging 'cause of death' levels
levels(gbd_results_cwafrica_fig$cause_of_death) <- c("HIV/AIDS and sexually transmitted infections", 
                                                        "Neglected tropical diseases and malaria", 
                                                        "PM2.5 relative to WHO guideline", 
                                                        "Unsafe water, sanitation, and handwashing")

# wrapping x-axis labels text 
levels(gbd_results_cwafrica_fig$cause_of_death) <- str_wrap(levels(gbd_results_cwafrica_fig$cause_of_death), 30)

# getting country wise population
country_wise_population <- gadm2_aqli_2023 %>%
  filter(country %in% central_and_west_african_countries) %>%
  gadm_level_summary(c("country"), c(2023), 10) %>%
  filter(country %in% c("Cameroon", "Rwanda","Burundi", "Republic of the Congo",
                        "Democratic Republic of the Congo")) %>%
  arrange(desc(population))

# reorder within each location as per the total life years lost column
gbd_results_cwafrica_fig <- gbd_results_cwafrica_fig %>% 
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column 
gbd_results_cwafrica_fig <- gbd_results_cwafrica_fig %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))

# plot 
ar_cw_africa_fig <- gbd_results_cwafrica_fig %>%
  ggplot(mapping = aes(x = reorder_within(cause_of_death, lyl, country), y = lyl)) + 
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  facet_wrap(~factor(country, levels = c("Cameroon", "Rwanda","Burundi", "Republic of the Congo",
                                         "Democratic Republic of the Congo")), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("#5e92a9", "#8ea75b", "#8F3931", "#f29e37")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "", 
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
  theme(axis.text.x = element_blank(), legend.position = "bottom", axis.ticks = element_blank(), 
        strip.text = element_text(size = 14), 
        plot.background = element_rect(fill = "white", color = "white"))