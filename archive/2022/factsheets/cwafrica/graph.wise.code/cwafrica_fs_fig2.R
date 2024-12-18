# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Central and West Africa factsheet figure 2: Life Expectancy Impacts of Particulate Pollution and Other Health Threats in the
# Five Most Populous Countries in Central and West Africa----------

# filtering color 2020 data for central and west african countries
color_data_cen_west_africa <- color_2020 %>%
  filter(country %in% central_and_west_african_countries) %>%
  mutate(name_1_country = str_c(name_1, "(", country, ")", sep = ""),
         region = ifelse(country %in% central_african_countries, "central african",
                         "west african"))


#> cwafrica fig2

# GBD results filtered for relevant cause of death and countries
gbd_results_cwafrica_fig2 <- gbd_results %>%
  filter(cause_of_death %in% c("PM2.5 relative to WHO",
                               "Malaria", "Unsafe water, sanitation, and handwashing",
                               "HIV/AIDS"), location %in% c("nigeria", "democratic republic of the congo", "Angola", "ghana", "cameroon"))

# making the 'location' column of type factor
gbd_results_cwafrica_fig2$location <- factor(gbd_results_cwafrica_fig2$location,
                                             levels = c("nigeria", "democratic republic of the congo", "Angola", "ghana", "cameroon"))

# Rename Democratic Republic of the Congo to DRC
gbd_results_cwafrica_fig2$location <-  str_replace(gbd_results_cwafrica_fig2$location,
                                                   "democratic republic of the congo", "DRC")

# Converting 'cause_of_death' to type factor
gbd_results_cwafrica_fig2$cause_of_death <- as.factor(gbd_results_cwafrica_fig2$cause_of_death)

# Rearranging 'cause of death' levels
levels(gbd_results_cwafrica_fig2$cause_of_death) <- c("HIV/AIDS", "Malaria", "PM2.5 relative to WHO", "Unsafe Water, Sanitation and Handwashing")

# wrapping x-axis labels text
levels(gbd_results_cwafrica_fig2$cause_of_death) <- str_wrap(levels(gbd_results_cwafrica_fig2$cause_of_death), 15)

# getting country wise population
country_wise_population <- color_2020 %>%
  group_by(country) %>%
  summarise(population_total = sum(population, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(country %in% c("Cameroon", "Nigeria", "Angola", "Ghana",
                        "Democratic Republic of the Congo")) %>%
  arrange(desc(population_total))

# reorder within each location as per the total life years lost column
gbd_results_cwafrica_fig2 <- gbd_results_cwafrica_fig2 %>%
  mutate(cause_of_death = reorder_within(cause_of_death, est_le_diff_vs_actual_aggregate, location))

# clean the "cause of death" column and save the cwafrica factsheet figure 2
cwafrica_fs_fig2_dataset <- gbd_results_cwafrica_fig2 %>%
  mutate(`Cause of Death` = str_remove(cause_of_death, "___.+"))

# plot AR figure 2
cwafrica_fs_fig2 <- cwafrica_fs_fig2_dataset %>%
  ggplot(mapping = aes(x = cause_of_death, y = est_le_diff_vs_actual_aggregate)) +
  geom_col(mapping = aes(fill = `Cause of Death`), width = 0.7, color = "white") +
  facet_wrap(~factor(location, levels = c("nigeria", "DRC", "Angola",
                                          "ghana", "cameroon")), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("darkcyan", "deepskyblue", "red",
                               "aquamarine")) +
  labs(x = "", y = "Years Lost", title = "Life Expectancy Impacts of PM2.5 & Other Health Threats",
       subtitle = "Five Most Populous Countries in Central and West Africa") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(), legend.position = "bottom", axis.ticks = element_blank())

# save cwafrica fig2
ggsave("./june.2022/factsheets/cwafrica/graphs/cwafrica_fig2.png", cwafrica_fs_fig2)

