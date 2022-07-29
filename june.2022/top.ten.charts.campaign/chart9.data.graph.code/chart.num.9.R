# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

#> Chart number 9: Life Expectancy Impacts of Particulate Pollution and Other Health Threats in the Four Most Populous Countries in Central and West Africa


# GBD results filtered for relevant cause of death and countries
chart9_dataset <- gbd_results %>%
  filter(cause_of_death %in% c("PM2.5 relative to WHO",
                               "Malaria", "Unsafe water, sanitation, and handwashing",
                               "HIV/AIDS"), location %in% c("nigeria", "democratic republic of the congo", "Angola", "ghana", "cameroon"))

# making the 'location' column of type factor
chart9_dataset$location <- factor(chart9_dataset$location,
                                  levels = c("nigeria", "democratic republic of the congo", "Angola", "ghana", "cameroon"))

# Rename Democratic Republic of the Congo to DRC
chart9_dataset$location <-  str_replace(chart9_dataset$location,
                                        "democratic republic of the congo", "DRC")

# Converting 'cause_of_death' to type factor
chart9_dataset$cause_of_death <- as.factor(chart9_dataset$cause_of_death)

# Rearranging 'cause of death' levels
levels(chart9_dataset$cause_of_death) <- c("HIV/AIDS", "Malaria", "PM2.5 relative to WHO", "Unsafe Water, Sanitation and Handwashing")

# wrapping x-axis labels text
levels(chart9_dataset$cause_of_death) <- str_wrap(levels(chart9_dataset$cause_of_death), 15)

# getting country wise population
country_wise_population <- color_2020 %>%
  group_by(country) %>%
  summarise(population_total = sum(population, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(country %in% c("Cameroon", "Nigeria", "Angola", "Ghana",
                        "Democratic Republic of the Congo")) %>%
  arrange(desc(population_total))

# reorder within each location as per the total life years lost column
chart9_dataset <- chart9_dataset %>%
  mutate(cause_of_death = reorder_within(cause_of_death, est_le_diff_vs_actual_aggregate, location))

# clean the "cause of death" column and remove "Angola"
chart9_dataset <- chart9_dataset %>%
  mutate(`Cause of Death` = str_remove(cause_of_death, "___.+")) %>%
  filter(location %notin% "Angola")


# plot chart 9
chart9 <- ggplot(chart9_dataset, mapping = aes(x = cause_of_death, y = est_le_diff_vs_actual_aggregate)) +
  geom_col(mapping = aes(fill = `Cause of Death`), width = 0.7, color = "white") +
  facet_wrap(~factor(location, levels = c("nigeria", "DRC", "Angola",
                                          "ghana", "cameroon")), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("darkcyan", "deepskyblue", "red",
                               "aquamarine")) +
  labs(x = "", y = "Years Lost", title = "Life Expectancy Impacts of PM2.5 & Other Health Threats",
       subtitle = "Five Most Populous Countries in Central and West Africa") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_blank(), legend.position = "bottom", axis.ticks = element_blank())

# Save chart number 9
ggsave("./june.2022/top.ten.charts.campaign/chart9.data.graph.code/chart9.png", chart9)


# save chart9 dataset
chart9_dataset %>%
  select(location, cause_of_death, est_le_diff_vs_actual_aggregate) %>%
  rename(life_years_lost = est_le_diff_vs_actual_aggregate) %>%
  mutate(life_years_lost = round(life_years_lost, 1)) %>%
  write_csv("./june.2022/top.ten.charts.campaign/chart9.data.graph.code/chart9_dataset.csv")

