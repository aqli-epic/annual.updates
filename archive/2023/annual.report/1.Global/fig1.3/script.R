# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")



#> Global section figure 1.3----------------------------------------------

# Description: 10 most populated countries in the world: population, life expectancy gains, total potential life years gained, 3 panel graph

# Creating the master dataset with all relevant columns to plot the 3 figures in the panel (will be arranged in a single row)

ar_fig1.3_dataset <- gadm2_aqli_2021 %>%
  gadm_level_summary(c("country"), c(2021), 10) %>%
  slice_max(population, n = 10) %>%
  mutate(pop_millions = round(population/1000000, 1), 
         total_person_years_gained_billions = (llpp_who_2021*population)/1000000000)

ar_fig1.3_dataset$country <- factor(ar_fig1.3_dataset$country, levels = c("China", "India", "United States", "Indonesia", 
                                                              "Pakistan", "Nigeria", "Brazil", 
                                                              "Bangladesh", "Russia", 
                                                              "México"))


# population plot of the top 10 most populous
plt_top10_most_populous_population <- ar_fig1.3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, total_lyl_who_2021_millions), y = pop_millions, fill = country), width = 0.7) +
  labs(x = "Country", y = "Population (in millions)") +
  scale_y_continuous(breaks = seq(0, 1500, 200)) +
  coord_flip() +
  ggthemes::theme_tufte() +
  themes_aqli_base +
   scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "white", color = "white"))

# Average Life Expectancy Gains experienced by a person in top 10 most populous countries if PM2.5 is reduced to the WHO guideline
plt_top10_most_populous_le_gains_per_person <- ar_fig1.3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, total_lyl_who_2021_millions), y = llpp_who_2021, fill = country), , width = 0.7) +
  labs(x = "", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 8, 1)) +
   ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip() +
     scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "white", color = "white"))

# Total person years gained on the country level for the top 10 most populous regions in the world (if PM2.5 is reduced to the WHO guideline)

plt_top10_most_populous_tot_person_years_gained <- ar_fig1.3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(country, total_lyl_who_2021_millions), y = total_person_years_gained_billions, fill = country), width = 0.7) +
  labs(x = "", y = "Total Person Years Gained (Billion Years)") +
  scale_y_continuous(breaks = seq(0, 8, 1)) +
   ggthemes::theme_tufte() +
  themes_aqli_base +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip() +
 scale_fill_viridis(option = "inferno", discrete = TRUE) +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "white", color = "white"))

# Combine all 3 graphs in a panel
ar_prelim_fig1.3 <- gridExtra::grid.arrange(plt_top10_most_populous_population , plt_top10_most_populous_le_gains_per_person, plt_top10_most_populous_tot_person_years_gained, nrow = 1)


