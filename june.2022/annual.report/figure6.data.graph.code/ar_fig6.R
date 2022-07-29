# Load in master helper file for june.2022 folder-------------------------------

source("./june.2022/R/june.2022.helper.script.R")

# AR figure 6 code--------------------------------------------------------------

#> Figure 6: Distribution of Person-Years Gained if PM2.5 is Reduced to WHO Guideline Around the World

# oecd countries
oecd_countries <- c("Australia", "Austria", "Belgium",
                    "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark",
                    "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                    "Iceland", "Ireland", "Israel", "Italy", "Japan", "North Korea", "South Korea",
                    "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands",
                    "New Zealand", "Norway", "Poland", "Portugal", "Slovakia", "Slovenia",
                    "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States")

# add a le_gain and le_gain_grp column in the color_2020 dataset, this is to group regions into LE gains buckets
ar_fig6_dataset <- color_2020 %>%
  mutate(le_gain = (pm2020 - who_guideline)*le_constant,
         le_gain = ifelse(le_gain < 0, 0, le_gain),
         le_gain_grp = ifelse(le_gain == 0, "0", le_gain),
         le_gain_grp = ifelse(le_gain > 0 & le_gain <= 1, ">0-1", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 1 & le_gain <= 2, ">1-2", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 2 & le_gain <= 3, ">2-3", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 3 & le_gain <= 4, ">3-4", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 4 & le_gain <= 5, ">4-5", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 5 & le_gain <= 6, ">5-6", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 6 & le_gain <= 7, ">6-7", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 7 & le_gain <= 8, ">7-8", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 8 & le_gain <= 9, ">8-9", le_gain_grp),
         le_gain_grp = ifelse(le_gain > 9, ">9", le_gain_grp)) %>%
  mutate(country_grp = ifelse(country %in% oecd_countries, "OECD countries", country),
         country_grp = ifelse(country_grp %in% c("OECD countries", "India", "China", "Bangladesh", "Pakistan", "Indonesia"),
                              country_grp, "Rest of the World")) %>%
  select(country, name_1, name_2, population, pm2020, le_gain_grp, country_grp)

# add a ordering column that will help us arrange the le_gain buckets in the final figure
ar_fig6_dataset <- ar_fig6_dataset %>%
  group_by(le_gain_grp, country_grp) %>%
  summarise(tot_population = sum(population, na.rm = TRUE),
            tot_population_millions = tot_population/1000000) %>%
  ungroup() %>%
  mutate(le_gain_grp_order = ifelse(le_gain_grp == "0", 1, 0),
         le_gain_grp_order = ifelse(le_gain_grp == ">0-1", 2, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">1-2", 3, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">2-3", 4, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">3-4", 5, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">4-5", 6, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">5-6", 7, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">6-7", 8, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">7-8", 9, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">8-9", 10, le_gain_grp_order),
         le_gain_grp_order = ifelse(le_gain_grp == ">9", 11, le_gain_grp_order))

# changing the ordering column type to type numeric
ar_fig6_dataset$le_gain_grp_order <- as.numeric(ar_fig6_dataset$le_gain_grp_order)

# save annual report figure 6 dataset
ar_fig6_dataset %>%
  write_csv("./june.2022/annual.report/figure6.data.graph/ar_fig6_dataset.csv")


# plot ar fig6 dataset
ar_fig6 <- ar_fig6_dataset %>%
  ggplot() +
  geom_bar(mapping = aes(x = fct_reorder(le_gain_grp, le_gain_grp_order), y = tot_population_millions, fill = country_grp), stat = "identity") +
  scale_fill_manual(values = c("chocolate3", "red", "black", "lightgreen", "lightblue", "orange", "grey"), name = "Region") +
  scale_y_continuous(breaks = seq(0, 2500, 500)) +
  labs(x = "Years of Life Gained if Pollution is reduced to WHO Guideline",
       y = "People (in Millions)") +
  theme_classic()


# save annual report figure 6
ggsave("./june.2022/annual.report/figure6.data.graph/ar_fig6.png")
