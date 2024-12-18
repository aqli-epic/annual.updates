# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Chart number 3: Percent of Population falling in different Life years lost buckets in 2020 v/s 1970


# read in "county for aqli pm2.5 dataset" (US technical appendix output file)
county_for_aqli_pm2.5_dataset <- read_csv("./june.2022/other.important.calculations.data/county_pm25_foraqli_stats.csv")

# us color dataset
color_2020_usa <- color_2020 %>%
  filter(country == "United States")

# right join us color data with 236 counties data, selecting and renaming certain columns
color_2020_usa_236_counties <-  color_2020_usa %>% right_join(county_for_aqli_pm2.5_dataset, by = c("name_1", "name_2"))  %>%
  select(country.x:natstandard, pm25_1970_aqli, pm2020.x) %>%
  rename(population = population.x,
         pm2020 = pm2020.x)

# adding a le_gain column
color_1970_usa_236_counties <- color_2020_usa_236_counties %>%
  mutate(le_gain_1970 = round((pm25_1970_aqli - 5)*0.098, 1),
         le_gain_1970 = ifelse(le_gain_1970 < 0, 0, le_gain_1970))

# adding a column that would serve as buckets, one for 2020 and one for 1970
color_1970_usa_236_counties <- color_1970_usa_236_counties %>%
  mutate(region_pm1970_tags = ifelse((le_gain_1970 >= 0 & le_gain_1970 <= 0.5), "0-0.5", le_gain_1970),
         region_pm1970_tags = ifelse((le_gain_1970 > 0.5 & le_gain_1970 <= 1), ">0.5-1", region_pm1970_tags),
         region_pm1970_tags = ifelse((le_gain_1970 > 1 & le_gain_1970 <= 1.5), ">1-1.5", region_pm1970_tags),
         region_pm1970_tags = ifelse((le_gain_1970 > 1.5), "1.5+", region_pm1970_tags))


color_2020_usa_all_counties <- color_2020 %>%
  filter(country == "United States") %>%
  mutate(region_pm2020_tags = ifelse((llpp_who5_2020 >= 0 & llpp_who5_2020 <= 0.5), "0-0.5", llpp_who5_2020),
         region_pm2020_tags = ifelse((llpp_who5_2020 > 0.5 & llpp_who5_2020 <= 1), ">0.5-1", region_pm2020_tags),
         region_pm2020_tags = ifelse((llpp_who5_2020 > 1 & llpp_who5_2020 <= 1.5), ">1-1.5", region_pm2020_tags),
         region_pm2020_tags = ifelse((llpp_who5_2020 > 1.5), "1.5+", region_pm2020_tags))



# bucket wise population 2020
bucket_wise_pop_2020 <- color_2020_usa_all_counties %>%
  group_by(region_pm2020_tags) %>%
  summarise(bucket_wise_pop_2020 = sum(population, na.rm = TRUE)) %>%
  mutate(pop_prop_2020 = bucket_wise_pop_2020/sum(bucket_wise_pop_2020, na.rm = TRUE)) %>%
  arrange(region_pm2020_tags)

# bucket wise population 1970
bucket_wise_pop_1970 <-  color_1970_usa_236_counties %>%
  group_by(region_pm1970_tags) %>%
  summarise(bucket_wise_pop_1970 = sum(population, na.rm = TRUE)) %>%
  mutate(pop_prop_1970 = bucket_wise_pop_1970/sum(bucket_wise_pop_1970, na.rm = TRUE))

# joining individual bucket wise 1970 and 2020 datasets and doing some variable renaming and minor modifications
final_dataset_la_times_fig_le_ver <- bucket_wise_pop_1970 %>%
  left_join(bucket_wise_pop_2020, by = c("region_pm1970_tags" = "region_pm2020_tags")) %>%
  rename(pm2.5_buckets = region_pm1970_tags) %>%
  mutate(pop_prop_1970 = pop_prop_1970*100,
         pop_prop_2020= pop_prop_2020*100) %>%
  select(pm2.5_buckets, pop_prop_1970, pop_prop_2020)

# chart 3 dataset
chart3_dataset <-
  final_dataset_la_times_fig_le_ver %>%
  pivot_longer(cols = pop_prop_1970:pop_prop_2020, names_to = "year", values_to = "pop_prop") %>%
  mutate(year = str_extract(year, "\\d+"),
         order_var = ifelse(pm2.5_buckets == "0-0.5", 1, 0),
         order_var = ifelse(pm2.5_buckets == ">0.5-1", 2, order_var),
         order_var = ifelse(pm2.5_buckets == ">1-1.5", 3, order_var),
         order_var = ifelse(pm2.5_buckets == "1.5+", 4, order_var))

# plot chart3
chart3 <- chart3_dataset %>%
  arrange(order_var) %>%
  ggplot() +
  geom_col(mapping = aes(x = fct_reorder(pm2.5_buckets, order_var), y = pop_prop, fill = pm2.5_buckets), width = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c("darkorange", "gold2", "turquoise", "darkred")) +
  ggthemes::theme_stata() +
  facet_wrap(~year) +
  labs(x = "Life Years Lost", y = "% of Population") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme(legend.position = "none")


