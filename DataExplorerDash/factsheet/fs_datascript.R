selected_country <- "Bangladesh"

# Filter data at different geographic levels
district_data <- gadm2_aqli_2023 %>% filter(country == selected_country)
province_data <- gadm1_aqli_2023 %>% filter(country == selected_country)
national_data <- gadm0_aqli_2023 %>% filter(country == selected_country)

# Ensure numeric conversion for LLPP columns
district_data <- district_data %>% mutate(across(starts_with("llpp_nat_"), as.numeric))
national_data <- national_data %>% mutate(across(starts_with("llpp_nat_"), as.numeric))

# National air quality standard
national_pm25_standard <- unique(district_data$natstandard)

# 2. National-level population statistics
total_population <- sum(district_data$population)
largest_district_population <- max(district_data$population)
largest_district_name <- district_data %>% 
  filter(population == largest_district_population) %>% pull(name_2)

# 3. WHO guideline-based statistics
national_llpp_if_who_met <- round(national_data$llpp_who_2023, 1)

total_life_years_gained_if_who_met <- district_data %>%
  filter(pm2023 > who_guideline) %>%
  summarise(total_lyl = round(sum(population * llpp_who_2023, na.rm = TRUE) / 1e6, 1)) %>%
  pull(total_lyl)

percent_population_exposed_above_who <- district_data %>%
  filter(pm2023 > who_guideline) %>%
  summarise(percent = round(sum(population, na.rm = TRUE) * 100 / total_population, 1)) %>%
  pull(percent)

# WHO-based life expectancy loss in most polluted provinces
top2_polluted_provinces_llpp_who <- province_data %>%
  arrange(desc(pm2023)) %>%
  head(2) %>%
  mutate(adjusted_llpp = ifelse(llpp_who_2023 < 1,
                                paste0(llpp_who_2023 * 12, " months"),
                                paste0(llpp_who_2023, " years"))) %>%
  pull(adjusted_llpp)

province1_llpp_who <- top2_polluted_provinces_llpp_who[1]
province2_llpp_who <- top2_polluted_provinces_llpp_who[2]

# 4. National standard-based statistics
percent_population_exposed_above_national <- district_data %>%
  filter(pm2023 > national_pm25_standard) %>%
  summarise(percent = round(sum(population, na.rm = TRUE) * 100 / total_population, 1)) %>%
  pull(percent)


total_life_years_lost_if_national_met <- district_data %>%
  filter(pm2023 > national_pm25_standard) %>%
  summarise(total_lyl = round(sum(population * llpp_nat_2023, na.rm = TRUE) / 1e6, 1)) %>%
  pull(total_lyl)

adjusted_llpp_national_level <- national_data %>%
  filter(country == selected_country) %>%
  mutate(adjusted_llpp = ifelse(llpp_nat_2023 < 1,
                                paste0(llpp_nat_2023 * 12, " months"),
                                paste0(llpp_nat_2023, " years"))) %>%
  pull(adjusted_llpp)

# 5. Subnational highlights
# Top 3 most polluted districts
top3_polluted_districts <- district_data %>%
  arrange(desc(pm2023)) %>%
  head(3) %>%
  pull(name_2)

district1_name <- top3_polluted_districts[1]
district2_name <- top3_polluted_districts[2]
district3_name <- top3_polluted_districts[3]

# Their corresponding life expectancy loss (WHO guideline)
top3_districts_llpp_who <- district_data %>%
  arrange(desc(pm2023)) %>%
  head(3) %>%
  pull(llpp_who_2023)

district1_llpp_who <- top3_districts_llpp_who[1]
district2_llpp_who <- top3_districts_llpp_who[2]
district3_llpp_who <- top3_districts_llpp_who[3]

# Most polluted provinces
top2_polluted_provinces <- province_data %>%
  arrange(desc(pm2023)) %>%
  head(2) %>%
  pull(name_1)

province1_name <- top2_polluted_provinces[1]
province2_name <- top2_polluted_provinces[2]

# Delhi (capital city) life loss
delhi_llpp_who <- district_data %>%
  filter(name_2 == "NCT of Delhi") %>%
  select(name_2, llpp_who_2023)

#GBD





######

top_10_pop <- district_data %>%
  arrange(desc(population)) %>%
  slice_head(n = 10)

top_10_pop_names <- top_10_pop$name_2
top_10_pop_pm <- top_10_pop$pm2023
top_10_pop_llpp <- top_10_pop$llpp_who_2023


# WHO/National guideline comparisons
lt_who <- district_data %>% filter(pm2023 < whostandard)
lt_nat <- district_data %>% filter(pm2023 < natstandard)
gt_who <- district_data %>% filter(pm2023 > whostandard)
gt_nat <- district_data %>% filter(pm2023 > natstandard)

pop_lt_who <- sum(lt_who$population) / total_population * 100
pop_lt_nat <- sum(lt_nat$population) / total_population * 100
pop_gt_who <- sum(gt_who$population) / total_population * 100
pop_gt_nat <- sum(gt_nat$population) / total_population * 100



poll_inc_pc <- (mean(district_data$pm2023) - mean(district_data$pm1999)) / mean(district_data$pm1999) * 100


lyl_inc_pc <- (mean(district_data$llpp_who_2023) - mean(district_data$llpp_who_1999)) / mean(district_data$llpp_who_1999) * 100
lyl_inc_month <- (mean(district_data$llpp_who_2023) - mean(district_data$llpp_who_1999)) * 12
