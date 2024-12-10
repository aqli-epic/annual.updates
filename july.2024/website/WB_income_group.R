library(tidyverse)
library(readxl)
library(data.table)

# set source file location
setwd("~/Desktop/AQLI/2024 AQLI Update/")

# world bank income classification
inc_grp <- read_excel("website/wb_inc_class_name_changed.xlsx") %>%
  filter(!is.na(Region))

# pm2.5 and lyl data
aqli <- read_csv("website/gadm0_2022_wide.csv") # %>% 
  # select(id, name, population, whostandard, natstandard, pm2022, who2022, nat2022)

# number of aq monitors by country (open_aq). Standardizing all names to AQLI data. govt and non govt sensor data received
aq_monitors_by_sensor <- read_csv("website/no_of_monitors_govt_other.csv") %>% 
  select(name, ismonitor, count) %>%
  pivot_wider(names_from = ismonitor,
              values_from = count) %>%
  rename(govt = "TRUE", other = "FALSE") %>%
  rowwise() %>%
  mutate(tot_monitor = sum(govt, other, na.rm=TRUE),
         name = ifelse(name == "Bosnia and Herz.", "Bosnia and Herzegovina", name),
         name = ifelse(name == "Central African Rep.", "Central African Republic", name),
         name = ifelse(name == "Dem. Rep. Congo", "Democratic Republic of the Congo", name),
         name = ifelse(name == "Dominican Rep.", "Dominican Republic", name),
         name = ifelse(name == "Mexico", "México", name),
         name = ifelse(name == "N. Cyprus", "Northern Cyprus", name),
         name = ifelse(name == "S. Sudan", "South Sudan", name),
         name = ifelse(name == "United States of America", "United States", name))

# high income country data. Standardizing all names to AQLI data
fund <- read_excel("website/caf_funding_data.xlsx") %>%
  mutate(Country = ifelse(Country == "Congo, Democratic Republic", "Democratic Republic of the Congo", Country),
         Country = ifelse(Country == "Congo, Republic", "Republic of the Congo", Country),
         Country = ifelse(Country == "Korea, Democratic People's Republic", "North Korea", Country),
         Country = ifelse(Country == "Lao PDR", "Laos", Country),
         Country = ifelse(Country == "Mexico", "México", Country),
         Country = ifelse(Country == "North Macedonia", "Macedonia", Country),
         Country = ifelse(Country == "State of Palestine", "Palestine", Country))


# combine data
summary_data <- aqli %>%
  left_join(aq_monitors_by_sensor, by = "name") %>%
  left_join(fund, by = c("name" = "Country"))

# check how many unmatched remain after changing names. Macao, HongKong and Channel Islands
unmatched <- anti_join(inc_grp, aqli, by = c("Economy" = "name"))

summary_data <- inner_join(summary_data, inc_grp, by = c("name" = "Economy"))

summary <- summary_data %>%
  filter(!is.na(`Income group`)) %>%
  select(`Income group`, starts_with("pm"), population, `Funding in USD million`, govt, other, tot_monitor) %>%
  group_by(`Income group`) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(avg_funding_in_USD_mil = round(mean(`Funding in USD million`, na.rm = TRUE), 2),
            avg_govt_monitors = round(mean(govt, na.rm = TRUE), 2),
            avg_other_monitors = round(mean(other, na.rm = TRUE), 2), 
            avg_total_monitors = round(mean(tot_monitor, na.rm = TRUE), 2),
            across(ends_with("weighted"), sum)) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(across(starts_with("pm"), 
                ~ 0.098 * (.x - 5), 
                .names = "who{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who)\\d+$"), ~ round(.x, 2)))
  

