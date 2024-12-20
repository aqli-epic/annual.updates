library(tidyverse)
library(readxl)
library(data.table)

# set source file location
setwd("~/Desktop/AQLI/2024 AQLI Update/")

# functions
`%notin%` <- Negate(`%in%`)

# vectors and data for caf funding regions
north_america <- c("United States", "Canada")
latin_america <- c("México", "Guatemala", "Honduras", "El Salvador",
                   "Nicaragua", "Costa Rica", "Panama", "Colombia",
                   "Venezuela", "Ecuador", "Peru", "Bolivia",
                   "Brazil", "Paraguay", "Chile", "Argentina",
                   "Uruguay", "Cuba", "Haiti", "Dominican Republic",
                   "Puerto Rico")
continent <- read_csv("data/country_continent.csv")


# pm2.5 and lyl data
aqli <- read_csv("data/gadm0_2022_wide.csv") # %>%
# select(id, name, population, whostandard, natstandard, pm2022, who2022, nat2022)

# number of aq monitors by country (open_aq). Standardizing all names to AQLI data.
# govt and non govt sensor data received
aq_monitors_by_sensor <- read_csv("~/Desktop/AQLI/aq_data_gap/input/no_of_monitors_govt_other.csv") %>%
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
fund <- read_excel("~/Desktop/AQLI/aq_data_gap/input/caf_funding_data.xlsx") %>%
  mutate(Country = ifelse(Country == "Congo, Democratic Republic", "Democratic Republic of the Congo", Country),
         Country = ifelse(Country == "Congo, Republic", "Republic of the Congo", Country),
         Country = ifelse(Country == "Korea, Democratic People's Republic", "North Korea", Country),
         Country = ifelse(Country == "Lao PDR", "Laos", Country),
         Country = ifelse(Country == "Mexico", "México", Country),
         Country = ifelse(Country == "North Macedonia", "Macedonia", Country),
         Country = ifelse(Country == "State of Palestine", "Palestine", Country))

# combine data
summary_data <- aqli %>%
  left_join(continent, by = c("name" = "country")) %>%
  left_join(aq_monitors_by_sensor, by = "name") %>%
  left_join(fund, by = c("name" = "Country"))

summary_data <- summary_data %>%
  mutate(`CAF Funding Region` = case_when(name %in% north_america ~ "North America",
                                          name %in% latin_america ~ "Latin America",
                                          name == "India" ~ "India",
                                          name == "China" ~ "China",
                                          continent == "Asia" & name %notin% c("India", "China") ~ "Rest of Asia",
                                          continent == "Africa" ~ "Africa",
                                          continent == "Europe" ~ "Europe"),
        `Funding in USD million` = replace_na(`Funding in USD million`, 0),
        govt = replace_na(govt, 0),
        other = replace_na(other, 0),
        tot_monitor = replace_na(tot_monitor, 0))

summary_data <- summary_data %>%
  filter(!is.na(`CAF Funding Region`), !is.nan(pm2022)) %>%
  select(`CAF Funding Region`, starts_with("pm"), population, govt, other, tot_monitor, `Funding in USD million`) %>%
  group_by(`CAF Funding Region`) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(tot_pop = sum(population)/100000,
            `Intl Dev Funding (USD million)` = round(mean(`Funding in USD million`, na.rm = TRUE), 2),
            avg_govt_monitors = round(weighted.mean(govt, population, na.rm = TRUE), 2),
            avg_other_monitors = round(weighted.mean(other, population, na.rm = TRUE), 2),
            avg_total_monitors = round(weighted.mean(tot_monitor, population, na.rm = TRUE), 2),
            across(ends_with("weighted"), sum)) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(across(starts_with("pm"),
                ~ 0.098 * (.x - 5),
                .names = "who{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who)\\d+$"), ~ round(.x, 2))) %>%
  mutate(`Philanthropic Funding (USD million)` = case_when(`CAF Funding Region` == "North America" ~ 115.5,
                                                `CAF Funding Region` ==  "Latin America" ~ 6.6,
                                                `CAF Funding Region` == "India" ~ 52.8,
                                                `CAF Funding Region` == "China" ~ 49.5,
                                                `CAF Funding Region` == "Rest of Asia" ~ 6.6,
                                                `CAF Funding Region` == "Africa" ~ 3.3,
                                                `CAF Funding Region` == "Europe" ~ 52.8),
         `Govt monitor density (per 100,000)` = avg_govt_monitors/tot_pop,
         `Other monitor density (per 100,000)` = avg_other_monitors/tot_pop,
         `Total monitor density (per 100,000)` = avg_total_monitors/tot_pop)


