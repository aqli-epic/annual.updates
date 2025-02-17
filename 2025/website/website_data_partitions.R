#### Set Up ####
library(tidyverse)
library(readxl)
library(data.table)
library(writexl)

# set source file location
setwd("~/Desktop/AQLI/2024 AQLI Update/")

# functions
`%notin%` <- Negate(`%in%`)

# data
continent <- read_csv("data/country_continent.csv")
aqli <- read_csv("data/gadm0_2022_wide.csv")
public_data <- read_csv("website/open_data.csv")
fund <- read_excel("~/Desktop/AQLI/aq_data_gap/input/caf_funding_data.xlsx") %>%
  mutate(Country = ifelse(Country == "Congo, Democratic Republic", "Democratic Republic of the Congo", Country),
         Country = ifelse(Country == "Congo, Republic", "Republic of the Congo", Country),
         Country = ifelse(Country == "Korea, Democratic People's Republic", "North Korea", Country),
         Country = ifelse(Country == "Lao PDR", "Laos", Country),
         Country = ifelse(Country == "Mexico", "México", Country),
         Country = ifelse(Country == "North Macedonia", "Macedonia", Country),
         Country = ifelse(Country == "State of Palestine", "Palestine", Country))

#### Data Partition ####
##### AQLI Regions #####
# North America
north_america <- c("United States", "Canada")

# Latin America
latin_america <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                   "Costa Rica", "Cuba", "Dominican Republic", "Ecuador",
                   "El Salvador", "Guatemala", "Haiti", "Honduras", "México",
                   "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico",
                   "Uruguay", "Venezuela")

# Central and West Africa 
central_west_africa <- c("Angola", "Burundi", "Cameroon", 
                         "Central African Republic", "Chad", 
                         "Republic of the Congo", 
                         "Democratic Republic of the Congo", "Equatorial Guinea", 
                         "Gabon", "São Tomé and Príncipe", "Rwanda", "Benin", 
                         "Burkina Faso", "Cabo Verde", "Gambia", "Ghana", 
                         "Guinea", "Guinea-Bissau", "Côte d'Ivoire", "Liberia", 
                         "Mali", "Mauritania", "Niger", "Nigeria", "Senegal", 
                         "Sierra Leone", "Togo")

# South East Asia 
se_asia <- c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia", "Laos", 
             "Malaysia", "Philippines", "Singapore", "Thailand", "Vietnam")

# Europe
europe <- read_csv("data/europe_countries.csv")

# Western Europe
western_europe <- c("Germany", "Switzerland", "Italy", "Monaco", "Luxembourg", 
                    "Belgium", "France", "Netherlands", "Andorra", "Spain", 
                    "United Kingdom", "Portugal", "Denmark", "Ireland", 
                    "Iceland", "Austria", "Liechtenstein", "San Marino")

# South Asia 
south_asia <- c("Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", 
                "Nepal", "Pakistan", "Sri Lanka")


# Middle East and North Africa
mid_east_north_africa <- c("Bahrain", "Iran", "Iraq", "Israel", "Jordan",
                           "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia",
                           "Syria", "United Arab Emirates", "Yemen", "Algeria", 
                           "Djibouti", "Egypt", "Libya", "Morocco", "Tunisia")

##### OECD #####
oecd <- read_excel("website/oecd.xlsx")

##### WB Income Group #####
wb_inc_grp <- read_excel("website/wb_inc_class_aqli_names.xlsx") %>%
  select("Economy", "Income group")

#### Summary Tables #####
summary_data <- aqli %>%
  select(name, population, natstandard, starts_with("pm")) %>%
  left_join(continent, by = c("name" = "country")) %>%
  left_join(public_data, by = c("name" = "Country or Dependency")) %>%
  left_join(fund, by = c("name" = "Country")) %>%
  left_join(wb_inc_grp, by = c("name" = "Economy"))

summary_data <- summary_data %>%
  mutate(region1 = case_when(name %in% north_america ~ "United States & Canada",
                             name %in% latin_america ~ "Latin America",
                             name %in% south_asia ~ "South Asia",
                             name %in% se_asia ~ "South East Asia",
                             name == "China" ~ "China",
                             name %in% unlist(europe) ~ "Europe",
                             name %in% central_west_africa ~ "Central & West Africa",
                             name %in% mid_east_north_africa ~ "Middle East & North Africa",
                             continent == "Oceania" ~ "Oceania and Australia"),
         region2 = case_when(name %in% north_america ~ "United States & Canada",
                             name %in% latin_america ~ "Latin America",
                             name %in% south_asia ~ "South Asia",
                             name %in% se_asia ~ "South East Asia",
                             name == "China" ~ "China",
                             name %in% western_europe  ~ "West Europe",
                             name %in% unlist(europe) & name %notin% western_europe ~ "East Europe",
                             name %in% central_west_africa ~ "Central & West Africa",
                             name %in% mid_east_north_africa ~ "Middle East & North Africa",
                             continent == "Oceania" ~ "Oceania and Australia"),
         oecd = if_else(name %in% unlist(oecd), "OECD", "Non-OECD"))

##### AQLI Region #####
summary_region1 <- summary_data %>%
  filter(!is.na(region1), !is.nan(pm2022)) %>%
  select(region1, starts_with("pm"), population, `Publicly accessible?`, `Funding in USD million`) %>%
  group_by(region1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(tot_pop = sum(population),
            `Intl Dev Funding (USD million)` = round(mean(`Funding in USD million`, na.rm = TRUE), 2),
            `Percentage of countries with open data` = round(100*(sum(`Publicly accessible?` != "No", na.rm = TRUE)/n()), 2),
            across(ends_with("weighted"), sum)) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(across(starts_with("pm"),
                ~ 0.098 * (.x - 5),
                .names = "who{str_extract(.col, '\\\\d+')}"),
         across(starts_with("pm"),
                ~ 0.098 * (.x - 5) * tot_pop/1000000,
                .names = "total_lyl{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who|total_lyl)\\d+$"), ~ round(.x, 2))) %>%
  rename("Region" = "region1") %>%
  select(-tot_pop)

summary_europe <- summary_data %>%
  filter(region2 %in% c("East Europe", "West Europe"), !is.nan(pm2022)) %>%
  select(region2, starts_with("pm"), population, `Publicly accessible?`, `Funding in USD million`) %>%
  group_by(region2) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(tot_pop = sum(population),
            `Intl Dev Funding (USD million)` = round(mean(`Funding in USD million`, na.rm = TRUE), 2),
            `Percentage of countries with open data` = round(100*(sum(`Publicly accessible?` != "No", na.rm = TRUE)/n()), 2),
            across(ends_with("weighted"), sum)) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(across(starts_with("pm"),
                ~ 0.098 * (.x - 5),
                .names = "who{str_extract(.col, '\\\\d+')}"),
         across(starts_with("pm"),
                ~ 0.098 * (.x - 5) * tot_pop/1000000,
                .names = "total_lyl{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who|total_lyl)\\d+$"), ~ round(.x, 2))) %>%
  rename("Region" = "region2") %>%
  select(-tot_pop)

summary_aqli_regions <- rbind(summary_region1, summary_europe)

##### WHO Income Group #####
summary_wb <- summary_data %>%
  filter(!is.na(`Income group`), !is.nan(pm2022)) %>%
  select(`Income group`, starts_with("pm"), population, `Publicly accessible?`, `Funding in USD million`) %>%
  group_by(`Income group`) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(tot_pop = sum(population),
            `Intl Dev Funding (USD million)` = round(mean(`Funding in USD million`, na.rm = TRUE), 2),
            `Percentage of countries with open data` = round(100*(sum(`Publicly accessible?` != "No", na.rm = TRUE)/n()), 2),
            across(ends_with("weighted"), sum)) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(across(starts_with("pm"),
                ~ 0.098 * (.x - 5),
                .names = "who{str_extract(.col, '\\\\d+')}"),
         across(starts_with("pm"),
                ~ 0.098 * (.x - 5) * tot_pop/1000000,
                .names = "total_lyl{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who|total_lyl)\\d+$"), ~ round(.x, 2))) %>%
  rename("World Bank Income Group" = "Income group") %>%
  select(-tot_pop)

##### OECD #####
summary_oecd <- summary_data %>%
  filter(!is.na(oecd), !is.nan(pm2022)) %>%
  select(oecd, starts_with("pm"), population, `Publicly accessible?`, `Funding in USD million`) %>%
  group_by(oecd) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(tot_pop = sum(population),
            `Intl Dev Funding (USD million)` = round(mean(`Funding in USD million`, na.rm = TRUE), 2),
            `Percentage of countries with open data` = round(100*(sum(`Publicly accessible?` != "No", na.rm = TRUE)/n()), 2),
            across(ends_with("weighted"), sum)) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(across(starts_with("pm"),
                ~ 0.098 * (.x - 5),
                .names = "who{str_extract(.col, '\\\\d+')}"),
         across(starts_with("pm"),
                ~ 0.098 * (.x - 5) * tot_pop/1000000,
                .names = "total_lyl{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who|total_lyl)\\d+$"), ~ round(.x, 2))) %>%
  rename("OECD Status" = "oecd") %>%
  select(-tot_pop)

#### Export ####
write_xlsx(summary_aqli_regions, "website/output/aq_stats_by_aqli_regions.xlsx")
write_xlsx(summary_wb, "website/output/aq_stats_by_wb_income_groups.xlsx")
write_xlsx(summary_oecd, "website/output/aq_stats_by_oecd_status.xlsx")
