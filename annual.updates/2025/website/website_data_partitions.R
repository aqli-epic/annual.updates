#### Set Up ####
library(tidyverse)
library(readxl)
library(data.table)
library(writexl)

# set source file location
setwd("~/Desktop/AQLI/2025 AQLI Update/")

# functions
`%notin%` <- Negate(`%in%`)

# data
continent <- read_csv("website/input/country_continent.csv")
aqli <- read_csv("website/input/gadm0_aqli_2023.csv")
public_data <- read_csv("data/open_data.csv")
wb_gdppc <- read_csv("website/input/wb_gdppc.csv")

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

# Europe (53 countries)
# regions <- read_csv("~/USAID IDG TATTVA/Data_AQ/Monitoring Density Plots/AQLI Monitoring Density Calculation/AQLI Regions Countries.csv")
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
oecd <- read_excel("website/input/oecd.xlsx")

##### WB Income Group #####
wb_inc_grp <- read_excel("website/input/wb_inc_class_aqli_names.xlsx") %>%
  select("Economy", "Income group")

#### Summary Tables #####
wb_gdppc <- wb_gdppc %>%
  mutate_at(c(3:13), as.numeric) %>%
  mutate(`Country Code` = if_else(`Country Code` == "XKX", "XKO", `Country Code`))

summary_data <- aqli %>%
  select(id, name, population, natstandard, starts_with("pm")) %>%
  left_join(continent, by = c("name" = "country")) %>%
  left_join(public_data, by = c("name" = "Country or Dependency")) %>%
  left_join(wb_gdppc, by = c("id" = "Country Code")) %>%
  left_join(wb_inc_grp, by = c("name" = "Economy"))

summary_data <- summary_data %>%
  mutate(region = case_when(name %in% north_america ~ "United States & Canada",
                             name %in% latin_america ~ "Latin America",
                             name %in% south_asia ~ "South Asia",
                             name %in% se_asia ~ "South East Asia",
                             name == "China" ~ "China",
                             name %in% unlist(europe) ~ "Europe",
                             name %in% central_west_africa ~ "Central & West Africa",
                             name %in% mid_east_north_africa ~ "Middle East & North Africa",
                             continent == "Oceania" ~ "Oceania and Australia"),
         oecd = if_else(name %in% unlist(oecd), "OECD", "Non-OECD"))


##### AQLI Region #####
summary_aqli_regions <- summary_data %>%
  filter(!is.na(region1), !is.na(pm2023)) %>%
  select(region1, id,starts_with("pm"), population, `Publicly accessible?`, `2023 [YR2023]`) %>%
  group_by(region1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(tot_pop = sum(population),
            gdp = sum(`2023 [YR2023]`*population, na.rm = TRUE),
            `Percentage of countries with open data` = round(100*(sum(`Publicly accessible?` != "No", na.rm = TRUE)/n()), 2),
            across(ends_with("weighted"), sum),
            Countries = paste(id, collapse = ", ")) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(`GDP per capita` = round(gdp/tot_pop, 2),
         across(starts_with("pm"),
                ~ if_else(.x < 5, 0, 0.098*(.x - 5)),
                .names = "who{str_extract(.col, '\\\\d+')}"),
         across(starts_with("pm"),
                ~ if_else(.x < 5, 0, 0.098*(.x - 5)*tot_pop/1000000),
                .names = "total_lyl{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who|total_lyl)\\d+$"), ~ round(.x, 2))) %>%
  rename("Region" = "region1") %>%
  select(Region, `Percentage of countries with open data`, `GDP per capita`, Countries, starts_with("pm"), starts_with("who"), starts_with("total")) %>%
  mutate(Category = "AQLI Regions")

##### WHO Income Group #####
summary_wb <- summary_data %>%
  filter(!is.na(`Income group`), !is.na(pm2023)) %>%
  select(`Income group`,id, starts_with("pm"), population, `Publicly accessible?`, `2023 [YR2023]`) %>%
  group_by(`Income group`) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(tot_pop = sum(population),
            gdp = sum(`2023 [YR2023]`*population, na.rm = TRUE),
            `Percentage of countries with open data` = round(100*(sum(`Publicly accessible?` != "No", na.rm = TRUE)/n()), 2),
            across(ends_with("weighted"), sum),
            Countries = paste(id, collapse = ", ")) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(`GDP per capita` = round(gdp/tot_pop, 2),
         across(starts_with("pm"),
                ~ if_else(.x < 5, 0, 0.098*(.x - 5)),
                .names = "who{str_extract(.col, '\\\\d+')}"),
         across(starts_with("pm"),
                ~ if_else(.x < 5, 0, 0.098*(.x - 5)*tot_pop/1000000),
                .names = "total_lyl{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who|total_lyl)\\d+$"), ~ round(.x, 2))) %>%
  rename("Region" = "Income group") %>%
  select(Region, `Percentage of countries with open data`, `GDP per capita`, Countries, starts_with("pm"), starts_with("who"), starts_with("total")) %>%
  mutate(Category = "World Bank Income Group")

##### OECD #####
summary_oecd <- summary_data %>%
  filter(!is.na(oecd), !is.na(pm2023)) %>%
  select(oecd, id,starts_with("pm"), population, `Publicly accessible?`, `2023 [YR2023]`) %>%
  group_by(oecd) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(tot_pop = sum(population),
            gdp = sum(`2023 [YR2023]`*population, na.rm = TRUE),
            `Percentage of countries with open data` = round(100*(sum(`Publicly accessible?` != "No", na.rm = TRUE)/n()), 2),
            across(ends_with("weighted"), sum),
            Countries = paste(id, collapse = ", ")) %>%
  rename_with(~ gsub("_weighted", "", .), starts_with("pm")) %>%
  mutate(`GDP per capita` = round(gdp/tot_pop, 2),
         across(starts_with("pm"),
                ~ if_else(.x < 5, 0, 0.098*(.x - 5)),
                .names = "who{str_extract(.col, '\\\\d+')}"),
         across(starts_with("pm"),
                ~ if_else(.x < 5, 0, 0.098*(.x - 5)*tot_pop/1000000),
                .names = "total_lyl{str_extract(.col, '\\\\d+')}")) %>%
  mutate(across(matches("^(pm|who|total_lyl)\\d+$"), ~ round(.x, 2))) %>%
  rename("Region" = "oecd") %>%
  select(Region, `Percentage of countries with open data`, `GDP per capita`,Countries, starts_with("pm"), starts_with("who"), starts_with("total")) %>%
  mutate(Category = "OECD vs Non-OECD")

#### Export ####
website_data_partitions <- bind_rows(summary_aqli_regions, summary_oecd, summary_wb) %>%
  select(Category, everything())

write_xlsx(website_data_partitions, "website/website_data_partitions.xlsx")

