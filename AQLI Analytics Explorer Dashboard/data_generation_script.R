# ============================================================
# Author: Purushottam Gupta
# Organization: Air Quality Life Index (AQLI), University of Chicago
# Email: guptap@uchicago.edu
#
# Description:
# Global data processing script for the AQLI dashboard.
# This script loads, cleans, reshapes, and exports air pollution
# and life expectancy data across GADM0 (country),
# GADM1 (state/province), and GADM2 (district/county) levels.
# ============================================================


library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(tidyverse)
library(sf)
library(usethis)
library(devtools)
library(data.table)
library(svglite)
library(here)
library(shiny)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(plotly)
library(reactable)
library(reactablefmtr)
library(highr)
library(highcharter)
library(leaflet)
library(RColorBrewer)
library(shinycssloaders)
library(DT)
library(writexl)
library(stringi)


# ------------------------------------------------------------
# Global variables
# ------------------------------------------------------------
`%notin%` <- Negate(`%in%`)

who_guideline <- 5
le_constant <- 0.098
latest_year <- 2023
first_year <- 1998


# ------------------------------------------------------------
# Load AQLI datasets
# ------------------------------------------------------------
gadm2_aqli_2023 <- as.data.table(read_csv("./data1/gadm2_aqli_2024_rgn_pg.csv"))
gadm1_aqli_2023 <- as.data.table(read_csv("./data1/gadm1_aqli_2024_rgn_pg.csv"))
gadm0_aqli_2023 <- as.data.table(read_csv("./data1/gadm0_aqli_2024_rgn_pg.csv"))


# ------------------------------------------------------------
# Fix missing continent values
# ------------------------------------------------------------
gadm1_aqli_2023$continent[is.na(gadm1_aqli_2023$continent)] <- "Other"
gadm0_aqli_2023$continent[is.na(gadm0_aqli_2023$continent)] <- "Other"
gadm2_aqli_2023$continent[is.na(gadm2_aqli_2023$continent)] <- "Other"


# ------------------------------------------------------------
# Fix missing region values
# ------------------------------------------------------------
gadm1_aqli_2023$region[is.na(gadm1_aqli_2023$region)] <- "Other"
gadm0_aqli_2023$region[is.na(gadm0_aqli_2023$region)] <- "Other"
gadm2_aqli_2023$region[is.na(gadm2_aqli_2023$region)] <- "Other"


# ------------------------------------------------------------
# Adjust region classification for specific countries
# ------------------------------------------------------------

# China as its own region
gadm1_aqli_2023$region[gadm1_aqli_2023$country == "China"] <- "China"
gadm0_aqli_2023$region[gadm0_aqli_2023$country == "China"] <- "China"
gadm2_aqli_2023$region[gadm2_aqli_2023$country == "China"] <- "China"

# United States & Canada region grouping
gadm1_aqli_2023$region[gadm1_aqli_2023$country == "Canada"] <- "United States & Canada"
gadm0_aqli_2023$region[gadm0_aqli_2023$country == "Canada"] <- "United States & Canada"
gadm2_aqli_2023$region[gadm2_aqli_2023$country == "Canada"] <- "United States & Canada"

gadm1_aqli_2023$region[gadm1_aqli_2023$country == "United States"] <- "United States & Canada"
gadm0_aqli_2023$region[gadm0_aqli_2023$country == "United States"] <- "United States & Canada"
gadm2_aqli_2023$region[gadm2_aqli_2023$country == "United States"] <- "United States & Canada"


# ------------------------------------------------------------
# Save cleaned datasets as Parquet (faster loading)
# ------------------------------------------------------------
arrow::write_parquet(gadm2_aqli_2023, "./data1/gadm2_aqli_2024.parquet")
arrow::write_parquet(gadm1_aqli_2023, "./data1/gadm1_aqli_2024.parquet")
arrow::write_parquet(gadm0_aqli_2023, "./data1/gadm0_aqli_2024.parquet")


# ------------------------------------------------------------
# Load Global Burden of Disease (GBD) results
# ------------------------------------------------------------
gbd_results_master_2025 <- read_csv("./data/gbd_results_master_2025.csv")


# ------------------------------------------------------------
# Load supporting geographic datasets
# ------------------------------------------------------------
country_continent <- readr::read_csv("./data/country_continent.csv")
region_countries <- readr::read_csv("./data/region_countries.csv")


# ------------------------------------------------------------
# Load earlier GBD dataset
# ------------------------------------------------------------
gbd_results_master_2023 <- read_csv("./data/gbd_results_master.csv")


# ------------------------------------------------------------
# Convert GADM1 data from wide → long format
# ------------------------------------------------------------
gadm1_long <- gadm1_aqli_2023 %>%
  pivot_longer(
    cols = -c(objectid_gadm1, iso_alpha3, country, name_1, population,
              whostandard, natstandard, continent, region),
    names_to = c("measure", "year"),
    names_pattern = "^(pm|llpp_who|llpp_nat)_?(\\d{4})$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  )


# ------------------------------------------------------------
# Convert GADM2 data from wide → long format
# ------------------------------------------------------------
gadm2_long <- gadm2_aqli_2023 %>%
  pivot_longer(
    cols = -c(objectid_gadm2, iso_alpha3, country, name_1, name_2, population,
              whostandard, natstandard, continent, region),
    names_to = c("measure", "year"),
    names_pattern = "^(pm|llpp_who|llpp_nat)_?(\\d{4})$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  )


# ------------------------------------------------------------
# Convert GADM0 data from wide → long format
# ------------------------------------------------------------
gadm0_long <- gadm0_aqli_2023 %>%
  pivot_longer(
    cols = -c(objectid_gadm0, iso_alpha3, continent, region, country,
              population, whostandard, natstandard),
    names_to = c("measure", "year"),
    names_pattern = "^(pm|llpp_who|llpp_nat)_?(\\d{4})$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  )


# ------------------------------------------------------------
# Save long datasets
# ------------------------------------------------------------
write_parquet(gadm0_long, "./data1/gadm0_long.parquet")
write_parquet(gadm1_long, "./data1/gadm1_long.parquet")
write_parquet(gadm2_long, "./data1/gadm2_long.parquet")


# ============================================================
# Additional Data Processing
# ============================================================

# ------------------------------------------------------------
# Extract PM2.5 time series
# ------------------------------------------------------------
gadm2_pm25 <- gadm2_aqli %>%
  select(starts_with("pm"), country, name_1, name_2, population,
         whostandard, natstandard) %>%
  setNames(c(as.character(1998:2024), "name0", "name1", "name2",
             "population", "whostandard", "natstandard")) %>%
  as.data.table()


# ------------------------------------------------------------
# Extract life expectancy loss vs WHO guideline
# ------------------------------------------------------------
gadm2_llppwho <- gadm2_aqli %>%
  select(starts_with("llpp_who_"), country, name_1, name_2,
         population, whostandard, natstandard) %>%
  setNames(c(as.character(1998:2024), "name0", "name1", "name2",
             "population", "whostandard", "natstandard")) %>%
  as.data.table()


# ------------------------------------------------------------
# Extract life expectancy loss vs national standard
# ------------------------------------------------------------
gadm2_llpp_nat <- gadm2_aqli %>%
  select(starts_with("llpp_nat_"), country, name_1, name_2,
         population, whostandard, natstandard) %>%
  setNames(c(as.character(1998:2024), "name0", "name1", "name2",
             "population", "whostandard", "natstandard")) %>%
  as.data.table()


# ------------------------------------------------------------
# Repeat processing for GADM1 level
# ------------------------------------------------------------
gadm1_pm25 <- gadm1_aqli %>%
  select(starts_with("pm"), country, name_1, population,
         whostandard, natstandard) %>%
  setNames(c(as.character(1998:2024), "name0", "name1",
             "population", "whostandard", "natstandard")) %>%
  as.data.table()

gadm1_llppwho <- gadm1_aqli %>%
  select(starts_with("llpp_who_"), country, name_1,
         population, whostandard, natstandard) %>%
  setNames(c(as.character(1998:2024), "name0", "name1",
             "population", "whostandard", "natstandard")) %>%
  as.data.table()

gadm1_llpp_nat <- gadm1_aqli %>%
  select(starts_with("llpp_nat_"), country, name_1,
         population, whostandard, natstandard) %>%
  setNames(c(as.character(1998:2024), "name0", "name1",
             "population", "whostandard", "natstandard")) %>%
  as.data.table()


# ------------------------------------------------------------
# Save processed datasets
# ------------------------------------------------------------
write_parquet(gadm2_pm25, "./data1/gadm2_pm25.parquet")
write_parquet(gadm2_llppwho, "./data1/gadm2_llppwho.parquet")
write_parquet(gadm2_llpp_nat, "./data1/gadm2_llpp_nat.parquet")

write_parquet(gadm1_pm25, "./data1/gadm1_pm25.parquet")
write_parquet(gadm1_llppwho, "./data1/gadm1_llppwho.parquet")
write_parquet(gadm1_llpp_nat, "./data1/gadm1_llpp_nat.parquet")
######################


# ------------------------------------------------------------
# Read GADM level-1 shapefile (administrative regions)
# ------------------------------------------------------------
aqli_shp1 <- st_read("~/Desktop/My AQLI Work/AQLI 2026/LatestShpFiles/gadm1/gadm1_jan2024.shp")


# ------------------------------------------------------------
# Join shapefile with AQLI data
# Matching:
# name0 → country
# name1 → first administrative unit
# ------------------------------------------------------------
finalgis <- left_join(
  aqli_shp1,
  gadm1_aqli,
  by = c("name0" = "country", "name1" = "name_1")
)


# ------------------------------------------------------------
# Save processed GIS object using qs for fast loading
# qs format loads much faster than RDS
# ------------------------------------------------------------
qsave(
  finalgis,
  "./shapefiles/aqli_gadm1_20Jan2024.qs",
  preset = "high"   # options: "fast", "balanced", "high"
)



# ------------------------------------------------------------
# Prepare GADM level-2 dataset
# Select relevant columns for analysis
# ------------------------------------------------------------
gadm2_long <- gadm2_long %>%
  select(
    continent, Region, country, name_1, name_2,
    population, year,
    whostandard, natstandard,
    pm, llpp_who, llpp_nat
  )


# ------------------------------------------------------------
# Convert national standard to numeric
# ------------------------------------------------------------
gadm2_long$natstandard <- as.numeric(gadm2_long$natstandard)


# ------------------------------------------------------------
# Convert dataset to data.table for faster processing
# ------------------------------------------------------------
gadm2_long <- as.data.table(gadm2_long)



# ============================================================
# Regional Definitions
# These vectors define groups of countries or regions used
# in AQLI analysis and reporting.
# ============================================================


# ------------------------------------------------------------
# Central Africa countries
# ------------------------------------------------------------
central_african_countries <- c(
  "Angola", "Burundi", "Cameroon",
  "Central African Republic", "Chad",
  "Republic of the Congo",
  "Democratic Republic of the Congo",
  "Equatorial Guinea", "Gabon",
  "São Tomé and Príncipe", "Rwanda"
)


# ------------------------------------------------------------
# West Africa countries
# ------------------------------------------------------------
west_african_countries <- c(
  "Benin", "Burkina Faso", "Cabo Verde",
  "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
  "Côte d'Ivoire", "Liberia", "Mali",
  "Mauritania", "Niger", "Nigeria",
  "Senegal", "Sierra Leone", "Togo"
)


# ------------------------------------------------------------
# Combined Central + West Africa
# ------------------------------------------------------------
central_and_west_african_countries <- c(
  central_african_countries,
  west_african_countries
)



# ------------------------------------------------------------
# Southeast Asia definition
# ------------------------------------------------------------
se_asia_vec <- c(
  "Brunei", "Myanmar", "Cambodia",
  "Timor-Leste", "Indonesia", "Laos",
  "Malaysia", "Philippines", "Singapore",
  "Thailand", "Vietnam"
)



# ------------------------------------------------------------
# Indo-Gangetic Plains states (India)
# ------------------------------------------------------------
indo_gangetic_plains_states <- c(
  "NCT of Delhi", "Uttar Pradesh", "Bihar",
  "Haryana", "Punjab", "Chandigarh", "West Bengal"
)



# ------------------------------------------------------------
# European country list (external dataset)
# ------------------------------------------------------------
european_countries <- read_csv("./data/europe_countries.csv")



# ------------------------------------------------------------
# Western Europe definition
# ------------------------------------------------------------
western_european_countries <- c(
  "Germany", "Switzerland", "Italy",
  "Monaco", "Luxembourg", "Belgium",
  "France", "Netherlands", "Andorra",
  "Spain", "United Kingdom", "Portugal",
  "Denmark", "Ireland", "Iceland", "Austria"
)



# ------------------------------------------------------------
# Middle East definition
# ------------------------------------------------------------
middle_east <- c(
  "Bahrain", "Iran", "Iraq", "Israel",
  "Jordan", "Kuwait", "Lebanon", "Oman",
  "Qatar", "Saudi Arabia", "Syria",
  "United Arab Emirates", "Yemen"
)



# ------------------------------------------------------------
# North Africa definition
# ------------------------------------------------------------
North_Africa <- c(
  "Algeria", "Djibouti", "Egypt",
  "Libya", "Morocco", "Tunisia"
)



# ------------------------------------------------------------
# European Union countries
# ------------------------------------------------------------
eu_countries <- c(
  "Belgium", "Bulgaria", "Czechia", "Denmark",
  "Germany", "Estonia", "Ireland", "Greece",
  "Spain", "France", "Croatia", "Italy",
  "Cyprus", "Latvia", "Lithuania", "Luxembourg",
  "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania",
  "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"
)



# ------------------------------------------------------------
# South Asia definition
# ------------------------------------------------------------
south_asia_def <- c(
  "Afghanistan", "Bangladesh", "Bhutan",
  "India", "Maldives", "Nepal",
  "Pakistan", "Sri Lanka"
)



# ------------------------------------------------------------
# Latin America definition
# ------------------------------------------------------------
latin_america_countries_vec <- c(
  "México", "Guatemala", "Honduras",
  "El Salvador", "Nicaragua", "Costa Rica",
  "Panama", "Colombia", "Venezuela",
  "Ecuador", "Peru", "Bolivia", "Brazil",
  "Paraguay", "Chile", "Argentina",
  "Uruguay", "Cuba", "Haiti",
  "Dominican Republic", "Puerto Rico"
)



# ============================================================
# OpenAQ Global Landscape 2022 Dataset
# ============================================================


# ------------------------------------------------------------
# Load OpenAQ monitoring dataset
# ------------------------------------------------------------
openaq_data <- read_csv("./data/openAQData2022_continent_adj.csv")


# ------------------------------------------------------------
# Standardize column names
# ------------------------------------------------------------
colnames(openaq_data) <- c(
  "country",
  "evid_govt_spon_aq_mon",
  "evidence_ad_hoc_studies",
  "aq_existed_in_past_not_cur",
  "data_in_phys_units",
  "transp_geog_scale_data_provided",
  "data_fine_temporal_scale",
  "prog_access",
  "continent"
)


############
# ============================================================
# Author: Purushottam Gupta
# Organization: Air Quality Life Index (AQLI), University of Chicago
# Email: guptap@uchicago.edu
# ============================================================


####################  GADM1 SHAPEFILE PREPARATION  ####################

# ------------------------------------------------------------
# Read shapefile once and join PM2.5 data
# This step avoids repeatedly loading large shapefiles
# ------------------------------------------------------------
gadm1_shp_pm <- sf::st_read(
  "./shapefiles/aqli_gadm1_final_june302023.shp",
  quiet = TRUE
) %>%
  left_join(
    gadm1_aqli_2023,
    by = c("obidgadm1" = "objectid_gadm1")
  ) %>%
  select(
    starts_with("pm"),
    obidgadm1,
    name0,
    name1,
    population,
    whostandard,
    natstandard,
    geometry
  )


# ------------------------------------------------------------
# Create helper ID for UI dropdowns
# ------------------------------------------------------------
gadm1_shp_pm$name1_id <- paste0(
  gadm1_shp_pm$name1,
  " (",
  gadm1_shp_pm$obidgadm1,
  ")"
)


# ------------------------------------------------------------
# Save optimized shapefile object
# ------------------------------------------------------------
saveRDS(gadm1_shp_pm, "./shapefiles/gadm1_shp_pm.rds")


# ------------------------------------------------------------
# Load optimized shapefile
# ------------------------------------------------------------
gadm1_shp_pm <- readRDS("./shapefiles/gadm1_shp_pm.rds")



######################  CAPITAL CITY DATA PROCESSING  ######################


# ------------------------------------------------------------
# Load capital city dataset
# ------------------------------------------------------------
country_capital <- read_xlsx("./data/Capital.xlsx")


# ------------------------------------------------------------
# Normalize special characters to ASCII
# ------------------------------------------------------------
country_capital <- country_capital %>%
  mutate(
    country = stri_trans_general(country, "Latin-ASCII"),
    Capital_city_region = stri_trans_general(Capital_city_region, "Latin-ASCII")
  )


# ------------------------------------------------------------
# Remove leading apostrophes if present
# ------------------------------------------------------------
country_capital$country <- str_remove(country_capital$country, "^'+")
country_capital$Capital_city_region <- str_remove(country_capital$Capital_city_region, "^'+")


# ------------------------------------------------------------
# Identify capital cities defined at GADM1 level
# ------------------------------------------------------------
data_1 <- country_capital %>%
  filter(grepl("gadm1", Capital_city_region, ignore.case = TRUE))


# Clean region label
data_1$Capital_city_region <- gsub("\\(GADM1\\)", "", data_1$Capital_city_region)
data_1$Capital_city_region <- trimws(data_1$Capital_city_region)



# ------------------------------------------------------------
# Capital cities defined at GADM2 level
# ------------------------------------------------------------
data_2 <- country_capital %>%
  filter(!grepl("gadm", Capital_city_region, ignore.case = TRUE))



# ------------------------------------------------------------
# Join GADM1 capital cities
# ------------------------------------------------------------
df <- left_join(
  data_1,
  gadm1_aqli,
  by = c("country" = "country", "Capital_city_region" = "name_1")
)

df <- df %>% select(-c("objectid_gadm1", "iso_alpha3"))



# ------------------------------------------------------------
# Join GADM2 capital cities
# ------------------------------------------------------------
df1 <- left_join(
  data_2,
  gadm2_aqli,
  by = c("country" = "country", "Capital_city_region" = "name_2")
)


# Remove duplicates
df1_unique <- df1 %>%
  distinct(country, .keep_all = TRUE)


# Remove invalid rows
df1_unique <- df1_unique %>%
  filter(Capital_city_region != "NA") %>%
  filter(!is.na(name_1))


df1_unique <- df1_unique %>%
  select(-c("objectid_gadm2", "iso_alpha3", "name_1"))



# ------------------------------------------------------------
# Combine GADM1 and GADM2 capital city datasets
# ------------------------------------------------------------
final_data_set <- rbind(df, df1_unique)


# Replace missing region and continent
final_data_set$region[is.na(final_data_set$region)]       <- "Other"
final_data_set$continent[is.na(final_data_set$continent)] <- "Other"



# ------------------------------------------------------------
# Create capital city ID for UI selection
# ------------------------------------------------------------
final_data_set$id <- 1:nrow(final_data_set)

final_data_set$capital_city_id <- paste0(
  final_data_set$Capital_city_region,
  "(",
  final_data_set$id,
  ")"
)



# ------------------------------------------------------------
# Remove problematic countries
# ------------------------------------------------------------
final_data_set <- final_data_set %>%
  filter(!country %in% c(
    "Cabo Verde",
    "Moldova",
    "Singapore",
    "Slovakia",
    "Trinidad and Tobago",
    "Turkmenistan"
  ))


# Export dataset
write_csv(final_data_set, "./data/country_capital.csv")



######################  PAKISTAN NATIONAL STANDARD UPDATE  ######################


# ------------------------------------------------------------
# Update Pakistan national standard
# ------------------------------------------------------------
gadm2_aqli$natstandard[gadm2_aqli$country == "Pakistan"] <- 15


# Separate Pakistan data
gadm2_aqli_wo_paki <- gadm2_aqli %>%
  filter(country != "Pakistan")

gadm2_aqli_paki <- gadm2_aqli %>%
  filter(country == "Pakistan") %>%
  select(-c(starts_with("llpp_nat")))



# ------------------------------------------------------------
# Recalculate life expectancy loss using new Pakistan standard
# ------------------------------------------------------------
gadm2_aqli_paki22 <- gadm2_aqli_paki %>%
  ungroup() %>%
  mutate(
    across(
      starts_with("pm"),
      ~ if_else(.x > natstandard,
                round(0.098 * (.x - natstandard), 1),
                0),
      .names = "llpp_nat_{substr(.col, 3, 6)}"
    )
  )


# Combine datasets
final_gadm2 <- rbind(gadm2_aqli_wo_paki, gadm2_aqli_paki22)

write_csv(final_gadm2, "./data1/gadm2_aqli_2024_20Jan.csv")



######################  AGGREGATE TO GADM1 LEVEL  ######################


gadm1_pak    <- gadm1_aqli %>% filter(country == "Pakistan")
gadm1_wo_pak <- gadm1_aqli %>% filter(country != "Pakistan")


gadm1_aqli_20Jan <- gadm2_aqli_paki22 %>%
  group_by(
    iso_alpha3,
    continent,
    region,
    country,
    name_1,
    whostandard,
    natstandard
  ) %>%
  summarise(
    across(
      all_of(value_cols),
      ~ round(
        sum(.x * population, na.rm = TRUE) /
          sum(population, na.rm = TRUE),
        2
      )
    ),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )


gadm1_aqli_20Jan <- gadm1_aqli_20Jan %>%
  arrange(name_1)


gadm1_aqli_20Jan$objectid_gadm1 <- 2225:2231


final_gadm1 <- rbind(gadm1_wo_pak, gadm1_aqli_20Jan)

write_csv(final_gadm1, "./data1/gadm1_aqli_2024_20Jan.csv")



######################  AGGREGATE TO GADM0 (COUNTRY) LEVEL  ######################


gadm0_pak    <- gadm0_aqli %>% filter(country == "Pakistan")
gadm0_wo_pak <- gadm0_aqli %>% filter(country != "Pakistan")


gadm0_aqli_20Jan <- gadm2_aqli_paki22 %>%
  group_by(
    iso_alpha3,
    continent,
    region,
    country,
    whostandard,
    natstandard
  ) %>%
  summarise(
    across(
      all_of(value_cols),
      ~ round(
        sum(.x * population, na.rm = TRUE) /
          sum(population, na.rm = TRUE),
        2
      )
    ),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )


gadm0_aqli_20Jan$objectid_gadm0 <- 166


final_gadm0 <- rbind(gadm0_wo_pak, gadm0_aqli_20Jan)

write_csv(final_gadm0, "./data1/gadm0_aqli_2024_20Jan.csv")



######################  EXPORT OPTIMIZED DATA FORMATS  ######################


# Load updated datasets
gadm0 <- read_csv("./data1/gadm0_aqli_2024_20Jan.csv")
gadm1 <- read_csv("./data1/gadm1_aqli_2024_20Jan.csv")
gadm2 <- read_csv("./data1/gadm2_aqli_2024_20Jan.csv")


# Save as parquet for faster loading in Shiny
write_parquet(gadm0, "./data1/gadm0_aqli_20Jan.parquet")
write_parquet(gadm1, "./data1/gadm1_aqli_20Jan.parquet")
write_parquet(gadm2, "./data1/gadm2_aqli_20Jan.parquet")