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
library(reactable)
library(reactablefmtr)
library(highr)
library(highcharter)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(shinycssloaders)
library(DT)
library(writexl)
library(arrow)
library(plotly)
library(qs)
library(waiter)
library(shinyBS)
library(shinymanager)
library(reactable)
library(reactablefmtr)
library(leafgl)
 
# ---------------------------------------------------------
# Global constants used across the project
# ---------------------------------------------------------

who_guideline <- 5        # WHO PM2.5 guideline (µg/m³)
le_constant <- 0.098      # Life expectancy conversion factor used in AQLI calculations
latest_year <- 2024       # Latest year available in dataset
first_year <- 1998        # First year available in dataset


# ---------------------------------------------------------
# Load spatial shapefile data (GADM level 1)
# ---------------------------------------------------------

# Read combined GADM1 shapefile containing PM, population and geometry
gadm1_combined <- (qs::qread("./shapefiles/aqli_gadm1_20Jan2024.qs"))

# Trigger garbage collection to free memory
gc()


# ---------------------------------------------------------
# Load AQLI datasets at different administrative levels
# ---------------------------------------------------------

# GADM level 2 (district level)
gadm2_aqli <- as.data.table(arrow::read_parquet("./data/gadm2_aqli_2024.parquet")) 

# GADM level 1 (state/province level)
gadm1_aqli <- as.data.table(arrow::read_parquet("./data/gadm1_aqli_2024.parquet"))

# GADM level 0 (country level)
gadm0_aqli <- as.data.table(arrow::read_parquet("./data/gadm0_aqli_2024.parquet")) 


# ---------------------------------------------------------
# Resolve duplicate province names across countries
# Example: Punjab exists in both India and Pakistan
# ---------------------------------------------------------

gadm1_aqli$name_1[gadm1_aqli$name_1=="Punjab" & gadm1_aqli$country=="Pakistan"] <- "Punjab(Pakistan)"
gadm1_aqli$name_1[gadm1_aqli$name_1=="Punjab" & gadm1_aqli$country=="India"]    <- "Punjab(India)"


# ---------------------------------------------------------
# Select causes used from Global Burden of Disease dataset
# ---------------------------------------------------------

selected_causes <- c(
  "Ambient ozone pollution", "Child and maternal malnutrition", "Childhood sexual abuse and bullying",
  "Dietary risks", "Drug use", "HIV/AIDS and sexually transmitted infections", "High alcohol use",
  "Intimate partner violence", "Low physical activity", "Neglected tropical diseases and malaria",
  "Nutritional deficiencies", "Occupational risks", "PM2.5 relative to WHO guideline",
  "Self-harm and interpersonal violence", "Substance use disorders", "Tobacco", "Transport injuries",
  "Non-optimal temperature", "Other environmental risks", "Unintentional injuries",
  "Unsafe sex", "Unsafe water, sanitation, and handwashing"
)


# ---------------------------------------------------------
# Load GBD results and filter to selected causes
# ---------------------------------------------------------

gbd_results_master_2025 <- as.data.table(
  arrow::read_parquet("./data/gbd_results_master_2025.parquet")
)

gbd_results_master_2025 <- gbd_results_master_2025[
  cause_of_death %in% selected_causes
]

##########################################################
##########################################################

# ---------------------------------------------------------
# Load geographic reference datasets
# ---------------------------------------------------------

# Country → continent mapping
country_continent <- as.data.table(
  arrow::read_parquet("./data/country_continent.parquet")
)

# Custom region classification used in AQLI
region_countries <- read_csv("./data/region_countries.csv")


###########

# ---------------------------------------------------------
# Prepare GADM1 spatial dataset with PM2.5 values
# ---------------------------------------------------------

gadm1_shp_pm <- gadm1_combined %>%
  select(starts_with("pm"), obidgadm1, name0, name1, population, whostandard, natstandard, geometry) %>%
  mutate(name1_id = paste0(name1, " (", obidgadm1, ")")) %>%   # create unique identifier
  setNames(c(as.character(1998:2024), "obidgadm1", "name0", "name1", "population", 
             "whostandard", "natstandard", "geometry", "name1_id")) 


# ---------------------------------------------------------
# Prepare life expectancy loss dataset (WHO standard)
# ---------------------------------------------------------

gadm1_shp_llppwho <- gadm1_combined %>%
  select(starts_with("llpp_who_"), obidgadm1, name0, name1, population, whostandard, natstandard, geometry) %>%
  setNames(c(as.character(1998:2024), "obidgadm1", "name0", "name1", "population", 
             "whostandard", "natstandard", "geometry"))


# ---------------------------------------------------------
# Load PM2.5 and life expectancy datasets by admin level
# ---------------------------------------------------------

# GADM2 datasets
gadm2_pm25 <-  as.data.table(arrow::read_parquet("./data/gadm2_pm25.parquet")) 
gadm2_llppwho <-  as.data.table(arrow::read_parquet("./data/gadm2_llppwho.parquet")) 
gadm2_llpp_nat <-  as.data.table(arrow::read_parquet("./data/gadm2_llpp_nat.parquet")) 

# GADM1 datasets
gadm1_pm25 <- as.data.table(arrow::read_parquet("./data/gadm1_pm25.parquet")) 
gadm1_llppwho <-  as.data.table(arrow::read_parquet("./data/gadm1_llppwho.parquet")) 
gadm1_llpp_nat <-  as.data.table(arrow::read_parquet("./data/gadm1_llpp_nat.parquet")) 


######### Added code ###############

# ---------------------------------------------------------
# Load long-format datasets for interactive filtering
# ---------------------------------------------------------

# Country level dataset
gadm0_long <- arrow::read_parquet("./data/gadm0_long.parquet") 

# Province/state level dataset
gadm1_long <- arrow::read_parquet("./data/gadm1_long.parquet")

# District level dataset
gadm2_long <- arrow::read_parquet("./data/gadm2_long.parquet")


# ---------------------------------------------------------
# Create unique identifier for provinces
# Needed because some province names repeat across countries
# ---------------------------------------------------------

check <- gadm2_long %>% 
  select(country, name_1) %>%  
  unique()

check$id <- 1:nrow(check)
check$name1_id <- paste0(check$name_1, "(", check$id, ")")


# Attach the unique identifier back to datasets
gadm2_long <- left_join(gadm2_long, check, by = c("country"="country", "name_1" = "name_1"))
gadm1_long <- left_join(gadm1_long, check, by = c("country"="country", "name_1" = "name_1"))


# ---------------------------------------------------------
# Select only required columns for analysis
# ---------------------------------------------------------

cols_gadm1 <- c(
  "continent", "region", "country", "name_1", "population",  "year",
  "whostandard", "natstandard", "pm", "llpp_who", "llpp_nat", "name1_id"
)

gadm1_long <- gadm1_long[, cols_gadm1]


# ---------------------------------------------------------
# Adjust region classification for United States
# ---------------------------------------------------------

gadm1_long$region[gadm1_long$country == "United States"] <- "US & Canada"
gadm2_long$region[gadm2_long$country == "United States"] <- "US & Canada"


# Convert to data.table for faster processing
gadm1_long <- as.data.table(gadm1_long)
gadm0_long <- as.data.table(gadm0_long)
gadm2_long <- as.data.table(gadm2_long)

gc()

###############

# ---------------------------------------------------------
# Create filter table used for UI dropdown selections
# ---------------------------------------------------------

select_filter <- unique(
  gadm2_long[, c("continent", "region", "country", "name_1", "name_2")]
)

select_filter <- as.data.table(select_filter)

# Available year range for filters
year_for_filter <- 1998:2024


# ---------------------------------------------------------
# Fix duplicate Punjab name again in filter table
# ---------------------------------------------------------

select_filter$name_1[select_filter$name_1=="Punjab" & select_filter$country=="Pakistan"] <- "Punjab(Pakistan)"
select_filter$name_1[select_filter$name_1=="Punjab" & select_filter$country=="India"] <- "Punjab(India)"
########################
# ---------------------------------------------------------
# Regional country group definitions used in analysis
# ---------------------------------------------------------

# Central Africa country list
central_african_countries <- c("Angola", "Burundi", "Cameroon",
                               "Central African Republic", "Chad",
                               "Republic of the Congo",
                               "Democratic Republic of the Congo",
                               "Equatorial Guinea", "Gabon",
                               "Sao Tome and Principe",
                               "Rwanda")

# West Africa country list
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde",
                            "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
                            "Cote d'Ivoire", "Liberia", "Mali", "Mauritania",
                            "Niger", "Nigeria", "Senegal", "Sierra Leone",
                            "Togo")

# Combined Central + West Africa region definition
central_and_west_african_countries <- c(
  central_african_countries,
  west_african_countries
)


# ---------------------------------------------------------
# Southeast Asia country definition
# ---------------------------------------------------------

se_asia_vec <- c("Brunei", "Myanmar", "Cambodia", "Timor-Leste",
                 "Indonesia", "Laos", "Malaysia", "Philippines",
                 "Singapore", "Thailand", "Vietnam")


# ---------------------------------------------------------
# Indo-Gangetic Plains (India) state definition
# ---------------------------------------------------------

indo_gangetic_plains_states <- c("NCT of Delhi", "Uttar Pradesh",
                                 "Bihar", "Haryana",
                                 "Punjab", "Chandigarh",
                                 "West Bengal")


# ---------------------------------------------------------
# Europe-related country definitions
# ---------------------------------------------------------

# Load full European country list
european_countries <- read_csv("./data/europe_countries.csv")

# Western Europe definition
western_european_countries <- c("Germany", "Switzerland", "Italy", "Monaco",
                                "Luxembourg", "Belgium", "France",
                                "Netherlands", "Andorra", "Spain",
                                "United Kingdom", "Portugal",
                                "Denmark", "Ireland", "Iceland", "Austria")


# ---------------------------------------------------------
# Middle East and North Africa regional definitions
# ---------------------------------------------------------

middle_east <- c("Bahrain", "Iran", "Iraq", "Israel", "Jordan",
                 "Kuwait", "Lebanon", "Oman", "Qatar",
                 "Saudi Arabia", "Syria",
                 "United Arab Emirates", "Yemen")

North_Africa <- c("Algeria", "Djibouti", "Egypt",
                  "Libya", "Morocco", "Tunisia")


# ---------------------------------------------------------
# European Union country list
# ---------------------------------------------------------

eu_countries <- c("Belgium", "Bulgaria", "Czechia", "Denmark",
                  "Germany", "Estonia", "Ireland", "Greece",
                  "Spain", "France", "Croatia", "Italy",
                  "Cyprus", "Latvia", "Lithuania", "Luxembourg",
                  "Hungary", "Malta", "Netherlands",
                  "Austria", "Poland", "Portugal", "Romania",
                  "Slovenia", "Slovakia", "Finland", "Sweden",
                  "United Kingdom")


# ---------------------------------------------------------
# South Asia regional definition
# ---------------------------------------------------------

south_asia_def <- c("Afghanistan", "Bangladesh",
                    "Bhutan", "India",
                    "Maldives", "Nepal",
                    "Pakistan", "Sri Lanka")


# ---------------------------------------------------------
# Latin America regional definition
# ---------------------------------------------------------

latin_america_countries_vec <- c("Mexico", "Guatemala", "Honduras",
                                 "El Salvador", "Nicaragua",
                                 "Costa Rica", "Panama",
                                 "Colombia", "Venezuela",
                                 "Ecuador", "Peru",
                                 "Bolivia", "Brazil",
                                 "Paraguay", "Chile",
                                 "Argentina", "Uruguay",
                                 "Cuba", "Haiti",
                                 "Dominican Republic",
                                 "Puerto Rico")


# ---------------------------------------------------------
# OpenAQ Global Landscape 2022 dataset
# ---------------------------------------------------------

# Read OpenAQ monitoring infrastructure dataset
openaq_data <- read_csv("./data/openAQData2022_continent_adj.csv")

# Rename columns for consistency and clarity
colnames(openaq_data) <- c(
  "country",
  "evid_govt_spon_aq_mon",      # evidence of government-sponsored AQ monitoring
  "evidence_ad_hoc_studies",    # ad-hoc research-based monitoring evidence
  "aq_existed_in_past_not_cur", # monitoring existed historically but not currently
  "data_in_phys_units",         # data reported in physical units
  "transp_geog_scale_data_provided", # transparency in geographic coverage
  "data_fine_temporal_scale",   # high-frequency data availability
  "prog_access",                # programmatic access availability
  "continent"
)



##############################
# Population-weighted PM2.5 and Life Expectancy Calculations
##############################


# ---------------------------------------------------------
# Compute population-weighted averages at continent level
# ---------------------------------------------------------
compute_weighted <- function(data, group_var){
  data %>%
    group_by({{group_var}}) %>%
    mutate(pop_weights = population / sum(population, na.rm = TRUE)) %>%
    mutate(
      across(starts_with("pm"), ~ .x * pop_weights, .names = "{col}_weighted"),
      across(starts_with("llpp_who"), ~ .x * pop_weights, .names = "{col}_weighted"),
      across(starts_with("llpp_nat"), ~ .x * pop_weights, .names = "{col}_weighted")
    ) %>%
    summarise(
      population = sum(population, na.rm = TRUE),
      across(ends_with("weighted"), sum, na.rm = TRUE),
      .groups = "drop"
    )
}


continent_weighted <- compute_weighted(gadm0_aqli, continent)
aqli_region_weighted <- compute_weighted(gadm0_aqli, region)

#%>%

# ---------------------------------------------------------
# Replace missing region values with "Other"
# ---------------------------------------------------------

aqli_region_weighted$region[is.na(aqli_region_weighted$region)] <- "Other"

# Convert to data.table for faster operations
aqli_region_weighted <- as.data.table(aqli_region_weighted)



# =========================================================
# AUTHENTICATION DATA (Simple login credentials)
# =========================================================

# Create a simple credentials dataframe
# NOTE: Passwords are stored in plain text (not recommended for production)
credentials <- data.frame(
  user = c("aqli", "puru"),     # allowed usernames
  password = c("admin", "abcd"), # corresponding passwords
  stringsAsFactors = FALSE
)



##########################
# Custom Value Box UI Component
##########################

# Function to generate a compact "pill-style" value box
# Used in dashboards to display small metrics with an icon
pill_value_box <- function(icon, value, label, bg = "#ffffff") {
  
  tags$div(
    style = paste(
      "background:", bg, ";",
      "border-radius: 12px;",                    # rounded pill shape
      "padding: 6px 10px;",
      "display: inline-flex; align-items: center;",
      "gap: 6px;",
      "box-shadow: 0 2px 6px rgba(0,0,0,0.08);", # subtle shadow
      "font-size: 13px; color:#1b1b1b;"
    ),
    
    # Icon (Font Awesome or similar)
    tags$i(class = icon, style = "font-size:15px; opacity:0.8;"),
    
    # Main value (bold)
    tags$span(style="font-weight:600;", value),
    
    # Label text
    tags$span(style="opacity:0.6;", label)
  )
}


#####################################

# ---------------------------------------------------------
# Country capital dataset
# ---------------------------------------------------------

# Load country-capital mapping dataset
# Useful for geographic visualizations, maps, or metadata
country_capital <- as.data.table(
  read_csv("./data/country_capital.csv")
)