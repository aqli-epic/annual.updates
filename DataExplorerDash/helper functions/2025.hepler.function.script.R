# Helper script. Based on "/appPublic/aqli.data.explorer.helper.script.R". Needs
# be updated with every update cycle.

# Global variables, datasets and packages--------------------------------------

# metadata
# author: Nishka Sharma
# email: nishkasharma@uchicago.edu

# libraries
library(tidytext)
library(readxl)
library(tidyverse)
library(sf)
library(usethis)
library(devtools)
library(data.table)
library(svglite)

# global variables
`%notin%` <- Negate(`%in%`)
who_guideline <- 5

# read in latest color file
# global variables
`%notin%` <- Negate(`%in%`)
who_guideline <- 5

# set working directory here
setwd("~/Desktop/")

# read in latest PM2.5 data file
gadm2_aqli_2023 <- read_csv("~/Desktop/aqli_gadm2_2023.csv")
gadm1_aqli_2023 <- read_csv("~/Desktop/aqli_gadm1_2023.csv")
gadm0_aqli_2023 <- read_csv("~/Desktop/aqli_gadm0_2023.csv")

# read in the shapefile
gadm2_aqli_2023_shp <- st_read("~/Desktop/shapefile/aqli_gadm2_final_june302023.shp")
gadm1_aqli_2023_shp <- st_read("~/Desktop/shapefile/aqli_gadm1_final_june302023.shp")
gadm0_aqli_2023_shp <- st_read("~/Desktop/shapefile/aqli_gadm0_final_june302023.shp")

# india state
india_state <- st_read("~/Desktop/india_state.shp")

# read archive data
color_2020 <- read_csv("~/Desktop/color_2020.csv")
color_2019 <- read_csv("~/Desktop/color_2019.csv")
color_2016 <- read_csv("~/Desktop/color_2016.csv")

# gbd results master
gbd_results_master_2025 <- read_csv("~/Desktop/gbd_results_master_2025.csv")

# US 1970 calculation master cleaned file read
us_1970_calc_results_cleaned <- read_csv("~/Desktop/county_pm25_foraqli_stats.csv")

# read in latest national standard file
#nat_standard <- readr::read_csv("C:/Users/Aarsh/Desktop/country_annual_average_pm2.5_standards_2024.csv", skip = 26) %>%
# rename(natstandard = `National Annual Average PM2.5 Standard (in µg/m³)`) %>%
#mutate(natstandard = as.numeric(natstandard))

# read in the country continent file
country_continent <- read_csv("~/Desktop/country_continent.csv")

# join gadm2 dataset with the country_continent file using the continent column
gadm2_aqli_2023 <- gadm2_aqli_2023 %>%
  left_join(country_continent, by = "country")

# global variables
who_guideline <- 5
le_constant <- 0.098
latest_year <- 2023
first_year <- 1998

# global operations
`%notin%` <- Negate(`%in%`)

# other region wise defintions
# Central Africa definition
central_african_countries <- c("Angola", "Burundi", "Cameroon",
                               "Central African Republic", "Chad",
                               "Republic of the Congo",
                               "Democratic Republic of the Congo",
                               "Equatorial Guinea", "Gabon",
                               "São Tomé and Príncipe", "Rwanda")
# West Africa definition
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde", "Gambia",
                            "Ghana", "Guinea", "Guinea-Bissau", "Côte d'Ivoire",
                            "Liberia", "Mali", "Mauritania", "Niger", "Nigeria",
                            "Senegal", "Sierra Leone", "Togo")

# central and west africa countries definition, combine in a single vector
central_and_west_african_countries <- c(central_african_countries, west_african_countries)

# South East Asia definition
se_asia_vec <- c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia",
                 "Laos", "Malaysia", "Philippines", "Singapore", "Thailand",
                 "Vietnam")

# Northern plains (India)
indo_gangetic_plains_states <- c("NCT of Delhi", "Uttar Pradesh", "Bihar", "Haryana",
                                 "Punjab", "Chandigarh", "West Bengal")

# Fenwei Plain (China)
fenwei_plain <- c("Xi'an", "Baoji", "Xianyang", "Weinan", "Tongchuan",
                  "Jinzhong", "Luliang", "Linfen", "Yuncheng", "Luoyang",
                  "Sanmenxia")

# European countries
european_countries <- read_csv("~/Desktop/europe_countries.csv")

# Western European countries
western_european_countries <- c("Germany", "Switzerland", "Italy", "Monaco",
                                "Luxembourg", "Belgium", "France", "Netherlands",
                                "Andorra", "Spain", "United Kingdom", "Portugal",
                                "Denmark", "Ireland", "Iceland", "Austria",
                                "Liechtenstein", "San Marino")


# European Union countries
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
                  "Czechia", "Denmark", "Estonia", "Finland", "France",
                  "Germany", "Greece", "Hungary", "Ireland", "Italy",
                  "Latvia", "Lithuania", "Luxembourg", "Malta",
                  "Netherlands",  "Poland", "Portugal", "Romania",
                  "Slovakia", "Slovenia", "Spain", "Sweden")

# South Asia definition
south_asia_def <- c("Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives",
                    "Nepal", "Pakistan", "Sri Lanka")

# Latin America definition
latin_america_countries_vec <- c("México", "Guatemala", "Honduras", "El Salvador",
                                 "Nicaragua", "Costa Rica", "Panama", "Colombia",
                                 "Venezuela", "Ecuador", "Peru", "Bolivia",
                                 "Brazil", "Paraguay", "Chile", "Argentina",
                                 "Uruguay", "Cuba", "Haiti", "Dominican Republic",
                                 "Puerto Rico")

# Middle East definition
mid_east_countries <- c("Bahrain", "Iran", "Iraq", "Israel", "Jordan",
                        "Kuwait", "Lebanon", "Oman", "Qatar", "Saudi Arabia",
                        "Syria", "United Arab Emirates", "Yemen")

# North Africa
north_africa_countries <- c("Algeria", "Djibouti", "Egypt", "Libya", "Morocco", "Tunisia")

# middle east and north africa
mena_countries <- c(mid_east_countries, north_africa_countries)

# aqli base theme function---------------------------------------------------------------------------
# add this to your graphs to standardize them, example: plt1 %>% theme_aqli_base
themes_aqli_base <- ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, margin = margin(b = 0.2, unit = "cm")),
        plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 0.7, unit = "cm")),
        axis.title.x = element_text(size = 14, margin = margin(t = 0.3, b = 0.5, unit = "cm")),
        axis.title.y = element_text(size = 14, margin = margin(r = 0.3, unit = "cm")),
        axis.text = element_text(size = 13),
        plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 0.7, unit = "cm"), face = "italic"),
        legend.box.background = element_rect(color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13),
        legend.position = "bottom")

# function that adds AQLI colors and axis titles (ViT color check complete, matched with note)

## arguments:
# df: AQLI dataset in question.
# scale_type: can be either one of "pollution" or "lyl"
# col_name: (in quotes), based on which the color buckets are to be assigned, the one being plotted.

add_aqli_color_scale_buckets <- function(df, scale_type = "pollution", col_name){
  if(scale_type == "lyl"){
    df %>%
      mutate(lyl_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 0.1), "0 to < 0.1", NA),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 0.1) & (!!as.symbol(col_name) < 0.5), "0.1 to < 0.5", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 0.5) & (!!as.symbol(col_name) < 1), "0.5 to < 1", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 1) & (!!as.symbol(col_name) < 2), "1 to < 2", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 2) & (!!as.symbol(col_name) < 3), "2 to < 3", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 3) & (!!as.symbol(col_name) < 4), "3 to < 4", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 4) & (!!as.symbol(col_name) < 5), "4 to < 5", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 6), "5 to < 6", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 6), ">= 6", lyl_bucket)) %>%
      mutate(order_lyl_bucket = ifelse(lyl_bucket == "0 to < 0.1", 1, NA),
             order_lyl_bucket = ifelse(lyl_bucket == "0.1 to < 0.5", 2, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "0.5 to < 1", 3, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "1 to < 2", 4, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "2 to < 3", 5, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "3 to < 4", 6, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "4 to < 5", 7, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "5 to < 6", 8, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == ">= 6", 9, order_lyl_bucket))
    
  } else if (scale_type == "pollution") {
    df %>%
      mutate(pol_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 5), "0 to < 5", NA),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 10), "5 to < 10", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 10) & (!!as.symbol(col_name) < 20), "10 to < 20", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 20) & (!!as.symbol(col_name) < 30), "20 to < 30", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 30) & (!!as.symbol(col_name) < 40), "30 to < 40", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 40) & (!!as.symbol(col_name) < 50), "40 to < 50", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 50) & (!!as.symbol(col_name) < 60), "50 to < 60", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 60) & (!!as.symbol(col_name) < 70), "60 to < 70", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 70), ">= 70", pol_bucket)) %>%
      mutate(order_pol_bucket = ifelse(pol_bucket == "0 to < 5", 1, NA),
             order_pol_bucket = ifelse(pol_bucket == "5 to < 10", 2, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "10 to < 20", 3, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "20 to < 30", 4, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "30 to < 40", 5, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "40 to < 50", 6, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "50 to < 60", 7, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "60 to < 70", 8, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == ">= 70", 9, order_pol_bucket))
    
  } else if (scale_type == "lyldiff"){
    
    df %>%
      mutate(lyldiff_bucket = ifelse((!!as.symbol(col_name) < -2), "< -2", NA),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -2) & (!!as.symbol(col_name) < -0.5), "-2 to (< -0.5)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -0.5) & (!!as.symbol(col_name) < -0.1), "-0.5 to (< -0.1)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -0.1) & (!!as.symbol(col_name) < 0), "-0.1 to (< 0)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 0.1), "0 to (< 0.1)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0.1) & (!!as.symbol(col_name) < 0.5), "0.1 to (< 0.5)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0.5) & (!!as.symbol(col_name) < 2), "0.5 to (< 2)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 2), ">= 2", lyldiff_bucket)) %>%
      mutate(order_lyldiff_bucket = ifelse(lyldiff_bucket == "< -2", 1, NA),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-2 to (< -0.5)", 2, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-0.5 to (< -0.1)", 3, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-0.1 to (< 0)", 4, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0 to (< 0.1)", 5, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0.1 to (< 0.5)", 6, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0.5 to (< 2)", 7, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == ">= 2", 8, order_lyldiff_bucket))
    
  } else if (scale_type == "poldiff"){
    
    df %>%
      mutate(poldiff_bucket = ifelse((!!as.symbol(col_name) < -10), "< -10", NA),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -10) & (!!as.symbol(col_name) < -5), "-10 to (< -5)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -5) & (!!as.symbol(col_name) < -1), "-5 to (< -1)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -1) & (!!as.symbol(col_name) < 0), "-1 to (< 0)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 1), "0 to (< 1)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 1) & (!!as.symbol(col_name) < 5), "1 to (< 5)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 10), "5 to (< 10)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 10), ">= 10", poldiff_bucket)) %>%
      mutate(order_poldiff_bucket = ifelse(poldiff_bucket == "< -10", 1, NA),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-10 to (< -5)", 2, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-5 to (< -1)", 3, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-1 to (< 0)", 4, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "0 to (< 1)", 5, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "1 to (< 5)", 6, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "5 to (< 10)", 7, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == ">= 10", 8, order_poldiff_bucket))
    
  }
  
}

#> GADM level summary and Compare Regions tabs function---------------------------------------

## arguments (Roxygen file upcoming):
# df: AQLI datasets (either gadm2, 1, 0, given the use in question)

# level_col_name_vec: specify complete vector of the level of summary. There are 2 cases:
#  (a) if you need a continent level summary, just enter: c("continent").
#  (b) For any other level summary,  you'll have to add the entire vector, starting from country, For example, to get a
#      state level summary, enter: c("country", "name_1"). Similarly, for district level summary,
#      enter, c("country", "name_1", "name_2").

# years: a vector of years for which you need the data, example: c(2022, 2020)

# perc_red_by: a custom percent reduction, which will create custom reduction columns, e.g. 10 for 10% reduction.
gadm_level_summary <- function(df, level_col_name_vec, years, perc_red_by){
  
  le_constant <- 0.098
  pol_col_names <- stringr::str_c("pm", years)
  pol_col_names_red_to <- stringr::str_c("pm", years, "_reduced_to")
  llpp_who_col_names <- stringr::str_c("llpp_who_", years)
  llpp_nat_col_names <- stringr::str_c("llpp_nat_", years)
  llpp_pol_red_names <- stringr::str_c("llpp_pol_red_", years)
  total_lyl_who_columns <- stringr::str_c("total_lyl_who_", years, "_millions")
  total_lyl_nat_columns <- stringr::str_c("total_lyl_nat_", years, "_millions")
  total_lyl_pol_red_to_columns <- stringr::str_c("total_lyl_pol_red_", years, "_millions")
  
  # when level just equal to continent, deal separately, as in this case natstandard column should not be included
  if((level_col_name_vec[1] == "continent") & (length(level_col_name_vec) == 1)){
    aqli_wide <- df %>%
      dplyr::group_by_at(level_col_name_vec) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE)) %>%
      dplyr::mutate(across(dplyr::starts_with("pm"), ~(.x*pop_weights), .names = "{col}_pop_weighted")) %>%
      dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 1)), .names = "avg_{col}"),
                       total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], .groups = "keep") %>%
      select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, dplyr::everything()) %>%
      dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
      dplyr::rename(population = total_population,
                    objectid_level = objectid_gadm2) %>%
      dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x - whostandard)*le_constant, 1)), .names = "llpp_who_{col}")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x*((perc_red_by)/100)), 1)*le_constant), .names = "llpp_pol_red_{col}")) %>%
      dplyr::mutate(across(starts_with("llpp"), ~ifelse(.x < 0, 0, .x))) %>%
      dplyr::select(objectid_level, iso_alpha3, all_of(level_col_name_vec), population, whostandard, dplyr::everything()) %>%
      dplyr::rename_with(~str_replace(.x, "pm", ""), dplyr::contains("llpp")) %>%
      dplyr::mutate(across(dplyr::matches("pm|llpp"), ~(round(.x, 1)), .names = "{col}")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(across(dplyr::matches("llpp_who"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::mutate(across(dplyr::matches("llpp_pol_red"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::rename_with(~str_replace_all(.x, "llpp", "lyl"), dplyr::contains("total_llpp")) %>%
      dplyr::mutate(across(dplyr::matches("pm"), ~.x - (round((.x)*(perc_red_by/100), 1)), .names = "{col}_reduced_to")) %>%
      dplyr::select(objectid_level:whostandard, all_of(c(pol_col_names, pol_col_names_red_to, llpp_who_col_names, llpp_pol_red_names, total_lyl_who_columns, total_lyl_pol_red_to_columns))) %>%
      dplyr::mutate(across(matches("pm|llpp|total"), ~ifelse(population == 0, NA, .x))) %>%
      dplyr::mutate(population = ifelse(population == 0, NA, population))
    
    
    return(aqli_wide)
    
  } else if(("name_2" %in% level_col_name_vec)){
    if((which(level_col_name_vec == "name_2") > 2)){
      aqli_wide <- df %>%
        dplyr::mutate(across(starts_with("pm"), (~round((.x*((perc_red_by)/100)), 1)*le_constant), .names = "llpp_pol_red_{col}")) %>%
        dplyr::mutate(across(starts_with("llpp"), ~ifelse(.x < 0, 0, .x))) %>%
        dplyr::rename_with(~str_replace(.x, "pm", ""), dplyr::contains("llpp")) %>%
        dplyr::mutate(across(dplyr::matches("pm|llpp"), ~(round(.x, 1)), .names = "{col}")) %>%
        dplyr::mutate(across(dplyr::matches("llpp_who"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
        dplyr::mutate(across(dplyr::matches("llpp_nat"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
        dplyr::mutate(across(dplyr::matches("llpp_pol_red"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
        dplyr::rename_with(~str_replace_all(.x, "llpp", "lyl"), dplyr::contains("total_llpp")) %>%
        dplyr::mutate(across(dplyr::matches("pm"), ~.x - (round((.x)*(perc_red_by/100), 1)), .names = "{col}_reduced_to")) %>%
        dplyr::select(objectid_gadm2:natstandard, continent, all_of(c(pol_col_names, pol_col_names_red_to, llpp_who_col_names, llpp_nat_col_names, llpp_pol_red_names, total_lyl_who_columns,
                                                                      total_lyl_nat_columns, total_lyl_pol_red_to_columns))) %>%
        dplyr::mutate(across(matches("pm|llpp|total"), ~ifelse(population == 0, NA, .x))) %>%
        dplyr::mutate(population = ifelse(population == 0, NA, population))
      
    }
    
    return(aqli_wide)
    
  } else {
    aqli_wide <- df %>%
      dplyr::group_by_at(level_col_name_vec) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE)) %>%
      dplyr::mutate(across(dplyr::starts_with("pm"), ~(.x*pop_weights), .names = "{col}_pop_weighted")) %>%
      dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 1)), .names = "avg_{col}"),
                       total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], natstandard = natstandard[1], .groups = "keep") %>%
      select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, natstandard, dplyr::everything()) %>%
      dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
      dplyr::rename(population = total_population,
                    objectid_level = objectid_gadm2) %>%
      dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x - whostandard)*le_constant, 1)), .names = "llpp_who_{col}")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x*((perc_red_by)/100)), 1)*le_constant), .names = "llpp_pol_red_{col}")) %>%
      dplyr::mutate(across(starts_with("pm"), (~round((.x - natstandard)*le_constant, 1)), .names = "llpp_nat_{col}")) %>%
      dplyr::mutate(across(starts_with("llpp"), ~ifelse(.x < 0, 0, .x))) %>%
      dplyr::mutate(across(starts_with("llpp_nat"), ~ifelse(natstandard == 0, NA, .x))) %>%
      dplyr::select(objectid_level, iso_alpha3, all_of(level_col_name_vec), population, whostandard, natstandard, dplyr::everything()) %>%
      dplyr::rename_with(~str_replace(.x, "pm", ""), dplyr::contains("llpp")) %>%
      dplyr::mutate(across(dplyr::matches("pm|llpp"), ~(round(.x, 1)), .names = "{col}")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(across(dplyr::matches("llpp_who"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::mutate(across(dplyr::matches("llpp_nat"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::mutate(across(dplyr::matches("llpp_pol_red"), ~round((population*.x)/1000000, 1), .names = "total_{col}_millions")) %>%
      dplyr::rename_with(~str_replace_all(.x, "llpp", "lyl"), dplyr::contains("total_llpp")) %>%
      dplyr::mutate(across(dplyr::matches("pm"), ~.x - (round((.x)*(perc_red_by/100), 1)), .names = "{col}_reduced_to")) %>%
      dplyr::select(objectid_level:natstandard, all_of(c(pol_col_names, pol_col_names_red_to, llpp_who_col_names, llpp_nat_col_names, llpp_pol_red_names, total_lyl_who_columns,
                                                         total_lyl_nat_columns, total_lyl_pol_red_to_columns))) %>%
      dplyr::mutate(across(matches("pm|llpp|total"), ~ifelse(population == 0, NA, .x))) %>%
      dplyr::mutate(population = ifelse(population == 0, NA, population))
    
    
    return(aqli_wide)
    
  }
}

#> aqli bar plot function (ViT color check complete and matched with note)---------------------------------------------------------

## arguments:

# df: AQLI dataset in question
# scale_type: "pollution" or "lyl"
# x_var: col name (in quotes) to be plotted on the x-axis.
# y_var: col name (in quotes) to be plotted on the y-axis.
# subtitle: subtitle for the figure
# x_label: x axis label for the figure
# y_label: y axis label for the figure
# legend_title: legend title
# caption: figure caption

aqli_bar <- function(df, scale_type = "pollution", x_var, y_var, title, subtitle, x_label, y_label, legend_title, caption){
  if(scale_type == "pollution"){
    plt <- df %>%
      add_aqli_color_scale_buckets(scale_type = "pollution", col_name = y_var) %>%
      ggplot() +
      geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol(x_var), !!as.symbol(y_var)), y = !!as.symbol(y_var), fill = forcats::fct_reorder(!!as.symbol("pol_bucket"), !!as.symbol("order_pol_bucket")))) +
      scale_fill_manual(values = c("0 to < 5" = "#a1f5ff",
                                   "5 to < 10" = "#92d4eb",
                                   "10 to < 20" = "#82b5d5",
                                   "20 to < 30" = "#7197be",
                                   "30 to < 40" = "#5f7aa5",
                                   "40 to < 50" = "#4e5e8b",
                                   "50 to < 60" = "#3c456f",
                                   "60 to < 70" = "#2b2d54",
                                   ">= 70" = "#1a1638")) +
      labs(x = x_label, y = y_label, title = title, subtitle = subtitle, caption = caption, fill = legend_title) +
      themes_aqli_base +
      coord_flip()
    return(plt)
    
  } else if(scale_type == "lyl"){
    plt <- df %>%
      add_aqli_color_scale_buckets(scale_type = "lyl", col_name = y_var) %>%
      ggplot() +
      geom_col(mapping = aes(x = forcats::fct_reorder(!!as.symbol(x_var), !!as.symbol(y_var)), y = !!as.symbol(y_var), fill = forcats::fct_reorder(!!as.symbol("lyl_bucket"), !!as.symbol("order_lyl_bucket")))) +
      scale_fill_manual(values = c("0 to < 0.1" = "#ffffff",
                                   "0.1 to < 0.5" = "#ffeda0",
                                   "0.5 to < 1" = "#fed976",
                                   "1 to < 2" = "#feb24c",
                                   "2 to < 3" = "#fd8d3c",
                                   "3 to < 4" = "#fc4e2a",
                                   "4 to < 5" = "#e31a1c",
                                   "5 to < 6" = "#bd0026",
                                   ">= 6" = "#800026")) +
      labs(x = x_label, y = y_label, title = title, subtitle = subtitle, caption = caption, fill = legend_title) +
      themes_aqli_base +
      coord_flip()
    return(plt)
    
  }
}