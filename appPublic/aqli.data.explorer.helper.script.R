# Global variables, datasets and packages--------------------------------------

# metadata
# author: Aarsh Batra
# email: aarshbatra.in@gmail.com

# libraries
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(readr)
library(tidytext)
library(tidyr)
library(tidyverse)
library(sf)
library(usethis)
library(devtools)
library(data.table)
library(svglite)

# load and document to keep things updated
# devtools::load_all()
# devtools::document()

# global variables
`%notin%` <- Negate(`%in%`)
who_guideline <- 5

# read in latest color file
gadm2_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/[june302023]gadm2_aqli_2021_internal_col_names.csv")
gadm1_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/[june302023]gadm1_aqli_2021_internal_col_names.csv")
gadm0_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/[june302023]gadm0_aqli_2021_internal_col_names.csv")

# load in the raw colormap shapefile
# colormap <- st_read("./september.2023/master.dataset/shapefiles/colormap.shp")

#> waterbodies--------------------------------------------- (not needed)

# # load in AQLI waterbodies, remove the "type" column from it and figure out the object id's in the gadm 2 data set that correspond to the
# # waterbodies
# waterbodies_aqli <- read_csv("./september.2023/other.important.calculations.data/waterbodies_aqli.csv")
# waterbodies_aqli <- waterbodies_aqli %>%
#   select(-type)
#
# gadm2_aqli_2021_with_waterbodies <- read_csv("./september.2023/master.dataset/[finalizedApr2023]gadm2_aqli2021_vit.csv")
#
# # create a version of old colnames gadm2 file that has waterbodies removed from it
# aqli_gadm2_collapse_master_finalized_wat_adj_internal <- gadm2_aqli_2021_with_waterbodies %>%
#   semi_join(waterbodies_aqli, by = c("name0" = "gadm0", "name1" = "gadm1", "name2" = "gadm2"))
#
# objids_waterbodies <- aqli_gadm2_collapse_master_finalized_wat_adj_internal %>%
#   select(objectid_gadm2) %>%
#   unlist() %>%
#   as_vector()
#
# # removing waterbodies from colormap
# colormap_sans_waterbodies <- colormap %>%
#   filter(objectid %notin% objids_waterbodies)
#
# # gadm 2 objectids for no data regions
# foo <- colormap_sans_waterbodies %>%
#   anti_join(gadm2_aqli_2021, by = c("objectid" = "objectid_gadm2"))



#------------------------------------------------------------- (not needed)


# updating the natstandard column (replacing 0's with NAs, corresponding llpp_nat columns already have NAs whereever natstandard is not available)
# gadm2_aqli_2021 <- gadm2_aqli_2021 %>%
#   dplyr::mutate(natstandard = ifelse(natstandard == 0, NA, natstandard))
#
# gadm1_aqli_2021 <- gadm1_aqli_2021 %>%
#   dplyr::mutate(natstandard = ifelse(natstandard == 0, NA, natstandard))
#
# gadm0_aqli_2021 <- gadm0_aqli_2021 %>%
#   dplyr::mutate(natstandard = ifelse(natstandard == 0, NA, natstandard))


# bringing the colnames back into the format in which our internal dashboard expects it (the above files are in a format in which ViT expects the colnames)
# gadm2_aqli_2021 <- gadm2_aqli_2021 %>%
#   dplyr::rename_with(~str_replace(.x, "who", "llpp_who_"), dplyr::contains("who")) %>%
#   dplyr::rename_with(~str_replace(.x, "nat", "llpp_nat_"), dplyr::contains("nat")) %>%
#   rename(whostandard = llpp_who_standard,
#          natstandard = llpp_nat_standard)
#
# gadm1_aqli_2021 <- gadm1_aqli_2021 %>%
#   dplyr::rename_with(~str_replace(.x, "who", "llpp_who_"), dplyr::contains("who")) %>%
#   dplyr::rename_with(~str_replace(.x, "nat", "llpp_nat_"), dplyr::contains("nat")) %>%
#   rename(whostandard = llpp_who_standard,
#          natstandard = llpp_nat_standard)
#
# gadm0_aqli_2021 <- gadm0_aqli_2021 %>%
#   dplyr::rename_with(~str_replace(.x, "who", "llpp_who_"), dplyr::contains("who")) %>%
#   dplyr::rename_with(~str_replace(.x, "nat", "llpp_nat_"), dplyr::contains("nat")) %>%
#   rename(whostandard = llpp_who_standard,
#          natstandard = llpp_nat_standard)


# read in the shapefile with pollution and lyl data
gadm2_aqli_2021_shp <- sf::st_read("./september.2023/master.dataset/shapefiles/june302023_post_missing_data_incorp_nat_stan_adj/aqli_gadm2_final_june302023.shp")
gadm1_aqli_2021_shp <- sf::st_read("./september.2023/master.dataset/shapefiles/june302023_post_missing_data_incorp_nat_stan_adj/aqli_gadm1_final_june302023.shp")
gadm0_aqli_2021_shp <- sf::st_read("./september.2023/master.dataset/shapefiles/june302023_post_missing_data_incorp_nat_stan_adj/aqli_gadm0_final_june302023.shp")

# india state
india_state <- st_read("./june.2022/clean.air.countdown.india.campaign.2023/input.data.shared.between.graphs/india_state.shp")

#> join each one of these with the country continent file, so that each one of these has a continent column

# read in the country continent file
country_continent <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/other.important.calculations.data/country_continent.csv")

# join each of the above 3 datasets with the country_continent file using the continent column
gadm2_aqli_2021 <- gadm2_aqli_2021 %>%
  left_join(country_continent, by = "country")

gadm1_aqli_2021 <- gadm1_aqli_2021 %>%
  left_join(country_continent, by = "country")

gadm0_aqli_2021 <- gadm0_aqli_2021 %>%
  left_join(country_continent, by = "country")

#> Filling in missing continents-----------------------------------------------

# countries for which continent is NA: for these fill in the continent manually
countries_with_missing_continent <- gadm0_aqli_2021 %>% filter(is.na(continent)) %>% pull(country) %>% unique()

# continent fill in for missing coutries
continents_for_missing_countries <- c("North America", "Africa")

# [CAUTION: perform a sanity check on the above 2 vectors and how they map countries to continents before proceeding]
#creating a data frame using the above 2 vectors as columns
missing_continents_df <- tibble(country = countries_with_missing_continent,
                                continent = continents_for_missing_countries)


# adding in the missing continent information in the gadmx_aqli_2021 datasets
gadm2_aqli_2021 <- gadm2_aqli_2021 %>%
  left_join(missing_continents_df, by = "country") %>%
  mutate(continent = ifelse(is.na(continent.x), continent.y, continent.x))

gadm1_aqli_2021 <- gadm1_aqli_2021 %>%
  left_join(missing_continents_df, by = "country") %>%
  mutate(continent = ifelse(is.na(continent.x), continent.y, continent.x))

gadm0_aqli_2021 <- gadm0_aqli_2021 %>%
  left_join(missing_continents_df, by = "country") %>%
  mutate(continent = ifelse(is.na(continent.x), continent.y, continent.x))


# global variables
who_guideline <- 5
le_constant <- 0.098
latest_year <- 2021
first_year <- 1998

# global operations
`%notin%` <- Negate(`%in%`)

#> gbd results-----------

# gbd results master
gbd_results_master_2021 <- read_csv("./september.2023/gbd.calculation/GBDComparisons/results_used_in_dashboard/gbd_results_master.csv")


# US 1970 calculation master cleaned file read
us_1970_calc_results_cleaned <- read_csv("./september.2023/other.important.calculations.data/county_pm25_foraqli_stats_cleaned.csv")


#> raw file is stored in the raw folder within other.important.calculations.data folder and below is the code to clean it just fyi. No, need
#> to run this, as we already are reading in the cleaned file above.

# us_1970_calc_results_raw <- read_csv("./september.2023/other.important.calculations.data/raw/county_pm25_foraqli_stats_raw.csv")
#
# us_1970_calc_results_cleaned <- us_1970_calc_results_raw %>%
#   left_join(gadm2_aqli_2021 %>% filter(country == "United States"), by = c("country", "name_1", "name_2")) %>%
#   select(objectid_gadm2.x, iso_alpha3, country, name_1, name_2, population.x, whostandard, natstandard, pm25_1970_aqli, pm1998.x, pm1999:pm2020, pm2021.x, llpp_who_1998:llpp_who_2020, llpp_who_2021.x, llpp_nat_1998:llpp_nat_2021) %>%
#   rename(objectid_gadm2 = objectid_gadm2.x, population = population.x, pm1998 = pm1998.x, llpp_who_2021 = llpp_who_2021.x,
#          pm1970 = pm25_1970_aqli, pm2021 = pm2021.x)



#> other region wise defintions

# central africa definition
central_african_countries <- c("Angola", "Burundi", "Cameroon",
                               "Central African Republic", "Chad",
                               "Republic of the Congo",
                               "Democratic Republic of the Congo",
                               "Equatorial Guinea", "Gabon",
                               "São Tomé and Príncipe",
                               "Rwanda")
# west africa definition
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde",
                            "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
                            "Côte d'Ivoire", "Liberia", "Mali", "Mauritania",
                            "Niger", "Nigeria", "Senegal", "Sierra Leone",
                            "Togo")

# central and west africa countries definition, combine in a single vector
central_and_west_african_countries <- c(central_african_countries, west_african_countries)

# South East Asia definition
se_asia_vec <- c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia", "Laos", "Malaysia", "Philippines", "Singapore", "Thailand", "Vietnam")

# indo gangetic plains states

indo_gangetic_plains_states <- c("NCT of Delhi", "Uttar Pradesh", "Bihar", "Haryana",
                                 "Punjab", "Chandigarh", "West Bengal")


# european countries
european_countries <- read_csv("./september.2023/other.important.calculations.data/europe_countries.csv")

# western european countries
western_european_countries <- c("Germany", "Switzerland", "Italy", "Monaco", "Luxembourg",
                                "Belgium", "France", "Netherlands", "Andorra", "Spain",
                                "United Kingdom", "Portugal", "Denmark", "Ireland", "Iceland", "Austria")


# European Union countries
eu_countries <- c("Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia",
                  "Ireland", "Greece", "Spain", "France",
                  "Croatia", "Italy", "Cyprus", "Latvia",
                  "Lithuania", "Luxembourg", "Hungary",
                  "Malta", "Netherlands",
                  "Austria", "Poland",
                  "Portugal", "Romania",
                  "Slovenia", "Slovakia", "Finland", "Sweden",
                  "United Kingdom")

# South Asia definition
south_asia_def <- c("Afghanistan", "Bangladesh",
                    "Bhutan", "India",
                    "Maldives", "Nepal",
                    "Pakistan", "Sri Lanka")

# latin america definition

latin_america_countries_vec <- c("México", "Guatemala", "Honduras",
                                 "El Salvador", "Nicaragua",
                                 "Costa Rica", "Panama",
                                 "Colombia", "Venezuela",
                                 "Ecuador", "Peru",
                                 "Bolivia", "Brazil",
                                 "Paraguay", "Chile",
                                 "Argentina", "Uruguay",
                                 "Cuba", "Haiti", "Dominican Republic",
                                 "Puerto Rico")

#> open AQ global landscape 2022 report and clean-------------

# read data
openaq_data <- read_csv("./september.2023/other.important.calculations.data/openAQDataGlobalLandscape2022_apr2023_continent_adj.csv")

# # remove top rows and some columns
openaq_data <- openaq_data[7:nrow(openaq_data), ]
colnames(openaq_data) <- openaq_data[1, ]
openaq_data <- openaq_data[2:nrow(openaq_data), ]

# set column names
colnames(openaq_data) <- c("country", "evid_govt_spon_aq_mon", "evidence_ad_hoc_studies", "aq_existed_in_past_not_cur",
                           "data_in_phys_units", "transp_geog_scale_data_provided", "data_fine_temporal_scale",
                           "prog_access", "continent")


#> load in archived AQLI gadm2 datasets from past years (and making their colnames same as gadm2_aqli_2021)---------------

# read in
color_2020 <- read_csv("./september.2023/master.dataset/archive/color_2020.csv")
color_2019 <- read_csv("./september.2023/master.dataset/archive/color_2019.csv")
color_2016 <- read_csv("./september.2023/master.dataset/archive/color_2016.csv")

# bring in same format as gadm2_aqli_2021
color_2020 <- color_2020 %>%
  dplyr::rename_with(~str_replace(.x, "llpp_who_", "llpp_who5_"), dplyr::contains("who")) %>%
  rename(objectid_gadm2 = objectid_color)

color_2019 <- color_2019 %>%
  dplyr::rename_with(~str_replace(.x, "llpp_who_", "llpp_who5_"), dplyr::contains("who")) %>%
  rename(objectid_gadm2 = objectid_color)

color_2016 <- color_2016 %>%
  dplyr::rename_with(~str_replace(.x, "llpp_who_", "llpp_who5_"), dplyr::contains("who")) %>%
  rename(objectid_gadm2 = objectid_color)

# natstandard AQLI (last updated: June 2023) raw file
natstan_aqli <- read_csv("./september.2023/other.important.calculations.data/country_annual_average_pm2.5_standards_asInJune2023.csv")


#> trendlines tab graph function-----------------------------------

## arguments info (a Roxygen file is upcoming):
# gadm2_file: gadm2 aqli finalized file
# level: one of, "country", "state", "district"
# country_name: if applicable (given the level argument)
# state_name: if applicable (given the level argument)
# district_name: if applicable (given the level argument)
# start_year: start year of trendline
# end_year: end year of trendline


trendlines_aqli <- function(gadm2_file, level = "country", country_name = "India", state_name = "NCT of Delhi", district_name = "NCT of Delhi", start_year, end_year){

  pm_weighted_col_start <- stringr::str_c("pm", start_year, "_weighted")
  pm_weighted_col_end <- stringr::str_c("pm", end_year, "_weighted")

  if(level == "country"){
    trendlines_aqli_data <- gadm2_file %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::filter(country == country_name) %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "National Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
     ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                color = "darkred") +
      ggplot2::scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Years",
           y = "Annual Average PM2.5 concentration (in µg/m3)") +
      ggplot2::theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 9))


  } else if (level == "state") {
    trendlines_aqli_data <- gadm2_file %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::filter(country == country_name, name_1 == state_name) %>%
      dplyr::group_by(country, name_1) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "State Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                color = "darkred") +
      ggplot2::scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Years",
           y = "Annual Average PM2.5 concentration (in µg/m3)") +
      ggplot2::theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 9))

  } else if (level == "district") {
    trendlines_aqli_data <- gadm2_file %>%
      dplyr::filter(!is.na(population)) %>%
      dplyr::filter(country == country_name, name_1 == state_name, name_2 == district_name) %>%
      dplyr::group_by(country, name_1, name_2) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      dplyr::summarise(across(ends_with("weighted"), sum)) %>%
      tidyr::pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "District Average") %>%
      dplyr::select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                color = "darkred") +
      ggplot2::scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_hc() +
      ggplot2::labs(x = "Years",
           y = "Annual Average PM2.5 concentration (in µg/m3)") +
      ggplot2::theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 9))

  }

  return(trendlines_aqli_plt)

}

#> GADM level summary and Compare Regions tabs function---------------------------------------


## arguments (Roxygen file upcoming):
# df: AQLI datasets (either gadm2, 1, 0, given the use in question)

# level_col_name_vec: specify complete vector of the level of summary. There are 2 cases:
#  (a) if you need a continent level summary, just enter: c("continent").
#  (b) For any other level summary,  you'll have to add the entire vector, starting from country, For example, to get a
#      state level summary, enter: c("country", "name_1"). Similarly, for district level summary,
#      enter, c("country", "name_1", "name_2").

# years: a vector of years for which you need the data, example: c(2021, 2020)

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

# when level  just equal to continent, deal separately, as in this case natstandard column should not be included
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

#> aqli base theme function---------------------------------------------------------------------------
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

#> function that adds AQLI colors and axis titles (ViT color check complete, matched with note)

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

#> aqli histogram function (ViT color check complete, matched with note)-----------------------------------------

# df: AQLI datasets
# scale_type: one of "pollution" or "lyl"
# col_name: col_name for which the histogram needs to be plotted.
# region_name: this will go in the subtitle of the figure.

aqli_hist <- function(df, scale_type = "pollution", col_name = "pm2021", region_name = "enter region name"){
  if(scale_type == "pollution"){
    pol_year <- as.numeric(str_remove(col_name, "pm"))
    x_axis_title <- str_c("Annual average PM2.5 in ", pol_year, "(µg/m³)")
    y_axis_title <- "Number of people"
    plot_title <- str_c("Distribution of Annual Average PM2.5 pollution in", pol_year)
    plot_subtitle <- region_name
    plot_caption <- "*AQLI only reports satellite derived annual average PM2.5 data"
    legend_title <- "PM2.5 (in µg/m³)"
    plt <- df %>%
      add_aqli_color_scale_buckets(scale_type = "pollution", col_name = col_name) %>%
      ggplot() +
      geom_histogram(mapping = aes(x = !!as.symbol(col_name), weight = population, fill = !!as.symbol(col_name), group = !!as.symbol(col_name))) +
      scale_fill_gradient(low = "#a1f5ff",
                          high = "#1a1638") +
      labs(x = x_axis_title, y = y_axis_title, title = plot_title, subtitle = plot_subtitle, caption = plot_caption,
           fill = legend_title) +
      themes_aqli_base
    return(plt)

  } else if(scale_type == "lyl"){
    lyl_year <- as.numeric(str_remove(col_name, "llpp_who_"))
    x_axis_title <- str_c("Life years lost to PM2.5 pollution in ", lyl_year)
    y_axis_title <- "Number of people"
    plot_title <- str_c("Distribution of life years lost to PM2.5 pollution in ", lyl_year)
    plot_subtitle <- region_name
    plot_caption <- "*AQLI only reports satellite derived annual average PM2.5 data."
    legend_title <- "Life years lost"
    plt <- df %>%
      add_aqli_color_scale_buckets(scale_type = "lyl", col_name = col_name) %>%
      ggplot() +
      geom_histogram(mapping = aes(x = !!as.symbol(col_name), weight = population, fill = !!as.symbol(col_name), group = !!as.symbol(col_name))) +
      labs(x = x_axis_title, y = y_axis_title, title = plot_title, subtitle = plot_subtitle, caption = plot_caption,
           fill = legend_title) +
      scale_fill_gradient(low = "#ffeda0",
                          high = "#800026") +
      themes_aqli_base




    return(plt)

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

