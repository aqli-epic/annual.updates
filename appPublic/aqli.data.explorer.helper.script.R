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

# load and document to keep things updated
# devtools::load_all()
# devtools::document()

# global variables
`%notin%` <- Negate(`%in%`)

# read in latest color file
gadm2_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/gadm2_aqli2021_vit.csv")
gadm1_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/gadm1_aqli2021_vit.csv")
gadm0_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/gadm0_aqli2021_vit.csv")

# read in the shapefile with pollution and lyl data
# gadm2_aqli_2021_shp <- sf::st_read("./september.2023/master.dataset/shapefiles/master_global_allyears_gadm2_with_geom_Jan192023.shp")

#> join each one of these with the country continent file, so that each one of these has a continent column

# read in the country continent file
country_continent <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/other.important.calculations.data/country_continent.csv")

continent_for_missing_countries <- c("")

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
continents_for_missing_countries <- c("Europe", "Asia", "Africa", "North America",
                                      "Europe", "Oceania", "North America", "Asia", "Asia",
                                      "Oceania", "Africa", "Africa", "Africa", "South America",
                                      "Asia")

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

# trendlines tab graph function-----------------------------------

trendlines_aqli <- function(gadm2_file, level = "country", country_name = "India", state_name = "NCT of Delhi", district_name = "NCT of Delhi", start_year, end_year){

  pm_weighted_col_start <- stringr::str_c("pm", start_year, "_weighted")
  pm_weighted_col_end <- stringr::str_c("pm", end_year, "_weighted")

  if(level == "country"){
    trendlines_aqli_data <- gadm2_file %>%
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

#> GADM level summary tab function---------------------------------------

gadm_level_summary <- function(df, level_col_name_vec, years){

  pol_col_names <- stringr::str_c("pm", years)
  llpp_who_col_names <- stringr::str_c("llpp_who_", years)
  llpp_nat_col_names <- stringr::str_c("llpp_nat_", years)

if((level_col_name_vec[1] == "continent") & (length(level_col_name_vec) == 1)){
 aqli_wide <- df %>%
    dplyr::group_by_at(level_col_name_vec) %>%
    dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE)) %>%
    dplyr::mutate(across(dplyr::starts_with("pm"), ~(.x*pop_weights), .names = "{col}_pop_weighted")) %>%
    dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 2)), .names = "avg_{col}"),
                     total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], .groups = "keep") %>%
    select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, dplyr::everything()) %>%
    dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
    dplyr::rename(population = total_population,
                  objectid_level = objectid_gadm2) %>%
    dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
    dplyr::mutate(across(starts_with("pm"), (~(.x - whostandard)*le_constant), .names = "llpp_who_{col}")) %>%
    dplyr::mutate(across(starts_with("llpp"), ~ifelse(.x < 0, 0, .x))) %>%
    dplyr::select(objectid_level, iso_alpha3, all_of(level_col_name_vec), population, whostandard, dplyr::everything()) %>%
    dplyr::rename_with(~str_replace(.x, "pm", ""), dplyr::contains("llpp")) %>%
    dplyr::mutate(across(dplyr::matches("pm|llpp"), ~(round(.x, 1)), .names = "{col}")) %>%
   dplyr::ungroup() %>%
   dplyr::select(objectid_level:whostandard, all_of(c(pol_col_names, llpp_who_col_names)))

 return(aqli_wide)

} else if(("name_2" %in% level_col_name_vec)){
    if((which(level_col_name_vec == "name_2") > 2)){
      aqli_wide <- df %>%
        dplyr::select(objectid_gadm2:natstandard, continent, all_of(c(pol_col_names, llpp_who_col_names, llpp_nat_col_names)))
    }

  return(aqli_wide)

} else {
  aqli_wide <- df %>%
    dplyr::group_by_at(level_col_name_vec) %>%
    dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE)) %>%
    dplyr::mutate(across(dplyr::starts_with("pm"), ~(.x*pop_weights), .names = "{col}_pop_weighted")) %>%
    dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 2)), .names = "avg_{col}"),
                     total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], natstandard = natstandard[1], .groups = "keep") %>%
    select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, natstandard, dplyr::everything()) %>%
    dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
    dplyr::rename(population = total_population,
                  objectid_level = objectid_gadm2) %>%
    dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
    dplyr::mutate(across(starts_with("pm"), (~(.x - whostandard)*le_constant), .names = "llpp_who_{col}")) %>%
    dplyr::mutate(across(starts_with("pm"), (~(.x - natstandard)*le_constant), .names = "llpp_nat_{col}")) %>%
    dplyr::mutate(across(starts_with("llpp"), ~ifelse(.x < 0, 0, .x))) %>%
    dplyr::mutate(across(starts_with("llpp_nat"), ~ifelse(natstandard == 0, NA, .x))) %>%
    dplyr::select(objectid_level, iso_alpha3, all_of(level_col_name_vec), population, whostandard, natstandard, dplyr::everything()) %>%
    dplyr::rename_with(~str_replace(.x, "pm", ""), dplyr::contains("llpp")) %>%
    dplyr::mutate(across(dplyr::matches("pm|llpp"), ~(round(.x, 1)), .names = "{col}")) %>%
    dplyr::ungroup() %>%
    dplyr::select(objectid_level:natstandard, all_of(c(pol_col_names, llpp_who_col_names, llpp_nat_col_names)))

  return(aqli_wide)

   }
}

