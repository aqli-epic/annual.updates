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

# read in latest color file
gadm2_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/gadm2_aqli2021_vit.csv")
gadm1_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/gadm1_aqli2021_vit.csv")
gadm0_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/gadm0_aqli2021_vit.csv")

# read in the shapefile with pollution and lyl data
# gadm2_aqli_2021_shp <- sf::st_read("./september.2023/master.dataset/shapefiles/master_global_allyears_gadm2_with_geom_Jan192023.shp")

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

gadm_level_summary <- function(df, level_col_name, year){

  pol_col_name <- stringr::str_c("pm", year)
  llpp_who_col_name <- stringr::str_c("llpp_who_", year)
  # llpp_nat_col_name <- stringr::str_c("llpp_nat_", year)

  df %>%
    group_by(!!as.symbol(level_col_name)) %>%
    mutate(pop_weights = population/sum(population, na.rm = TRUE),
           pm_pop_weighted = pop_weights*(!!(as.symbol(pol_col_name)))) %>%
    summarise(avg_pm2.5 = sum(pm_pop_weighted, na.rm = TRUE),
              lyl_who = (avg_pm2.5 - who_guideline)*0.098,
              lyl_who = ifelse(lyl_who < 0, 0, lyl_who)) %>%
    return()
}
