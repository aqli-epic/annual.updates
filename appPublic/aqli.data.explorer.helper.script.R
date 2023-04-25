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
who_guideline <- 5

# read in latest color file
gadm2_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/[missingAndNAPopRegionsIncorpButViTcolNameChangesPartiallyIncorp]master_global_allyears_gadm2_non_geom.csv")
gadm1_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/[missingAndNAPopRegionsIncorpButViTcolNameChangesPartiallyIncorp]master_global_allyears_gadm1_non_geom.csv")
gadm0_aqli_2021 <- readr::read_csv("C:/Users/Aarsh/Desktop/aqli-epic/annual.updates/september.2023/master.dataset/[missingAndNAPopRegionsIncorpButViTcolNameChangesPartiallyIncorp]master_global_allyears_gadm0_non_geom.csv")

# updating the natstandard column (replacing 0's with NAs, corresponding llpp_nat columns already have NAs whereever natstandard is not available)
gadm2_aqli_2021 <- gadm2_aqli_2021 %>%
  dplyr::mutate(natstandard = ifelse(natstandard == 0, NA, natstandard))

gadm1_aqli_2021 <- gadm1_aqli_2021 %>%
  dplyr::mutate(natstandard = ifelse(natstandard == 0, NA, natstandard))

gadm0_aqli_2021 <- gadm0_aqli_2021 %>%
  dplyr::mutate(natstandard = ifelse(natstandard == 0, NA, natstandard))


# bringing the colnames back into the format in which our internal dashboard expects it (the above files are in a format in which ViT expects the colnames)
gadm2_aqli_2021 <- gadm2_aqli_2021 %>%
  dplyr::rename_with(~str_replace(.x, "who", "llpp_who_"), dplyr::contains("who")) %>%
  dplyr::rename_with(~str_replace(.x, "nat", "llpp_nat_"), dplyr::contains("nat")) %>%
  rename(whostandard = llpp_who_standard,
         natstandard = llpp_nat_standard)

gadm1_aqli_2021 <- gadm1_aqli_2021 %>%
  dplyr::rename_with(~str_replace(.x, "who", "llpp_who_"), dplyr::contains("who")) %>%
  dplyr::rename_with(~str_replace(.x, "nat", "llpp_nat_"), dplyr::contains("nat")) %>%
  rename(whostandard = llpp_who_standard,
         natstandard = llpp_nat_standard)

gadm0_aqli_2021 <- gadm0_aqli_2021 %>%
  dplyr::rename_with(~str_replace(.x, "who", "llpp_who_"), dplyr::contains("who")) %>%
  dplyr::rename_with(~str_replace(.x, "nat", "llpp_nat_"), dplyr::contains("nat")) %>%
  rename(whostandard = llpp_who_standard,
         natstandard = llpp_nat_standard)

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
continents_for_missing_countries <- c("Africa", "Europe", "North America", "Asia", "Asia",
                                      "Europe", "Africa", "Africa", "Africa", "Asia")

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

#> GADM level summary and Compare Regions tabs function---------------------------------------

gadm_level_summary <- function(df, level_col_name_vec, years, perc_red_by){

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
    dplyr::summarise(across(dplyr::contains("pop_weighted"), ~(round(sum(.x, na.rm = TRUE), 2)), .names = "avg_{col}"),
                     total_population = sum(population, na.rm = TRUE), objectid_gadm2 = objectid_gadm2[1], iso_alpha3 = iso_alpha3[1], whostandard = whostandard[1], .groups = "keep") %>%
    select(objectid_gadm2, iso_alpha3, all_of(level_col_name_vec), total_population, whostandard, dplyr::everything()) %>%
    dplyr::mutate(objectid_gadm2 = dplyr::row_number()) %>%
    dplyr::rename(population = total_population,
                  objectid_level = objectid_gadm2) %>%
    dplyr::rename_with(~str_replace_all(.x, "(_pop_weighted)|(avg_)", ""), dplyr::contains("_pop_weighted")) %>%
    dplyr::mutate(across(starts_with("pm"), (~(.x - whostandard)*le_constant), .names = "llpp_who_{col}")) %>%
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
   dplyr::select(objectid_level:whostandard, all_of(c(pol_col_names, pol_col_names_red_to, llpp_who_col_names, llpp_pol_red_names, total_lyl_who_columns, total_lyl_pol_red_to_columns)))

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
                                                                      total_lyl_nat_columns, total_lyl_pol_red_to_columns)))
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
    dplyr::mutate(across(starts_with("pm"), (~round((.x*((perc_red_by)/100)), 1)*le_constant), .names = "llpp_pol_red_{col}")) %>%
    dplyr::mutate(across(starts_with("pm"), (~(.x - natstandard)*le_constant), .names = "llpp_nat_{col}")) %>%
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
                                                       total_lyl_nat_columns, total_lyl_pol_red_to_columns)))

  return(aqli_wide)

   }
}

# CH China data request----

china_pm2.5_data <- gadm_level_summary(gadm2_aqli_2021, c("continent", "country", "name_1", "name_2"), c(1998:2021), 10) %>%
  filter(country == "China")

# pol 2020
china_pm2.5_data %>%
  filter(name_1 == "Hebei") %>%
  select(objectid_gadm2:pm2021) %>%
  tidyr::pivot_longer(dplyr::starts_with("pm"), names_to = "year", values_to = "pm2.5_pollution") %>%
  dplyr::mutate(year = as.numeric(str_remove(year, "pm"))) %>%
  dplyr::select(objectid_gadm2:natstandard, year, pm2.5_pollution) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  filter(year == 2020) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(name_2, pm2.5_pollution), y = pm2.5_pollution, fill = pm2.5_pollution), width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "City", y = expression(paste("2020 Annual Average PM"[2.5], " (in ", mu, "g/m"^"3", ")")),
                title = expression("2020" ~ PM[2.5] ~ "Pollution comparion"),
                subtitle = "(All cities of Hebei Province, China)",
                fill = expression(paste("Annual Average PM"[2.5], " (in ", mu, "g/m"^"3", ")"))) +
  ggthemes::theme_hc() +
  ggplot2::theme(plot.title = element_text(hjust = 0.5),
                 axis.line = element_line(),
                 legend.title = element_text(hjust = 0.5),
                 legend.position = "none") +
  scale_fill_gradient2(
    low = "#CBE8F3",
    mid = "#8FA1AD",
    high = "#1C2B39",
    midpoint = .5
  ) +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 8, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 60, 10))

# lyl who 2020
china_pm2.5_data %>%
  filter(name_1 == "Hebei") %>%
  select(objectid_gadm2:natstandard, starts_with("llpp_who_")) %>%
  tidyr::pivot_longer(dplyr::starts_with("llpp_who_"), names_to = "year", values_to = "lyl_who") %>%
  dplyr::mutate(year = as.numeric(str_remove(year, "llpp_who_"))) %>%
  dplyr::select(objectid_gadm2:natstandard, year, lyl_who) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  filter(year == 2020) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(name_2, lyl_who), y = lyl_who, fill = lyl_who), width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "City", y = expression(paste("Life Years Lost relative to WHO PM"[2.5], " guideline (5 ", mu, "g/m"^"3", ")")),
                title = "Life Years Lost to air pollution in 2020",
                subtitle = "(All cities of Hebei Province, China)",
                fill = "Life Years Lost",
                caption = expression("*Note that the WHO" ~ PM[2.5] ~ "guideline is an annual average.")) +
  ggthemes::theme_hc() +
  ggplot2::theme(plot.title = element_text(hjust = 0.5),
                 axis.line = element_line(),
                 legend.title = element_text(hjust = 0.5),
                 legend.position = "none") +
  scale_fill_gradient2(
    low = "#FFE6B3",
    mid = "#FF9600",
    high = "#8C130E",
    midpoint = .5
  ) +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.title = element_text(size = 9),
        plot.caption = element_text(hjust = 0, size = 6)) +
  scale_y_continuous(breaks = seq(0, 6, 1))


# lyl nat 2020

china_pm2.5_data %>%
  filter(name_1 == "Hebei") %>%
  select(objectid_gadm2:natstandard, starts_with("llpp_nat_")) %>%
  tidyr::pivot_longer(dplyr::starts_with("llpp_nat_"), names_to = "year", values_to = "lyl_nat") %>%
  dplyr::mutate(year = as.numeric(str_remove(year, "llpp_nat_"))) %>%
  dplyr::select(objectid_gadm2:natstandard, year, lyl_nat) %>%
  dplyr::mutate(year = as.factor(year)) %>%
  filter(year == 2020) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(mapping = aes(x = forcats::fct_reorder(name_2, lyl_nat), y = lyl_nat, fill = lyl_nat), width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "City", y = expression(paste("Life Years Lost relative to National PM"[2.5], " guideline (35 ", mu, "g/m"^"3", ")")),
                title = "Life Years Lost to air pollution in 2020",
                subtitle = "(All cities of Hebei Province, China)",
                fill = "Life Years Lost",
                caption = str_wrap("*Note that the China PM2.5 guideline specified above is an annual average. All, but 3 cities of Hebei province
                have pollution levels above national standards.", 50)) +
  ggthemes::theme_hc() +
  ggplot2::theme(plot.title = element_text(hjust = 0.5),
                 axis.line = element_line(),
                 legend.title = element_text(hjust = 0.5),
                 legend.position = "none") +
  scale_fill_gradient2(
    low = "#FFE6B3",
    mid = "#FF9600",
    high = "#8C130E",
    midpoint = .5
  ) +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.title = element_text(size = 9),
        plot.caption = element_text(hjust = 0, size = 6)) +
  scale_y_continuous(breaks = seq(0, 3, 0.5))


# trendlines

foo <- gadm2_aqli_2021 %>%
  dplyr::filter(country == "China", name_1 == "Hebei", name_2 == "Shijiazhuang") %>%
  dplyr::group_by(country, name_1, name_2) %>%
  dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  dplyr::summarise(across(ends_with("weighted"), sum)) %>%
  tidyr::pivot_longer(cols = pm1998_weighted:pm2021_weighted , names_to = "years",
                      values_to = "pop_weighted_avg_pm2.5") %>%
  dplyr::mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
                region = "District Average") %>%
  dplyr::select(years, region, pop_weighted_avg_pm2.5) %>%
  filter(years < 2021)

foo %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                     color = "darkred") +
  ggplot2::scale_x_continuous(breaks = seq(1998, 2021, 2)) +
  # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
  ggthemes::theme_clean() +
  ggplot2::labs(x = "Years",
                y = expression(paste("Annual Average PM"[2.5], " (in ", mu, "g/m"^"3", ")")),
                title = expression("Annual average" ~ PM[2.5] ~ "in Shijiazhuang from 1998 to 2020")) +
  ggplot2::theme(legend.position = "bottom", legend.title = element_blank(),
                 legend.text = element_text(size = 7),
                 axis.title.y = element_text(size = 9),
                 axis.title.x = element_text(size = 9)) +
  scale_y_continuous(breaks = seq(0, 110, 10), limits = c(0, 110)) +
  theme(plot.title = element_text(size = 11, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5),
        axis.title = element_text(size = 9),
        plot.caption = element_text(hjust = 0, size = 6))

