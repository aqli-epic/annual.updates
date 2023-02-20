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

  pm_weighted_col_start <- str_c("pm", start_year, "_weighted")
  pm_weighted_col_end <- str_c("pm", end_year, "_weighted")

  if(level == "country"){
    trendlines_aqli_data <- gadm2_file %>%
      filter(country == country_name) %>%
      group_by(country) %>%
      mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      summarise(across(ends_with("weighted"), sum)) %>%
      pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "National Average") %>%
      select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
      ggplot() +
      geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                color = "darkred") +
      scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_clean() +
      labs(x = "Years",
           y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 9))


  } else if (level == "state") {
    trendlines_aqli_data <- gadm2_file %>%
      filter(country == country_name, name_1 == state_name) %>%
      group_by(country, name_1) %>%
      mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      summarise(across(ends_with("weighted"), sum)) %>%
      pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "State Average") %>%
      select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
      ggplot() +
      geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                color = "darkred") +
      scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_clean() +
      labs(x = "Years",
           y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 9))

  } else if (level == "district") {
    trendlines_aqli_data <- gadm2_file %>%
      filter(country == country_name, name_1 == state_name, name_2 == district_name) %>%
      group_by(country, name_1, name_2) %>%
      mutate(pop_weights = population/sum(population, na.rm = TRUE),
             mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
      summarise(across(ends_with("weighted"), sum)) %>%
      pivot_longer(cols = !!as.symbol(pm_weighted_col_start):!!as.symbol(pm_weighted_col_end) , names_to = "years",
                   values_to = "pop_weighted_avg_pm2.5") %>%
      mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
             region = "District Average") %>%
      select(years, region, pop_weighted_avg_pm2.5)

    trendlines_aqli_plt <- trendlines_aqli_data %>%
      ggplot() +
      geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5), lwd = 1.1,
                color = "darkred") +
      scale_x_continuous(breaks = seq(start_year, end_year, 2)) +
      # scale_y_continuous(breaks = seq(0, max(trendlines_aqli_data$pop_weighted_avg_pm2.5), 10)) +
      ggthemes::theme_clean() +
      labs(x = "Years",
           y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
      theme(legend.position = "bottom", legend.title = element_blank(),
            legend.text = element_text(size = 7),
            axis.title.y = element_text(size = 9),
            axis.title.x = element_text(size = 9))

  }

  return(trendlines_aqli_plt)

}
