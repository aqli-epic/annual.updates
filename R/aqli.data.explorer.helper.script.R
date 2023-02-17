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
