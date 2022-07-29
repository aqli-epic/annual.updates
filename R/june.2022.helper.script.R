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
devtools::load_all()
devtools::document()

# read in latest color file
color_2020 <- read_csv("./master.dataset/color_2020.csv")

# read in old available color datasets
color_2019 <- read_csv("./master.dataset/color_2019.csv")
color_2016 <- read_csv("./master.dataset/color_2016.csv")

# GBD final results file (resulting file that we get after applying the steps in the GBD technical appendix)
gbd_results <- read_csv("./other.important.calculations.data/estimated_life_expectancy_differences_master_table_final.csv")

# global variables
who_guideline <- 5
le_constant <- 0.098
ncap_midpoint <- 25
nat_stan_india <- 40
nat_stan_sk <- 25
nat_stan_bangladesh <- 15

# global operations
`%notin%` <- Negate(`%in%`)
