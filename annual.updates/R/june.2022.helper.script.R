# Global variables, datasets and packages--------------------------------------

# metadata
# author: Aarsh Batra

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

# Paths are relative to the repository root (archive/2022 is the former june.2022 tree).
source(file.path(dirname(normalizePath(
  (function() {
    for (i in rev(seq_len(sys.nframe()))) {
      f <- sys.frame(i)$ofile
      if (!is.null(f)) return(f)
    }
    file.path(getwd(), "R/june.2022.helper.script.R")
  })()
)), "paths.R"))

color_2020 <- read_csv(aqli_input(
  "color_2020.csv",
  repo_rel = "archive/2022/master.dataset/color_2020.csv"
))
color_2019 <- read_csv(aqli_input(
  "color_2019.csv",
  repo_rel = "archive/2022/master.dataset/color_2019.csv"
))
color_2016 <- read_csv(aqli_input(
  "color_2016.csv",
  repo_rel = "archive/2022/master.dataset/color_2016.csv"
))
gbd_results <- read_csv(aqli_input(
  "estimated_life_expectancy_differences_master_table_final.csv",
  repo_rel = "archive/2022/other.important.calculations.data/estimated_life_expectancy_differences_master_table_final.csv"
))

# global variables
who_guideline <- 5
le_constant <- 0.098
ncap_midpoint <- 25
nat_stan_india <- 40
nat_stan_sk <- 25
nat_stan_bangladesh <- 15

# global operations
`%notin%` <- Negate(`%in%`)
