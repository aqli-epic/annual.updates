# load libraries
library(tidyverse)
library(readxl)
library(ggpubr)
library(sf)
library(igisci)
library(geodata)
library(gridExtra)

# operators
`%notin%` <- Negate(`%in%`)

# read data
aqli_2022 <- read_csv("~/Desktop/AQLI/2024 AQLI Update/data/aqli_gadm2_2022.csv")

# India data. Incomplete because of China conflict territories
# Pakistan conflict region
azad_kashmir_gilgit_baltistan <- aqli_2022 %>%
  filter(country == "Pakistan", name_1 %in% c("Azad Kashmir", "Gilgit Baltistan"))

india_aqli <- aqli_2022 %>%
  filter(country == "India") %>%
  # bind_rows(azad_kashmir_gilgit_baltistan) %>%
  select(objectid_gadm2, name_1, name_2, population, pm2022:pm1998) %>%
  mutate(diff_ncap = pm2022-pm2017,
         pct_decline = 100*(abs(diff_ncap)/pm2017))

# NCAP data
ncap_dist <- read_csv("data/ncap_cities_district_state_long.csv") %>%
  mutate(state_ut = ifelse(state_ut == "Jammu & Kashmir", "Jammu and Kashmir", state_ut),
         state_ut = ifelse(district == "Chandigarh", "Chandigarh", state_ut)) %>%
  left_join(aqli_2022, by = c("state_ut" = "name_1", "district" = "name_2")) %>%
  select(objectid_gadm2, state_ut, district, population, pm2022:pm2017) %>%
  mutate(diff_ncap = pm2022-pm2017,
         pct_decline = 100*(abs(diff_ncap)/pm2017),
         tot_lyl = population*0.098*abs(diff_ncap))

######### Aggregate estimates #########
# list of unique non attainment districts
non_attainmnent_cities <- ncap_dist %>%
  distinct(district) %>%
  pull()

# summary table comparing non attainment cities with others
summary <- india_aqli %>%
  mutate(category = ifelse(name_2 %in% non_attainmnent_cities, "Non-attainment", "Not non-attainment")) %>%
  group_by(category) %>%
  mutate(tot_pop = sum(population, na.rm = TRUE),
         pop_weights = population/tot_pop, 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum))

# rate of change
rate_of_change <- india_aqli %>%
  group_by(name_1, name_2) %>%
  select(pm2017:pm2022) %>%
  pivot_longer(cols = pm2017:pm2022, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         category = ifelse(name_2 %in% non_attainmnent_cities, "Non-attainment", "Not non-attainment"),
         full_name = paste(name_2, name_1, sep = ', '),
         pct_change = (pop_weighted_avg_pm2.5/lag(pop_weighted_avg_pm2.5) - 1)*100) %>%
  group_by(category) %>%
  mutate(avg_pct_decline = mean(pct_change, na.rm = TRUE))

rate_of_change %>% group_by(category) %>% distinct(avg_pct_decline)

# ten districts with the greatest absolute decline by category
india_aqli %>%
  mutate(category = ifelse(name_2 %in% non_attainmnent_cities, "Non-attainment", "Not non-attainment"),
         full_name = paste(name_2, name_1, sep = ', ')) %>%
  group_by(category) %>%
  slice_min(diff_ncap, n=10) %>% # slice min used here because pm2022-pm2017 is negative 
  select(full_name)

# ten districts with the greatest % decline by category
india_aqli %>%
  mutate(category = ifelse(name_2 %in% non_attainmnent_cities, "Non-attainment", "Not non-attainment"),
         full_name = paste(name_2, name_1, sep = ', ')) %>%
  group_by(category) %>%
  slice_max(pct_decline, n=10) %>%
  select(full_name, pct_change, pm2017)

######### Trends in non attainment cities #########
# spread of ACAG data in non attainment cities
ncap_dist_trendlines_data <- ncap_dist %>%
  group_by(state_ut, district) %>%
  select(starts_with("pm")) %>%
  pivot_longer(cols = pm2017:pm2022, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         full_name = paste(district, state_ut, sep = ', ')) 

# scatter plot with best fit curve
trend <- ggplot(data = ncap_dist_trendlines_data) +
  geom_point(mapping = aes(x = years, y = pop_weighted_avg_pm2.5), 
             alpha = 0.3)  + # color = "#7197be", 
  geom_smooth(aes(x = years, y = pop_weighted_avg_pm2.5)) +
  theme_bw()  +
  labs(title = "Non-attainment cities: Spread of \nACAG data and it's best fit curve \nfrom 2017-2022",
       x = "Year",
       y = "Annual average PM2.5")

# change in total life years lost due to decline in PM2.5 in 2022 
bubble_plot_data <- ncap_dist %>%
  mutate(year1 = 2017,
         year2 = 2022) %>%
  mutate(tot_lyl_cat = ifelse(population <= 1000000, "< 10 lakh", NA),
         tot_lyl_cat = ifelse(population > 1000000 & population <= 2000000, "10 to 20 lakh", tot_lyl_cat), 
         tot_lyl_cat = ifelse(population > 2000000 & population <= 3000000, "20 to 30 lakh", tot_lyl_cat), 
         tot_lyl_cat = ifelse(population > 3000000 & population <= 4000000, "30 to 40 lakh", tot_lyl_cat), 
         tot_lyl_cat = ifelse(population > 4000000 & population <= 5000000, "40 to 50 lakh", tot_lyl_cat), 
         tot_lyl_cat = ifelse(population > 5000000 & population <= 10000000, "50 lakh to 1 cr", tot_lyl_cat), 
         tot_lyl_cat = ifelse(population > 10000000, "> 1 cr", tot_lyl_cat))%>%
  mutate(order_tot_lyl_category = ifelse(tot_lyl_cat == "< 10 lakh", 1, NA), 
         order_tot_lyl_category = ifelse(tot_lyl_cat == "10 to 20 lakh", 2, order_tot_lyl_category),
         order_tot_lyl_category = ifelse(tot_lyl_cat == "20 to 30 lakh", 3, order_tot_lyl_category), 
         order_tot_lyl_category = ifelse(tot_lyl_cat == "30 to 40 lakh", 4, order_tot_lyl_category), 
         order_tot_lyl_category = ifelse(tot_lyl_cat == "40 to 50 lakh", 5, order_tot_lyl_category), 
         order_tot_lyl_category = ifelse(tot_lyl_cat == "50 lakh to 1 cr", 6, order_tot_lyl_category), 
         order_tot_lyl_category = ifelse(tot_lyl_cat == "> 1 cr", 7, order_tot_lyl_category))

total_lyl <- ggplot(data = bubble_plot_data) +
  geom_point(mapping = aes(x = year1, y = pm2017), 
             alpha = 0.5, size = 3) +
  geom_point(mapping = aes(x = year2, y = pm2022, 
                           colour = reorder(tot_lyl_cat, order_tot_lyl_category)), 
             alpha = 0.5, size = 3) +
  theme_bw()  +
  labs(title = "Non-attainment cities: Change in \ntotal life years lost due to decline \nin PM2.5 (µg/m3) from 2017 to 2022",
       x = "Year",
       y = "Annual average PM2.5") +
  scale_color_manual(name = "Gain in total life years",
                     values = c("< 10 lakh" = "#B2DF8A",
                                "10 to 20 lakh" = "#66C2A5",
                                "20 to 30 lakh" = "#33A02C",
                                "30 to 40 lakh" = "#3288BD",
                                "40 to 50 lakh" = "#1F78B4",
                                "50 lakh to 1 cr" = "#5E4FA2",
                                "> 1 cr" = "#9E0142")) +
  scale_x_discrete(limits = c(2017, 2022)) +
  theme(panel.spacing.x = unit(0, "lines"))

##### CPCB data #####
cpcb_data_wide_annual <- read_excel("data/CPCB_consistent_monitors_annual.xlsx")

cpcb_annual <- cpcb_data_wide_annual %>%
  mutate_at(-c(1), as.numeric) %>%
  pivot_longer(cols = -c(1), 
               names_to = c("monitor_location", "city", "pollution_board"), 
               names_sep = ", ", 
               values_to = "pollutant_conc") %>%
  mutate(district = city,
         district = ifelse(city == "Ahmedabad", "Ahmadabad", district),
         district = ifelse(city == "Bengaluru", "Bangalore", district),
         district = ifelse(city == "Delhi", "NCT of Delhi", district),
         district = ifelse(city == "Durgapur", "Barddhaman", district),
         district = ifelse(city == "Kanpur", "Kanpur Nagar", district),
         district = ifelse(city == "Mumbai", "Mumbai City", district),
         district = ifelse(city == "Navi Mumbai", "Raigad", district),
         district = ifelse(city == "Tirupati", "Chittoor", district),
         source = "CPCB") %>%
  group_by(Year, district) %>%
  mutate(annual_avg = mean(pollutant_conc, na.rm = TRUE)) %>%
  distinct(Year, city, district, annual_avg, source)

# India trendlines data
# Note: Aurangabad, Maharashtra is in the list of consistent monitors. Hence, 
#       renaming Aurangabad, Bihar to avoid data duplictaion in plot.
all_dist_trendlines_data <- india_aqli %>%
  group_by(name_1, name_2) %>%
  select(pm2017:pm2022) %>%
  pivot_longer(cols = pm2017:pm2022, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         full_name = paste(name_2, name_1, sep = ', '),
         name_2 = ifelse(name_2 == "Aurangabad" & name_1 == "Bihar", "Aurangabad2", name_2)) 

# loop to plot overlaid trends. done this way to visualise the smallest of
# changes in district trends
plots_list <- list()
# loop over the district values
for (district_val in unique(cpcb_annual$district)) {
  # filter data for the current district
  cpcb_district_data <- cpcb_annual %>% filter(district == district_val)
  ncap_district_data <- all_dist_trendlines_data %>% filter(name_2 == district_val) # to match with all districts trendlines data
  
  # create a plot for the current district
  plot <- ggplot() +
    geom_line(data = cpcb_district_data,
              aes(x=Year, y=annual_avg, colour = "annual_avg"),
              lwd = 0.5) +
    geom_line(data = ncap_district_data,
              aes(x=years, y=pop_weighted_avg_pm2.5, colour = "pop_weighted_avg_pm2.5"),
              lwd = 0.5) +
    labs(title = district_val,
         x = "Years",
         y = "PM2.5",
         colour = " ") +
    scale_x_continuous(breaks = c(seq(2017, 2023, 1))) +
    scale_color_manual(values = c(annual_avg = "blue", pop_weighted_avg_pm2.5 = "red"),
                       labels = c(annual_avg = "CPCB Data", pop_weighted_avg_pm2.5 = "ACAG Data"),
                       limits = c("annual_avg", "pop_weighted_avg_pm2.5")) +
    theme_bw() 
  
  # add the plot to the list
  plots_list[[district_val]] <- plot
}

# convert the list of plots to a list of grobs (graphical objects)
plot_grobs <- lapply(plots_list, ggplotGrob)
# arrange the plots in a grid
grid.arrange(grobs = plot_grobs, ncol = 4)

##### US consulates data ##### 
# read and append data 
lapply_read_csv_bind_rows <- function(path, pattern = "\\.csv$") {
  files = list.files(path, pattern, full.names = TRUE)
  lapply(files, read_csv) %>% bind_rows()
}

us_consul_data <- lapply_read_csv_bind_rows("data/US consulates India")

# check for more than 50% data availability and filter valid observations
us_consul_fun <- function(data){
  data %>%
    filter(`QC Name` == "Valid") %>%
    group_by(Site, Year, Month) %>%
    mutate(days = n(),
           tot_days = case_when(Month %in% c("01","03","05","07","08","10","12") ~ 744,
                                Month %in% c("04","06","09","11") ~ 720,
                                Month == "02" & Year == 2020 ~ 696,
                                Month == "02" ~ 672),
           pct_data_avl = 100*(days/tot_days),
           district = Site,
           district = ifelse(Site == "New Delhi", "NCT of Delhi", district),
           district = ifelse(Site == "Mumbai", "Mumbai City", district)) %>%
    filter(pct_data_avl >= 50)
}

# annual average
us_consul_annual <- us_consul_fun(us_consul_data) %>%
  ungroup() %>%
  group_by(Site, Year) %>%
  mutate(annual_avg = mean(`Raw Conc.`)) %>%
  distinct(Year, Site, district, annual_avg) %>%
  mutate(source = "US Consulate")

# combine ACAG, CPCB and US Consulate data to compare annual trends
data_comparison <- ncap_dist_trendlines_data %>%
  ungroup() %>%
  select(years, district, pop_weighted_avg_pm2.5) %>%
  mutate(source = "ACAG") %>%
  rename(Year = years,
         annual_avg = pop_weighted_avg_pm2.5) %>%
  add_row(cpcb_annual %>% ungroup() %>% select(Year, district, annual_avg, source)) %>%
  add_row(us_consul_annual %>% ungroup() %>% select(Year, district, annual_avg, source)) %>%
  filter(district %in% unique(us_consul_annual$district))

# plot with US consulate
us_consul <- data_comparison %>%
  mutate(annual_avg = ifelse(annual_avg == 0, NaN, annual_avg)) %>%
  ggplot(aes(x=Year, y=annual_avg, colour=source)) +
  geom_line() +
  labs(title = "Comparison of ACAG, CPCB and US Consulate annual average PM2.5 values",
       x = "Year",
       y = "Annual average PM2.5") +
  scale_x_continuous(breaks = c(seq(2017, 2023, 1))) +
  scale_y_continuous(breaks = c(seq(0, 130, 10))) +
  theme_bw() +
  facet_wrap(~district, nrow = 1)

######### Change in LYL #########
# read shapefiles
gadm2_aqli_2021_shp <- st_read("~/Desktop/AQLI/2023 AQLI Update/gadm_shapefiles/gadm2/aqli_gadm2_final_june2023.shp")
gadm1_aqli_2021_shp <- st_read("~/Desktop/AQLI/2023 AQLI Update/gadm_shapefiles/gadm1/aqli_gadm1_final_june2023.shp")
gadm0_aqli_2021_shp <- st_read("~/Desktop/AQLI/2023 AQLI Update/gadm_shapefiles/gadm0/aqli_gadm0_final_june2023.shp")

# India state shapefile. Incomplete because of China conflict territories
ind_state <- gadm1_aqli_2021_shp  %>% filter(name0 == "India")
akgb_state <-  gadm1_aqli_2021_shp  %>% filter(name0 == "Pakistan", name1 %in% c("Azad Kashmir", "Gilgit Baltistan"))
ind_state <- ind_state %>% bind_rows(akgb_state)

# functions
# generate map data
map_data <- function(data, column1, column2){
  data %>%
    select(objectid_gadm2, {{column1}}, {{column2}}, diff_ncap) %>%
    left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
    mutate(lyl_cat = ifelse(diff_ncap >= 0, "0 to 6 months", diff_ncap), 
           lyl_cat = ifelse(diff_ncap < 0 & diff_ncap >= -5, "> 0 - 6 months", lyl_cat),
           lyl_cat = ifelse(diff_ncap < -5 & diff_ncap >= -10, "> 6 months - 1 year", lyl_cat), 
           lyl_cat = ifelse(diff_ncap < -10 & diff_ncap >= -15, "> 1 - 1.5 years", lyl_cat), 
           lyl_cat = ifelse(diff_ncap < -15 & diff_ncap >= -20, "> 1.5 - 2 years", lyl_cat), 
           lyl_cat = ifelse(diff_ncap < -20, "> 2 years", lyl_cat)) %>%
    mutate(order_lyl_category = ifelse(lyl_cat == "0 to 6 months", 1, 0), 
           order_lyl_category = ifelse(lyl_cat == "> 0 - 6 months", 2, order_lyl_category), 
           order_lyl_category = ifelse(lyl_cat == "> 6 months - 1 year", 3, order_lyl_category),
           order_lyl_category = ifelse(lyl_cat == "> 1 - 1.5 years", 4, order_lyl_category), 
           order_lyl_category = ifelse(lyl_cat == "> 1.5 - 2 years", 5, order_lyl_category), 
           order_lyl_category = ifelse(lyl_cat == "> 2 years", 6, order_lyl_category)) %>%
    select(-geometry, geometry) %>%
    st_as_sf()
}

# plot map with proper formatting
map_plot <- function(data){
  ggplot(data) +
    geom_sf(mapping = aes(fill = reorder(lyl_cat, order_lyl_category)), color = "lightgrey", lwd = 0.05) +
    geom_sf(data = ind_state, color = "black", fill = "transparent", lwd = 0.5) +
    ggthemes::theme_map() + 
    labs(fill = expression("Change in life expectancy from 2019 to 2022"), title= {{title}}) +
    scale_fill_manual(values = c("0 to 6 months" = "#FFCC66", 
                                 "> 0 - 6 months" = "powderblue", 
                                 "> 6 months - 1 year" = "lightskyblue", 
                                 "> 1 - 1.5 years" = "cornflowerblue",
                                 "> 1.5 - 2 years" = "lightslateblue", 
                                 "> 2 years" = "slateblue4")) +
    theme(plot.background = element_rect(colour = "white", fill = "white"), 
          legend.position = "bottom", 
          legend.justification = "center", 
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 16),
          legend.box.background = element_rect(color = "black"))
}

# India map data
india_map_data <- map_data(india_aqli, name_1, name_2)
# map
india_map <- map_plot(india_map_data) 

# NCAP cities shapefile
ncap_map <- map_data(ncap_dist, state_ut, district) 
# map
ncap_cities_map <- map_plot(ncap_map) 



