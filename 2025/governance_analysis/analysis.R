#### set up ####
# libraries
library(tidytext)
library(readxl)
library(tidyverse)
library(sf)
library(usethis)
library(devtools)
library(data.table)
# library(svglite)

# global variables
who_guideline <- 5
le_constant <- 0.098
latest_year <- 2022
first_year <- 1998

# global operations
`%notin%` <- Negate(`%in%`)

source("~/Desktop/AQLI/REPO/annual.updates/R/colour_scale.R")
source("~/Desktop/AQLI/REPO/annual.updates/R/aqli_regions.R")

#### data ####
# read in latest PM2.5 data file
gadm2_aqli_2022 <- read_csv("~/Desktop/AQLI/2024 AQLI Update/data/gadm2_2022_wide.csv")
gadm1_aqli_2022 <- read_csv("~/Desktop/AQLI/2024 AQLI Update/data/gadm1_2022_wide.csv")
gadm0_aqli_2022 <- read_csv("~/Desktop/AQLI/2024 AQLI Update/data/gadm0_2022_wide.csv")

# openAQ data on monitoring
public_data <- read_csv("~/Desktop/AQLI/2024 AQLI Update/website/open_data.csv")

# national standards year of adoption
nat_standard <- read_excel("~/Desktop/AQLI/nature communications/aq_standards.xlsx") %>%
  filter(!is.na(`Year first adopted`), `Year first adopted` < 2022) %>%
  select(Country, `Year first adopted`)

# US data
us_county <- read_csv("~/Desktop/AQLI/2024 AQLI Update/CleanAirAct/final/county_pm25_foraqli_stats.csv")

# gadm0 shapefile
gadm0_aqli_2022_shp <- st_read("~/Desktop/AQLI/shapefiles/global/gadm0/aqli_gadm0_final_june302023.shp")

#### Introduction stats ####
gadm0_aqli_2022 %>%
  filter(who2022 > 1.9) %>% # number of countries where PGLE > global average
  count()

gadm0_aqli_2022 %>%
  filter(who2022 > 1.9, is.na(natstandard)) %>% # countries where PGLE > global average and don't have natl standard
  view()

above_glob_avg_natstd <- gadm0_aqli_2022 %>%
  filter(who2022 > 1.9, !is.na(natstandard)) %>%
  select(id)

above_glob_avg_no_natstd <- gadm0_aqli_2022 %>%
  filter(who2022 > 1.9, is.na(natstandard)) %>%
  select(id)

gadm2_aqli_2022 %>%
  filter(!is.na(population), !is.na(pm2022)) %>%
  mutate(region = if_else(name0 %in% south_asia_def, "South Asia", NA),
         region = if_else(name0 == "China", "China", region),
         region = if_else(name0 %in% central_african_countries, "Central Africa", region),
         region = if_else(name0 %in% west_african_countries, "West Africa", region),
         region = if_else(name0 %in% mena_countries, "Middle East & North Africa", region),
         region = if_else(name0 %in% se_asia_vec, "South East Asia", region),
         region = if_else(name0 %in% unlist(european_countries) & name0 %notin% western_european_countries, "Eastern Europe", region),
         region = if_else(name0 %in% western_european_countries, "Western Europe", region),
         region = if_else(name0 %in% latin_america_countries_vec, "Latin America", region),
         region = if_else(name0 == "United States", "United States", region, missing = "Rest of the World")) %>%
  group_by(region) %>%
  summarise(avg_pm = weighted.mean(pm2022,population)) %>%
  mutate(lyl = 0.098*(avg_pm-5)) %>%
  arrange(desc(avg_pm))

gadm0_aqli_2022 %>%
  filter(name %in% c("Bangladesh", "India", "Democratic Republic of the Congo",
                     "Cameroon", "China", "United States", "Canada", "Japan")) %>%
  select(name, population, natstandard, pm2022, who2022) %>%
  arrange(desc(pm2022))

gadm0_aqli_2022 %>%
  filter(!is.na(natstandard), !is.na(population)) %>%
  summarise(tot_pop = sum(population/1000000))

#### LYL w.r.to WHO map ####
fig1a_data <- gadm0_aqli_2022 %>%
  mutate(who2022 = if_else(pm2022 <= whostandard, 0, who2022, missing = 0)) %>%
  # filter(!is.nan(who2022)) %>%
  left_join(gadm0_aqli_2022_shp, by = c("id" = "isoalp3")) %>%
  add_aqli_color_scale_buckets("lyl", "who2022") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

fig1a <- fig1a_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm0_aqli_2022_shp, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  geom_sf(data = gadm0_aqli_2022_shp %>%
            filter(isoalp3 %in% unlist(above_glob_avg_natstd)),
          color = "black", fill = NA, lwd = 0.3) +
  geom_sf(data = gadm0_aqli_2022_shp %>%
            filter(isoalp3 %in% unlist(above_glob_avg_no_natstd)),
          color = "black", fill = NA, lwd = 0.3 , linetype = "dashed") +
  ggthemes::theme_map()  +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF",
                               "0.1 to < 0.5" = "#FFF2E1",
                               "0.5 to < 1" = "#FFEDD3",
                               "1 to < 2" = "#FFC97A",
                               "2 to < 3" = "#FFA521",
                               "3 to < 4" = "#EB6C2A",
                               "4 to < 5" = "#D63333",
                               "5 to < 6" = "#8E2946",
                               ">= 6" = "#451F59")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years)  ", title = "",
       subtitle = "") +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"),
        legend.box.spacing = unit(0, "cm"),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))

fig1b_data <- gadm0_aqli_2022 %>%
  select(id, name, natstandard) %>%
  mutate(aq_std_bucket = if_else(natstandard >= 5 & natstandard <= 10, "5 - 10", NA),
         aq_std_bucket = if_else(natstandard > 10 & natstandard <= 20, "10 - 20", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 20 & natstandard <= 30, "20 - 30", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 30 & natstandard <= 40, "30 - 40", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 40 & natstandard <= 50, "40 - 50", aq_std_bucket, missing = "Does not have a standard")) %>%
  left_join(gadm0_aqli_2022_shp, by = c("id" = "isoalp3"))  %>%
  select(-geometry, geometry, ) %>%
  st_as_sf()

fig1b_data$aq_std_bucket <- factor(fig1b_data$aq_std_bucket,
                                   levels = c("5 - 10", "10 - 20", "20 - 30",
                                              "30 - 40", "40 - 50", "Does not have a standard"))


fig1b <- ggplot() +
  geom_sf(data = gadm0_aqli_2022_shp, color = "cornsilk4", fill = "white", lwd = 0.05) +
  geom_sf(data = fig1b_data, mapping = aes(fill = aq_std_bucket), color = "cornsilk4", lwd = 0.05, alpha = 0.7) +
  ggthemes::theme_map() +
  labs(fill="Ambient annual PM2.5 standard (in µg/m³)") +
  scale_fill_manual(values = c("5 - 10" = "#0078a3",
                               "10 - 20" = "#599ba8",
                               "20 - 30" = "#b08a4e",
                               "30 - 40" = "#cf570e",
                               "40 - 50" = "#b90d1f",
                               "Does not have a standard" = "#111111")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1))

#### Change in PM since standard adoption ####
pm_range_df_long <- gadm0_aqli_2022 %>%
  select(name, natstandard, starts_with("pm")) %>%
  inner_join(nat_standard, by = c("name" = "Country")) %>%
  pivot_longer(cols = pm1998:pm2022,
               names_to = "years",
               values_to = "pm") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  rename("country" = "name", "year_first_adopted" = "Year first adopted")

pm_range_df <- pm_range_df_long  %>%
  filter(years == 2022 | years == year_first_adopted) %>%
  group_by(country) %>%
  mutate(range_pm = pm - lag(pm),
         pm_nat_std = pm - range_pm) %>%
  filter(!is.na(range_pm)) # note: pm - range = pm_in_year_of_nat_std

fig2 <- pm_range_df  %>%
  ggplot(aes(x = reorder(country, range_pm) , y = range_pm)) +
  geom_hline(yintercept = 0, linewidth = 1) +
  geom_point(aes(y = 0, color = "PM2.5 in the year national \nstandard was adopted"), size = 2) +
  geom_point(aes(y = range_pm, color = "PM2.5 in 2022"), size = 2) +
  geom_segment(aes(xend = country, y = 0, yend = range_pm),
               linetype = "dashed", color = "black") +
  scale_color_manual(name = "", values = c("PM2.5 in the year national \nstandard was adopted" = "darkgrey",
                                           "PM2.5 in 2022" = "darkred"),
                     breaks = c("PM2.5 in 2022", "PM2.5 in the year national \nstandard was adopted"),
                     labels = c("PM2.5 in 2022", "PM2.5 in the year national \nstandard was adopted")) +
  labs(x = "", y = expression("Change in" ~ PM[2.5] ~ "(in µg/m³)"),
    caption = "Note: The data on year of adoption of PM2.5 standards has been compiled to the best of our ability. Only countries that adopted their PM2.5 standards before 2022 are included in this plot. We are unable to include Sudan, \nBahrain, Iraq, Barbados, and Jamaica in this plot due to unavailability of the year in which they adopted their standard.") +
  scale_y_continuous(breaks = seq(-15, 5, 1), limits = c(-15, 5)) +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(color = "black"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.y = element_line(colour = "lightgrey", linetype = "solid", linewidth = 0.25),
        plot.title = element_text(hjust = 0.5), # element_text(hjust = 0.5, size = 10, margin = margin(b = 0.3, unit = "cm")),
        plot.caption = element_text(hjust = 0, size = 7), # element_text(hjust = 0.5, size = 9, face = "italic", margin = margin(t = 1.3, unit = "cm")),
        plot.background = element_rect(fill = "white", color = "white") #,
        )

##### Population by national standard buckets #####
public_data <- gadm0_aqli_2022 %>%
  select(name, population, natstandard, pm2022) %>%
  left_join(public_data , by = c("name" = "Country or Dependency"))

table1a <- public_data %>%
  filter(!is.nan(population)) %>%
  select(name, natstandard, population, pm2022) %>%
  mutate(pm_bucket = if_else(pm2022 <= 5 , "up to 5", NA),
         pm_bucket = if_else(pm2022 > 5 & pm2022 <= 10, "5 - 10", pm_bucket),
         pm_bucket = if_else(pm2022 > 10 & pm2022 <= 20, "10 - 20", pm_bucket),
         pm_bucket = if_else(pm2022 > 20 & pm2022 <= 30, "20 - 30", pm_bucket),
         pm_bucket = if_else(pm2022 > 30 & pm2022 <= 40, "30 - 40", pm_bucket),
         pm_bucket = if_else(pm2022 > 40 & pm2022 <= 50, "40 - 50", pm_bucket),
         pm_bucket = if_else(pm2022 > 50 , "more than 50", pm_bucket,  missing = "No data")) %>%
  group_by(pm_bucket) %>%
  summarise(num_countries = n(),
            pm_bucket_pop = round(sum(population/1000000, na.rm = TRUE), 0)) %>%
  mutate(percent_pop = round(100*pm_bucket_pop/sum(pm_bucket_pop, na.rm = TRUE), 1)) %>%
  arrange(factor(pm_bucket,
                 levels = c("up to 5", "5 - 10", "10 - 20", "20 - 30",
                            "30 - 40", "40 - 50", "more than 50")))

table1b <- public_data %>%
  select(name, natstandard, population, pm2022) %>%
  mutate(aq_std_bucket = if_else(natstandard >= 5 & natstandard <= 10, "5 - 10", NA),
         aq_std_bucket = if_else(natstandard > 10 & natstandard <= 20, "10 - 20", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 20 & natstandard <= 30, "20 - 30", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 30 & natstandard <= 40, "30 - 40", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 40 & natstandard <= 50, "40 - 50", aq_std_bucket, missing = "Does not have a standard")) %>%
  group_by(aq_std_bucket) %>%
  summarise(num_countries = n(),
            aq_bucket_pop = round(sum(population[pm2022<=natstandard]/1000000, na.rm = TRUE), 0),
            tot_pop = round(sum(population/1000000, na.rm = TRUE), 0)) %>%
  mutate(percent_pop = round(100*aq_bucket_pop/tot_pop, 1)) %>%
  arrange(factor(aq_std_bucket,
                 levels = c("5 - 10", "10 - 20", "20 - 30",
                            "30 - 40", "40 - 50", "Does not have a standard")))

##### AQ monitoring #####
public_data %>%
  filter(!is.na(`Is there any evidence of government operated AQ monitoring system in 2022?`)) %>%
  mutate(`Publicly accessible?` = if_else(`Publicly accessible?` == "Yes*", "Yes", `Publicly accessible?`)) %>%
  group_by(`Publicly accessible?`) %>%
  count()

table2 <- public_data %>%
  filter(!is.na(`Is there any evidence of current government operated AQ monitoring system in 2024?`)) %>%
  group_by(`Is there any evidence of current government operated AQ monitoring system in 2024?`) %>%
  summarise(num_countries = n(),
            pop = sum(population, na.rm = TRUE),
            avg_pm = round(weighted.mean(pm2022, population), 1)) %>%
  mutate(percent_pop = round(100*pop/sum(pop, na.rm = TRUE), 1))%>%
  rename("government_aq_monitoring" = "Is there any evidence of current government operated AQ monitoring system in 2024?") %>%
  select(government_aq_monitoring, num_countries, percent_pop, avg_pm) %>%
  arrange(factor(government_aq_monitoring,
                 levels = c("Yes", "No")))

#### Regional points ####
# Europe and Central Asia
europe_pm2022 <- gadm0_aqli_2022 %>%
  select(name, natstandard, pm2022, population) %>%
  filter(name %in% unlist(european_countries), !is.nan(population)) %>%
  mutate(has_std = if_else(!is.na(natstandard), "Adopted annual PM2.5 standard", "Did not adopt a standard or \ndoes not have a standard")) %>%
  group_by(has_std) %>%
  summarise(pm2022_wtd = weighted.mean(pm2022, population))

japan <- gadm0_aqli_2022 %>%
  filter(name == "Japan") %>%
  select(name, starts_with("pm")) %>%
  pivot_longer(cols = pm1998:pm2022, names_to = "years",
               values_to = "pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))))

japan %>%
  ggplot() +
  geom_line(aes(x = years, y = `pm2.5`), linewidth = 1.5) +
  ylim(0,50) +
  labs(title = "Japan")

china <- gadm0_aqli_2022 %>%
  filter(name == "China") %>%
  select(name, starts_with("pm")) %>%
  pivot_longer(cols = pm1998:pm2022, names_to = "years",
               values_to = "pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))))

china %>%
  ggplot() +
  geom_line(aes(x = years, y = `pm2.5`), linewidth = 1.5) +
  ylim(0,50) +
  labs(title = "China")

usa <- us_county %>%
  select(country, pm25_1970_aqli, population) %>%
  group_by(country) %>%
  summarise(pm1970 = round(weighted.mean(pm25_1970_aqli, population, na.rm = TRUE), 1)) %>%
  right_join(gadm0_aqli_2022 %>%
               filter(name == "United States") %>%
               select(name, starts_with("pm")), by = c("country" = "name")) %>%
  pivot_longer(cols = pm1970:pm2022, names_to = "years",
               values_to = "pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))))

usa %>%
  ggplot() +
  geom_line(aes(x = years, y = `pm2.5`), linewidth = 1.5) +
  ylim(0,50) +
  labs(title = "USA")

#### Conclusion stats ####
gadm0_aqli_2022 %>%
  summarise(no_std_pop = sum(population[is.na(natstandard)], na.rm = TRUE),
            dsnt_meet_std_pop = sum(population[pm2022>natstandard], na.rm = TRUE),
            tot_pop = sum(population, na.rm = TRUE))

gadm0_aqli_2022 %>%
  filter(is.na(natstandard)) %>%
  count()

gadm0_aqli_2022 %>%
  filter(pm2022>natstandard) %>%
  count()

public_data %>%
  filter(is.na(natstandard)) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE))

public_data %>%
  filter(`Publicly accessible?` == "No") %>%
  summarise(tot_pop = sum(population, na.rm = TRUE))


