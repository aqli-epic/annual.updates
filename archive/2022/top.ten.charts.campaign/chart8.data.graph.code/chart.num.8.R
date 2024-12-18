# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> Chart number 8:  Comparison of Life Expectancy Lost Due to Particulate Pollution Not Meeting the WHO Guideline (Eastern v/s Western Europe)

# make a copy of the color dataset, so as to convert the columns in the new dataset into lower case, specifically for this chart
color_2020_chart8 <- color_2020


# convert character columns to lower case
color_2020_chart8$country <- str_to_lower(color_2020_chart8$country)
color_2020_chart8$name_1 <- str_to_lower(color_2020_chart8$name_1)
color_2020_chart8$name_2 <- str_to_lower(color_2020_chart8$name_2)


# europe countries
europe_countries <- read_csv("./other.important.calculations.data/europe_countries.csv")

# europe countries converted to lower case
europe_countries$Country <- str_to_lower(europe_countries$Country)

# replacing south and north macedonia with macedonia
europe_countries$Country <- str_replace(europe_countries$Country, "(north macedonia)|(south macedonia)", "macedonia")


# filtering color level dataset to only include the european countries.
color_2020_europe_custom <- color_2020_chart8 %>%
  filter(country %in% europe_countries$Country) %>%
  filter(country %notin% c("russia", "turkey", "sweden", "finland", "norway", "kazakhstan", "iceland", "georgia", "azerbaijan", "armenia", "cyprus", "northern cyprus"))

# Western European definition as per chart #8
western_european_countries <- c("Germany", "Switzerland", "Italy", "Monaco", "Luxembourg",
                                "Belgium", "France", "Netherlands", "Andorra", "Spain",
                                "United Kingdom", "Portugal", "Denmark", "Ireland", "Iceland", "Austria")

# converting western european countries names to lower case
western_european_countries <- str_to_lower(western_european_countries)

# creating a country level dataset, calculating average PM2.5
color_2020_europe_custom_country_level <- color_2020_europe_custom %>%
  group_by(country) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pm2020*pop_weights) %>%
  summarise(avg_pm2.5_2020 = round(sum(pm2020_pop_weighted, na.rm = TRUE), 1))


# read in country level shape file (please request this separately from aqli-info@uchicago.edu, this is not present in the Git repository, its a heavy file)
# Note: for all operations in chart #8 post this point, you would need the country shape file, so please request that file in case you'd
# like to proceed.

country_shp <- st_read("./june.2022/other.important.calculations.data/world.country.level.shape.file/color_country.shp")

# renaming shape file columns and selecting only relevant columns
country_shp <- country_shp %>%
  rename(country = NAME_0,
         name_1 = NAME_1,
         name_2 = NAME_2) %>%
  select(-c(iso_alpha3, objectid))

# converting shape file country, name_1, name_2 columns to lower case so that they matcht the color file lower case columns
country_shp$country <- str_to_lower(country_shp$country)
country_shp$name_1 <- str_to_lower(country_shp$name_1)
country_shp$name_2 <- str_to_lower(country_shp$name_2)

#
country_shp_europe_def <- country_shp %>%
  filter(country %in% color_2020_europe_custom_country_level$country)

# filter the shape file to only include those countries that are present in the custom europe color file.

color_2020_europe_custom_final <- color_2020_europe_custom_country_level %>%
  left_join(country_shp_europe_def, by = c("country")) %>%
  st_as_sf()

chart8_dataset <- color_2020_europe_custom_final %>%
  mutate(in_western_europe = ifelse(country %in% western_european_countries, "Western Europe", "Eastern Europe")) %>%
  select(country:name_2, in_western_europe, geometry)

# write chart8 shape file
st_write(chart8_dataset, "./june.2022/top.ten.charts.campaign/chart8.data.graph.code/chart8_dataset.shp")


# chart 8 plot (be careful with the column names in the fill aesthetic, st_read and st_write sometimes abbreviates them)
chart8 <- ggplot(data = chart8_dataset, mapping = aes(fill = in_western_europe)) +
  geom_sf(color= "black") +
  scale_fill_manual(values = c("darkred", "darkorange")) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("") +
  ggthemes::theme_map() +
  theme(legend.position = "none")

# Save chart number 8
ggsave("./june.2022/top.ten.charts.campaign/chart8.data.graph.code/chart8.png", chart8)

