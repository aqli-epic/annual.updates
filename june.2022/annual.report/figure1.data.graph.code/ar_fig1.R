# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

# AR figure 1 code--------------------------------------------------------------

#> Figure 1: Impact of the Revised WHO Guideline on Polluted Regions in the World

# dividing color dataset into 3 pollution groups: [0, 5], (5, 10], (10, inf)

ar_fig1_dataset <- color_2020 %>%
  mutate(region = ifelse(pm2020 >= 0 & pm2020 <= 5, "In Compliance with new WHO Guideline", pm2020),
         region = ifelse((pm2020 > 5 & pm2020 <= 10), "Newly out of compliance", region),
         region = ifelse((pm2020 > 10), "Already out of compliance", region))


# read in color map shape file
colormap_shp <- st_read("./june.2022/other.important.calculations.data/color.map.shape.file/colormap.shp")

# ar figure 1 dataset
ar_fig1_dataset <- ar_fig1_dataset %>%
  left_join(colormap_shp, by = c("country" = "NAME_0", "name_1" = "NAME_1", "name_2" = "NAME_2")) %>%
  select(country, name_1, name_2, pm2020, region, geometry)

# save annual report figure 1 dataset
ar_fig1_dataset %>%
  st_write("./june.2022/annual.report/figure1.data.graph/ar_fig1_dataset.shp")

# plot ar figure 1
ar_fig1 <- ar_fig1_dataset %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(mapping = aes(fill = region))

# image saved separately by taking a screenshot from high definiton monitor
