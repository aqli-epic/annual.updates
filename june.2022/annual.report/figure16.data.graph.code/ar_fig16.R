# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

# AR figure 16 code--------------------------------------------------------------

#> Figure 16:  Potential Gain in Life Expectancy from Reducing PM2.5 to the WHO Guideline in Mainland China, 2013 vs. 2020

# filter master dataset for China
color_2020_china <- color_2020 %>%
  filter(country %in% "China")

# BTH region (name_1)
bth_region <- c("Beijing", "Tianjin", "Hebei")

# YRD (name_1)
yrd_region <- c("Shanghai", "Jiangsu", "Zhejiang")

# PRD (name_1, name_2)
prd_region_name_2_guandong_regions <- c("Dongguan", "Foshan", "Guangzhou",
                                        "Huizhou", "Jiangmen", "Shenzhen",
                                        "Zhaoqing", "Zhongshan", "Zhuhai")

prd_region_name_1 <- c("Hong Kong", "Macao")

# prd combined
prd <- c(prd_region_name_2_guandong_regions, prd_region_name_1)


# add region column in the China dataset
color_2020_china  <- color_2020_china %>%
  mutate(region = ifelse(name_1 %in% bth_region, "BTH", "others"),
         region = ifelse(name_1 %in% yrd_region,  "YRD", region),
         region = ifelse(name_1 %in% prd_region_name_1, "PRD", region),
         region = ifelse(name_2 %in% prd_region_name_2_guandong_regions, "PRD", region))

# Part-1: BTH, YRD, PRD gain in LE from 2013 to 2020
bth_yrd_prd_gain_le_part1 <- color_2020_china %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted  = pm2020*pop_weights,
         pm2013_pop_weighted = pm2013*pop_weights) %>%
  summarise(avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            avg_pm2.5_2013 = sum(pm2013_pop_weighted, na.rm = TRUE),
            diff_avg_pm2.5_2013_to_2020 = (avg_pm2.5_2013 - avg_pm2.5_2020),
            change_in_le_2013_to_2020 = round((diff_avg_pm2.5_2013_to_2020*0.098), 1)) %>%
  filter(region %in% c("BTH", "YRD", "PRD"))

# Part-2: China gain in LE from 2013 to 2020
china_gain_le_part2 <- color_2020_china %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted  = pm2020*pop_weights,
         pm2013_pop_weighted = pm2013*pop_weights) %>%
  summarise(avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            avg_pm2.5_2013 = sum(pm2013_pop_weighted, na.rm = TRUE),
            diff_avg_pm2.5_2013_to_2020 = (avg_pm2.5_2013 - avg_pm2.5_2020),
            change_in_le_2013_to_2020 = round((diff_avg_pm2.5_2013_to_2020*0.098), 1)) %>%
  mutate(region = "China") %>%
  select(region, everything())

#> Part-3: China gain in LE for specific regions as shown in AR figure 16

# regions of interest (note: there are 2 "Suzhou", I have taken the one that is in the Jiangsu gadm level 1 region, mention this in a footnote or as a note within the figure itself)

fig16_gadm2_regions <- c("Baoding", "Beijing", "Chengdu",
                         "Chongqing", "Guangzhou", "Harbin",
                         "Nanyang", "Shanghai", "Suzhou",
                         "Tianjin")

# filter color dataset for the above regions
custom_regions_china_le_part3 <- color_2020_china %>%
  filter(name_2 %in% fig16_gadm2_regions) %>%
  group_by(name_1, name_2) %>%
  summarise(pm2013 = pm2013[1], pm2020 = pm2020[1], diff_avg_pm2.5_2013_to_2020 = (pm2013 - pm2020),
            change_in_le_2013_to_2020 = round((diff_avg_pm2.5_2013_to_2020*0.098), 1)) %>%
  filter(name_1 != "Anhui") %>%
  ungroup() %>%
  select(name_2, pm2020, pm2013, diff_avg_pm2.5_2013_to_2020, change_in_le_2013_to_2020) %>%
  rename(region = name_2, avg_pm2.5_2020 = pm2020, avg_pm2.5_2013 = pm2013)

# correcting Suzhou's 2020 and 2013 value, to be the one that corresponds to the Anhui gadm level 1 region
custom_regions_china_le_part3[custom_regions_china_le_part3$region == "Suzhou", ]$avg_pm2.5_2020 <- 44.9
custom_regions_china_le_part3[custom_regions_china_le_part3$region == "Suzhou", ]$avg_pm2.5_2013 <- 63.6
custom_regions_china_le_part3[custom_regions_china_le_part3$region == "Suzhou", ]$diff_avg_pm2.5_2013_to_2020 <- 18.7
custom_regions_china_le_part3[custom_regions_china_le_part3$region == "Suzhou", ]$change_in_le_2013_to_2020 <- 1.8

# combine Parts 1, 2 and 3 datasets to create AR figure 16 dataset
ar_fig16_dataset <- rbind(bth_yrd_prd_gain_le_part1, china_gain_le_part2, custom_regions_china_le_part3)

# save AR figure 16 dataset
ar_fig16_dataset %>%
  write_csv("./june.2022/annual.report/figure16.data.graph.code")

# plot annual report figure 16
ar_fig16_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = fct_reorder(region, change_in_le_2013_to_2020), y = change_in_le_2013_to_2020, fill = change_in_le_2013_to_2020)) +
  scale_fill_gradient(low = "orange", high = "red") +
  coord_flip() +
  ggthemes::theme_hc() +
  theme(axis.line.x = element_line(),
        axis.line.y = element_line()) +
  scale_y_continuous(breaks = seq(0, 6, 1)) +
  labs(x = "Years gained between 2013 and 2020", y = "Region", fill = "Gain in Life Expectancy (in years)")
