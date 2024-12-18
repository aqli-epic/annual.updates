# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


#> figure 6.3==================================================

# filtering the color file for China
color_2021_china <- gadm2_aqli_2021 %>%
  filter(country == "China", !is.na(population))

# BTH region (name_1)
bth_region <- c("Beijing", "Tianjin", "Hebei")

# YRD (name_1)
yrd_region <- c("Shanghai", "Jiangsu", "Zhejiang")

# PRD (name_1, name_2)

prd_region_name_2_guandong_regions <- c("Dongguan", "Foshan", "Guangzhou", 
                                        "Huizhou", "Jiangmen", "Shenzhen", 
                                        "Zhaoqing", "Zhongshan", "Zhuhai")

prd_region_name_1 <- c("Hong Kong", "Macau")

prd <- c(prd_region_name_2_guandong_regions, prd_region_name_1)


# add region column
color_2021_china  <- color_2021_china %>%
  mutate(region = ifelse(name_1 %in% bth_region, "BTH", "others"), 
         region = ifelse(name_1 %in% yrd_region,  "YRD", region), 
         region = ifelse(name_1 %in% prd_region_name_1, "PRD", region), 
         region = ifelse(name_2 %in% prd_region_name_2_guandong_regions, "PRD", region)) 



# Part-1: BTH, YRD, PRD gain in LE from 2014 to 2021
bth_yrd_prd_gain_le_part1 <- color_2021_china %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         pm2021_pop_weighted  = pm2021*pop_weights, 
         pm2014_pop_weighted = pm2014*pop_weights) %>% 
summarise(avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE), 
          avg_pm2.5_2014 = sum(pm2014_pop_weighted, na.rm = TRUE), 
          diff_avg_pm2.5_2014_to_2021 = (avg_pm2.5_2014 - avg_pm2.5_2021), 
          change_in_le_2014_to_2021 = round((diff_avg_pm2.5_2014_to_2021*0.098), 1)) %>%
  filter(region %in% c("BTH", "YRD", "PRD"))

# Part-2: China gain in LE from 2014 to 2021
china_gain_le_part2 <- color_2021_china %>%
   mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         pm2021_pop_weighted  = pm2021*pop_weights, 
         pm2014_pop_weighted = pm2014*pop_weights) %>% 
summarise(avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE), 
          avg_pm2.5_2014 = sum(pm2014_pop_weighted, na.rm = TRUE), 
          diff_avg_pm2.5_2014_to_2021 = (avg_pm2.5_2014 - avg_pm2.5_2021), 
          change_in_le_2014_to_2021 = round((diff_avg_pm2.5_2014_to_2021*0.098), 1)) %>%
  mutate(region = "China") %>%
  select(region, everything())

#> Part-3: China gain in LE for specific regions as shown in AR figure 16

# regions of interest (note: there are 2 "Suzhou", I have taken the one that is in the Jiangsu gadm level 1 region, mention this in a footnote or as a note within the figure itself)

gadm2_regions_to_plt <- c("Baoding", "Beijing", "Chengdu", 
                         "Chongqing", "Guangzhou", "Harbin", 
                         "Nanyang", "Shanghai", "Suzhou", 
                         "Tianjin")

# filter color dataset for the above regions
custom_regions_china_le_part3 <- color_2021_china %>%
  filter(name_2 %in% gadm2_regions_to_plt) %>%
  group_by(name_1, name_2) %>%
  summarise(pm2014 = pm2014[1], pm2021 = pm2021[1], diff_avg_pm2.5_2014_to_2021 = (pm2014 - pm2021), 
          change_in_le_2014_to_2021 = round((diff_avg_pm2.5_2014_to_2021*0.098), 1)) %>%
  filter(name_1 != "Jiangsu") %>%
  ungroup() %>%
  select(name_2, pm2021, pm2014, diff_avg_pm2.5_2014_to_2021, change_in_le_2014_to_2021) %>%
  rename(region = name_2, avg_pm2.5_2021 = pm2021, avg_pm2.5_2014 = pm2014) 

# # correcting Suzhou's 2020 and 2013 value, to be the one that corresponds to the Anhui gadm level 1 region
# custom_regions_china_le_part3[custom_regions_china_le_part3$region == "Suzhou", ]$avg_pm2.5_2020 <- 44.9
# custom_regions_china_le_part3[custom_regions_china_le_part3$region == "Suzhou", ]$avg_pm2.5_2013 <- 63.6
# custom_regions_china_le_part3[custom_regions_china_le_part3$region == "Suzhou", ]$diff_avg_pm2.5_2013_to_2020 <- 18.7
# custom_regions_china_le_part3[custom_regions_china_le_part3$region == "Suzhou", ]$change_in_le_2013_to_2020 <- 1.8

# combine Parts 1, 2 and 3 datasets to create AR figure 16 dataset
ar_fig6.3_data <- rbind(bth_yrd_prd_gain_le_part1, china_gain_le_part2, custom_regions_china_le_part3)


# plot annual report figure 16
ar_fig6.3 <- ar_fig6.3_data %>%
  add_aqli_color_scale_buckets(scale_type = "lyldiff", col_name = "change_in_le_2014_to_2021") %>%
  mutate(order_manual = ifelse(region == "Baoding", 14, NA), 
         order_manual = ifelse(region == "Beijing", 13, order_manual), 
         order_manual = ifelse(region == "Tianjin", 12, order_manual), 
         order_manual = ifelse(region == "Suzhou", 11, order_manual), 
         order_manual = ifelse(region == "Harbin", 10, order_manual), 
         order_manual = ifelse(region == "Nanyang", 9, order_manual), 
         order_manual = ifelse(region == "Shanghai", 8, order_manual), 
         order_manual = ifelse(region == "Chongqing", 7, order_manual),
         order_manual = ifelse(region == "Chengdu", 6, order_manual),
         order_manual = ifelse(region == "Guangzhou", 5, order_manual),
         order_manual = ifelse(region == "BTH", 4, order_manual),
         order_manual = ifelse(region == "China", 3, order_manual), 
         order_manual = ifelse(region == "PRD", 2, order_manual), 
         order_manual = ifelse(region == "YRD", 3, order_manual)) %>%
  ggplot() +
  geom_col(mapping = aes(x = fct_reorder(region, order_manual), y = change_in_le_2014_to_2021, fill = forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket))) +
 scale_fill_manual(values = c("< -2" = "#4575b4", 
                               "-2 to (< -0.5)" = "#74add1", 
                               "-0.5 to (< -0.1)" = "#abd9e9", 
                               "-0.1 to (< 0)" = "#e0f3f8", 
                               "0 to (< 0.1)" = "#fee090", 
                               "0.1 to (< 0.5)" = "#fdae61", 
                               "0.5 to (< 2)" = "#f46d43", 
                               ">= 2" = "#d73027")) +
  coord_flip() +
  ggthemes::theme_clean() +
    themes_aqli_base +
  theme(axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        plot.background = element_rect(color = "white", fill = "white")) +
  scale_y_continuous(breaks = seq(0, 6, 1)) +
  labs(x = "Region", y = "Life years gained", fill = "Life years gained") 


