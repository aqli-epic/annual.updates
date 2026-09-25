# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> China factsheet figure 2: Top 10 gadm level 1 most populous cities LE gains graph----------

# filtering the color file for China
color_2020_china <- color_2020 %>%
  filter(country == "China")

# BTH region (name_1)
bth_region <- c("Beijing", "Tianjin", "Hebei")

# YRD (name_1)
yrd_region <- c("Shanghai", "Jiangsu", "Zhejiang")

# PRD (name_1, name_2)

prd_region_name_2_guandong_regions <- c("Dongguan", "Foshan", "Guangzhou",
                                        "Huizhou", "Jiangmen", "Shenzhen",
                                        "Zhaoqing", "Zhongshan", "Zhuhai")

prd_region_name_1 <- c("Hong Kong", "Macao")

prd <- c(prd_region_name_2_guandong_regions, prd_region_name_1)


# add region column
color_2020_china  <- color_2020_china %>%
  mutate(region = ifelse(name_1 %in% bth_region, "BTH", "others"),
         region = ifelse(name_1 %in% yrd_region,  "YRD", region),
         region = ifelse(name_1 %in% prd_region_name_1, "PRD", region),
         region = ifelse(name_2 %in% prd_region_name_2_guandong_regions, "PRD", region))

# China factsheet figure 2 dataset
china_fs_fig2_dataset <- color_2020_china %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2020_pop_weighted = pop_weights*pm2020) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), avg_pm2.5_2020 = sum(pm2020_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2020 - who_guideline)*le_constant,
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  ungroup()


# plot China factsheet figure 2 dataset
china_fs_fig2 <- china_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = forcats::fct_reorder(name_1, le_gain), y = le_gain, fill = le_gain), width = 0.7) +
  labs(x = "Region", y = "Average Life Expectancy Gains (Years)") +
  scale_y_continuous(breaks = seq(0, 6, 0.5)) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")

