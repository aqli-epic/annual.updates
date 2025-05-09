# Load in master helper file for june.2022 folder (uncomment this before running)-------------------------------

# source("./june.2022/R/june.2022.helper.script.R")

#> China factsheet figure 3: PM2.5 concentrations in Mainland China overtime (1998 to 2020)

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

# China factsheet figure 3 (part-1): trend lines figure region wise (BTH, YRD, PRD) dataset
trendlines_china_region_wise_df <- color_2020_china %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5) %>%
  filter(region %in% c("YRD", "BTH", "PRD"))

# China factsheet figure 3 (part-2): trend line national average dataset
trendline_national_avg_df <- color_2020_china %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2020_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "China") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# China factsheet figure 3 dataset final
china_fs_fig3_dataset <- rbind(trendlines_china_region_wise_df, trendline_national_avg_df)

# save China factsheet figure 3 dataset
china_fs_fig3_dataset %>%
  write_csv("./june.2022/factsheets/china/graph.wise.datasets/china_fs_fig3.csv")

# plot china factsheet figure 3
china_fs_fig3 <- ggplot(ar_fig15_dataset) +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5, color = region), lwd = 1.1) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.5, linetype = "dashed") +
  geom_vline(mapping = aes(xintercept = 2014), lwd = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(1998, 2020, 2)) +
  scale_color_manual(values = c("China" = "darkgrey", "BTH" = "darkred", "YRD" = "darkorange", "PRD" = "burlywood1")) +
  ggthemes::theme_clean() +
  labs(x = "Years",
       y = expression(paste("Average PM2.5 concentration ( ", mu, "g", "/", m^3, " )"))) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 6.5),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  geom_text(x = 2003.8, y = 10, label = expression(paste("WHO PM2.5 Guideline (last updated: 2021): 5 ", mu, "g","/", m^3, ""))) +
  geom_text(x = 2016.5, y = 93, label = str_wrap("China announces war on pollution", width = 18))


# save China factsheet figure 3
ggsave("./june.2022/factsheets/china/graphs/china_fs_fig3.png", china_fs_fig3)

