# read in the helper file
source("~/R/july.2025.helper.script.R")

# Figure 5: Annual average PM2.5 concentrations in major regions in Mainland China, 1998-2023

# filtering the color file for China
color_2023_china <- gadm2_aqli_2023 %>%
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

# Fenwei plain
fenwei_plain <- c("Xi’an", "Baoji", "Xianyang", "Weinan","Tongchuan", "Jinzhong", "Lvliang", "Linfen", "Yuncheng", "Luoyang", "Sanmenxia")

# add region column
color_2023_china  <- color_2023_china %>%
  mutate(region = ifelse(name_1 %in% bth_region, "BTH", "others"),
         region = ifelse(name_1 %in% yrd_region,  "YRD", region),
         region = ifelse(name_1 %in% prd_region_name_1, "PRD", region),
         region = ifelse(name_2 %in% prd_region_name_2_guandong_regions, "PRD", region))


# China factsheet figure 3 (part-1): trend lines figure region wise (BTH, YRD, PRD) dataset
trendlines_china_region_wise_df <- color_2023_china %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5) %>%
  filter(region %in% c("YRD", "BTH", "PRD"))

# China factsheet figure 3 (part-2): trend line national average dataset
trendline_national_avg_df <- color_2023_china %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2023_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "China") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# China factsheet figure 3 dataset final
china_fs_fig5_dataset <- rbind(trendlines_china_region_wise_df, trendline_national_avg_df)

china_fs_fig5_dataset <-  china_fs_fig5_dataset %>%
    mutate(region_order = ifelse(region == "China", 1, NA),
         region_order = ifelse(region == "PRD", 2, region_order),
         region_order = ifelse(region == "YRD", 3, region_order),
         region_order = ifelse(region == "BTH", 4, region_order))


china_fs_fig5_dataset$region <- factor(china_fs_fig5_dataset$region, levels = c("BTH", "China", "YRD", "PRD"))

# plot china factsheet figure 5
china_fs_fig5 <- china_fs_fig5_dataset %>%
  ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5, color = interaction(region),
                                   linetype = interaction(region)), lwd = 1.1) +
    geom_hline(mapping = aes(yintercept = 5), lwd = 0.5, linetype = "dashed") +
  geom_hline(mapping = aes(yintercept = 35), lwd = 0.5, linetype = "dashed") +
    geom_vline(mapping = aes(xintercept = 2014), lwd = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(breaks = c(seq(1998, 2018, 5), 2023)) +
    scale_color_manual(values = c("PRD" = "#7197be", "China" = "#5f7aa5", "BTH" = "#5f7aa5", "YRD" = "#7197be"), name = "legend") +
    scale_linetype_manual(
    values = c("PRD" = "dashed", "China" = "solid", "BTH" = "dashed", "YRD" = "dashed"), name = "legend") +
  ggthemes::theme_clean() +
    themes_aqli_base +
  labs(x = "Year",
       y = expression("Annual Average  " ~ PM[2.5] ~ " Concentration (in µg/m³)"),
       title = "",
    subtitle = "",
    color = "Region") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, color="#222222"),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
        axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
        axis.line = element_line(),
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"),
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 20, color="#222222"),
        plot.background = element_rect(color = "white", fill = "white"),
        axis.ticks = element_blank()) +
    geom_text(x = 2003.54, y = 8.1, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 7.5) +
    geom_text(x = 2015.3, y = 92.8, label = str_wrap("China announces war on pollution", width = 18), size = 7.5)+
  geom_text(x = 2003.2, y = 36.7, label = expression("China National" ~ PM[2.5] ~ "Standard: 35 µg/m³"), size = 7.5)

