# read in the helper file
source("R/july.2024.helper.script.R")

# China figure 7.1 ============
color_2022_china <- gadm2_aqli_2022 %>%
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
color_2022_china  <- color_2022_china %>%
  mutate(region = ifelse(name_1 %in% bth_region, "BTH", "others"), 
         region = ifelse(name_1 %in% yrd_region,  "YRD", region), 
         region = ifelse(name_1 %in% prd_region_name_1, "PRD", region), 
         region = ifelse(name_2 %in% prd_region_name_2_guandong_regions, "PRD", region)) 

# China section fig1 data part-1: trend lines figure region wise (BTH, YRD, PRD) dataset
trendlines_china_region_wise_df <- color_2022_china %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5) %>%
  filter(region %in% c("YRD", "BTH", "PRD"))

# China section fig1 data (part-2): trend line national average dataset
trendline_national_avg_df <- color_2022_china %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))), 
         region = "China") %>% 
  select(years, region, pop_weighted_avg_pm2.5)

# China section figure 1 data final
ar_china_fig7.1_data <- rbind(trendlines_china_region_wise_df, trendline_national_avg_df)

ar_china_fig7.1_data <- ar_china_fig7.1_data %>%
  mutate(region_order = ifelse(region == "China", 1, NA), 
         region_order = ifelse(region == "PRD", 2, region_order), 
         region_order = ifelse(region == "YRD", 3, region_order), 
         region_order = ifelse(region == "BTH", 4, region_order))

ar_china_fig7.1_data$region <- factor(ar_china_fig7.1_data$region, levels = c("BTH", "China", "YRD", "PRD"))

# plot
ar_china_fig7.1 <- ar_china_fig7.1_data %>%
  ggplot() +
  geom_line(mapping = ggplot2::aes(x = years, y = pop_weighted_avg_pm2.5, color = interaction(region), 
                                   linetype = interaction(region)), lwd = 1.1) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.5, linetype = "dashed") +
  geom_vline(mapping = aes(xintercept = 2014), lwd = 0.5, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(breaks = c(seq(1998, 2022, 3))) +
  scale_color_manual(values = c("PRD" = "#7197be", "China" = "#5f7aa5", "BTH" = "#5f7aa5", "YRD" = "#7197be"), name = "legend") +
  scale_linetype_manual(values = c("PRD" = "dashed", "China" = "solid", "BTH" = "dashed", "YRD" = "dashed"), name = "legend") +
  ggthemes::theme_clean() +
  themes_aqli_base +
  labs(x = "Year", 
       y = expression("Annual Average  " ~ PM[2.5] ~ " Concentration (in µg/m³)"), 
       title = "", 
       subtitle = "", 
       color = "Region") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        plot.background = element_rect(fill = "white", color = "white"), 
        legend.key.width = unit(2, "cm")) + 
  geom_text(x = 2002.1, y = 8.1, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³")) + 
  geom_text(x = 2015.8, y = 92.8, label = str_wrap("China announces war on pollution", width = 18)) 
