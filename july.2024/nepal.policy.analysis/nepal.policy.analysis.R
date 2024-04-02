##### Load libraries #####
library('tidyverse')
library('ggnewscale')
library('fabricatr')
library('sf')


##### DATA #####
# read shapefiles 
gadm2_aqli_2021_shp <- st_read("~/Desktop/AQLI/shapefiles/gadm2/aqli_gadm2_final_june2023.shp") 
gadm1_aqli_2021_shp <- st_read("~/Desktop/AQLI/shapefiles/gadm1/aqli_gadm1_final_june2023.shp")
gadm0_aqli_2021_shp <- st_read("~/Desktop/AQLI/shapefiles/gadm0/aqli_gadm0_final_june2023.shp")

# read AQLI data
gadm2_aqli_2021 <- read.csv("~/Desktop/AQLI/2023 AQLI Update/[June302023]aqli_internal_col_names_CSVs/[june302023]gadm2_aqli_2021_internal_col_names.csv")
gadm1_aqli_2021 <- read.csv("~/Desktop/AQLI/2023 AQLI Update/[June302023]aqli_internal_col_names_CSVs/[june302023]gadm1_aqli_2021_internal_col_names.csv")
gadm0_aqli_2021 <- read.csv("~/Desktop/AQLI/2023 AQLI Update/[June302023]aqli_internal_col_names_CSVs/[june302023]gadm0_aqli_2021_internal_col_names.csv")

# read GBD data
gbd_results_master_2021 <- read.csv("~/Desktop/AQLI/2023 AQLI Update/gbd_results_master.csv")


##### SET UP ######
# global operations
`%notin%` <- Negate(`%in%`)

# global variables
who_guideline <- 5
le_constant <- 0.098

# colour scale function
add_aqli_color_scale_buckets <- function(df, scale_type = "pollution", col_name){
  if(scale_type == "lyl"){
    df %>%
      mutate(lyl_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 0.1), "0 to < 0.1", NA),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 0.1) & (!!as.symbol(col_name) < 0.5), "0.1 to < 0.5", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 0.5) & (!!as.symbol(col_name) < 1), "0.5 to < 1", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 1) & (!!as.symbol(col_name) < 2), "1 to < 2", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 2) & (!!as.symbol(col_name) < 3), "2 to < 3", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 3) & (!!as.symbol(col_name) < 4), "3 to < 4", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 4) & (!!as.symbol(col_name) < 5), "4 to < 5", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 6), "5 to < 6", lyl_bucket),
             lyl_bucket = ifelse((!!as.symbol(col_name) >= 6), ">= 6", lyl_bucket)) %>%
      mutate(order_lyl_bucket = ifelse(lyl_bucket == "0 to < 0.1", 1, NA),
             order_lyl_bucket = ifelse(lyl_bucket == "0.1 to < 0.5", 2, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "0.5 to < 1", 3, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "1 to < 2", 4, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "2 to < 3", 5, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "3 to < 4", 6, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "4 to < 5", 7, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == "5 to < 6", 8, order_lyl_bucket),
             order_lyl_bucket = ifelse(lyl_bucket == ">= 6", 9, order_lyl_bucket))
    
  } else if (scale_type == "pollution") {
    df %>%
      mutate(pol_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 5), "0 to < 5", NA),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 10), "5 to < 10", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 10) & (!!as.symbol(col_name) < 20), "10 to < 20", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 20) & (!!as.symbol(col_name) < 30), "20 to < 30", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 30) & (!!as.symbol(col_name) < 40), "30 to < 40", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 40) & (!!as.symbol(col_name) < 50), "40 to < 50", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 50) & (!!as.symbol(col_name) < 60), "50 to < 60", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 60) & (!!as.symbol(col_name) < 70), "60 to < 70", pol_bucket),
             pol_bucket = ifelse((!!as.symbol(col_name) >= 70), ">= 70", pol_bucket)) %>%
      mutate(order_pol_bucket = ifelse(pol_bucket == "0 to < 5", 1, NA),
             order_pol_bucket = ifelse(pol_bucket == "5 to < 10", 2, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "10 to < 20", 3, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "20 to < 30", 4, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "30 to < 40", 5, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "40 to < 50", 6, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "50 to < 60", 7, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == "60 to < 70", 8, order_pol_bucket),
             order_pol_bucket = ifelse(pol_bucket == ">= 70", 9, order_pol_bucket))
    
  } else if (scale_type == "lyldiff"){
    
    df %>%
      mutate(lyldiff_bucket = ifelse((!!as.symbol(col_name) < -2), "< -2", NA),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -2) & (!!as.symbol(col_name) < -0.5), "-2 to (< -0.5)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -0.5) & (!!as.symbol(col_name) < -0.1), "-0.5 to (< -0.1)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= -0.1) & (!!as.symbol(col_name) < 0), "-0.1 to (< 0)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 0.1), "0 to (< 0.1)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0.1) & (!!as.symbol(col_name) < 0.5), "0.1 to (< 0.5)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 0.5) & (!!as.symbol(col_name) < 2), "0.5 to (< 2)", lyldiff_bucket),
             lyldiff_bucket = ifelse((!!as.symbol(col_name) >= 2), ">= 2", lyldiff_bucket)) %>%
      mutate(order_lyldiff_bucket = ifelse(lyldiff_bucket == "< -2", 1, NA),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-2 to (< -0.5)", 2, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-0.5 to (< -0.1)", 3, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "-0.1 to (< 0)", 4, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0 to (< 0.1)", 5, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0.1 to (< 0.5)", 6, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == "0.5 to (< 2)", 7, order_lyldiff_bucket),
             order_lyldiff_bucket = ifelse(lyldiff_bucket == ">= 2", 8, order_lyldiff_bucket))
    
  } else if (scale_type == "poldiff"){
    
    df %>%
      mutate(poldiff_bucket = ifelse((!!as.symbol(col_name) < -10), "< -10", NA),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -10) & (!!as.symbol(col_name) < -5), "-10 to (< -5)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -5) & (!!as.symbol(col_name) < -1), "-5 to (< -1)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= -1) & (!!as.symbol(col_name) < 0), "-1 to (< 0)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 0) & (!!as.symbol(col_name) < 1), "0 to (< 1)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 1) & (!!as.symbol(col_name) < 5), "1 to (< 5)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 5) & (!!as.symbol(col_name) < 10), "5 to (< 10)", poldiff_bucket),
             poldiff_bucket = ifelse((!!as.symbol(col_name) >= 10), ">= 10", poldiff_bucket)) %>%
      mutate(order_poldiff_bucket = ifelse(poldiff_bucket == "< -10", 1, NA),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-10 to (< -5)", 2, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-5 to (< -1)", 3, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "-1 to (< 0)", 4, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "0 to (< 1)", 5, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "1 to (< 5)", 6, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == "5 to (< 10)", 7, order_poldiff_bucket),
             order_poldiff_bucket = ifelse(poldiff_bucket == ">= 10", 8, order_poldiff_bucket))
    
  }
  
}


##### Figure 1 #####
# nepal policy analysis fig 1 data
nepal_fig1_dataset <- gadm2_aqli_2021 %>%
  filter(country == "Nepal") %>%
  left_join(gadm2_aqli_2021_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2021', 'llpp_who_2021', 'geometry') %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

most_least_poll <- gadm2_aqli_2021_shp %>% 
  filter(name0 == "Nepal", name2 %in% c("Mahottari", "Humla"))

# figure 1
nepal_fig1 <- nepal_fig1_dataset %>%
  ggplot() +
  geom_sf(mapping = aes(fill = reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2021_shp %>% filter(name0 == "Nepal"), color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = gadm0_aqli_2021_shp %>% filter(name0 == "Nepal"), color = "cornsilk4", fill = "transparent", lwd = 0.5) +
  geom_sf(data = most_least_poll, aes(linetype = name2), color = "black", fill = "transparent", lwd = 0.75) +
  ggthemes::theme_map() + 
  scale_linetype_manual(name= "District", values = c("dotdash", "longdash")) +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff", 
                               "0.1 to < 0.5" = "#ffeda0", 
                               "0.5 to < 1" = "#fed976", 
                               "1 to < 2" = "#feb24c", 
                               "2 to < 3" = "#fd8d3c", 
                               "3 to < 4" = "#fc4e2a", 
                               "4 to < 5" = "#e31a1c", 
                               "5 to < 6" = "#bd0026", 
                               ">= 6" = "#800026")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years)") + 
  theme(legend.position = "bottom", 
        legend.justification = c(0.5, 3), 
        legend.background = element_rect(color = "black"), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 15), 
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7), 
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"), 
        # legend.key = element_rect(color = "black"), 
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1))

# save nepal figure 1
# ggsave("~/Desktop/AQLI/REPO/annual.updates/july.2024/nepal.policy.analysis/fig1.png", nepal_fig1, height = 10, width = 15)


##### Figure 2 #####
# nepal policy analysis figure 2 dataset
nepal_fig2_dataset <- gadm2_aqli_2021 %>%
  filter(country == "Nepal") %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# nepal factsheet figure 4
nepal_fig2 <- nepal_fig2_dataset %>%
  ggplot() +
  geom_line(mapping = aes(x = as.integer(years), 
                          y = as.double(pop_weighted_avg_pm2.5)), 
            lwd = 1.1, color = "#3c456f") +
  geom_smooth(aes(x = as.integer(years), y = as.double(pop_weighted_avg_pm2.5)),
              lwd = 0.5, linetype = "dashed", se = FALSE) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 60)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2021))  +
  ggthemes::theme_tufte() +
  labs(x = "Year", 
       y = expression("Annual Average" ~ PM[2.5] ~ "Concentration (in µg/m³)")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 13, margin = margin(r = 0.6, unit = "cm")), 
        axis.title.x = element_text(size = 13, margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.line = element_line(), 
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 0.7, unit = "cm")), 
        plot.subtitle = element_text(hjust = 0.5, size = 8, face = "italic"), 
        plot.caption = element_text(size = 7, margin = margin(t = 0.8, unit = "cm"), hjust = 0, face = "italic"),
        axis.text = element_text(size = 12), 
        plot.background = element_rect(color = "white"), 
        axis.ticks = element_blank()) +
  geom_text(x = 2002.8, y = 6.8, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 4.5)

# save figure 2
# ggsave("~/Desktop/AQLI/REPO/annual.updates/july.2024/nepal.policy.analysis/fig2.png", nepal_fig2, height = 8, width = 10)


##### Figure 3 #####
# nepal policy analysis fig 3 data
nepal_fig3_dataset <- gadm2_aqli_2021 %>%
  filter(country == "Nepal") %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2021', 'llpp_who_2021') %>%
  mutate(name_1 = ifelse(name_1 == "Province 1", "Koshi", name_1)) %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2021_pop_weighted = pop_weights*pm2021,
         llpp_who_2021_pop_weighted = pop_weights*llpp_who_2021) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), 
            avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2021 - who_guideline)*le_constant,
            llpp_who_2021 = sum(llpp_who_2021_pop_weighted, na.rm = TRUE),
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021")

# nepal figure 3
nepal_fig3 <- nepal_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(name_1, llpp_who_2021), y = llpp_who_2021, fill = lyl_bucket), width = 0.5) +
  labs(x = "Province", y = "Potential Gain in Life Expectancy (Years)", fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 8, 1), limits = c(0, 8)) +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff", 
                               "0.1 to < 0.5" = "#ffeda0", 
                               "0.5 to < 1" = "#fed976", 
                               "1 to < 2" = "#feb24c", 
                               "2 to < 3" = "#fd8d3c", 
                               "3 to < 4" = "#fc4e2a", 
                               "4 to < 5" = "#e31a1c", 
                               "5 to < 6" = "#bd0026", 
                               ">= 6" = "#800026")) +
  coord_flip() + 
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust =  0.5, size = 10, face = "italic", margin = margin(b = 0.8, unit = "cm")), 
        plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(color = "white"),
        axis.line = element_line(), 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 13), 
        axis.title.y = element_text(margin = margin(r = 0.7, unit = "cm")), 
        axis.title.x = element_text(margin = margin(t = 0.6, b = 0.6, unit = "cm")), 
        axis.ticks = element_blank()) 

# save figure 3
# ggsave("~/Desktop/AQLI/REPO/annual.updates/july.2024/nepal.policy.analysis/fig3.png", nepal_fig3, height = 8, width = 10)


##### Figure 4 #####
#  state wise min, average and max lyl dataset for 10 most populous states
nepal_fig4_dataset <- gadm2_aqli_2021 %>%
  filter(country == "Nepal") %>%
  mutate(name_1 = ifelse(name_1 == "Province 1", "Koshi", name_1)) %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         pm2021_pop_weighted = pm2021*pop_weights) %>%
  summarise(min_lyl = round(min(llpp_who_2021, na.rm = TRUE), 1),
            max_lyl = round(max(llpp_who_2021, na.rm = TRUE), 1),
            avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE),
            avg_lyl = round((avg_pm2.5_2021 - 5)*0.098, 1),
            range_lyl = max_lyl - min_lyl,
            total_population = sum(population, na.rm = TRUE)) %>%
  mutate(avg_lyl = ifelse(avg_lyl < 0, 0, avg_lyl))


# plot lyl range graph (click on zoom in the plots window in R-Studio to view the plot)
nepal_fig4 <- nepal_fig4_dataset  %>%
  ggplot(aes(x = forcats::fct_reorder(name_1, total_population), y = min_lyl)) +
  geom_point(aes(color = "Minimum life years lost"), size = 3) +
  geom_point(aes(y = max_lyl, color = "Maximum life years lost"), size = 3) +
  geom_point(aes(y = avg_lyl, color = "Average life years lost"), size = 3) +
  geom_segment(aes(xend = name_1, yend = max_lyl),
               linetype = "dashed", color = "black") +
  scale_color_manual(name = "", values = c("Minimum life years lost" = "cornflowerblue",
                                           "Average life years lost" = "darkgrey",
                                           "Maximum life years lost" = "darkred"), 
                     breaks = c("Minimum life years lost", "Average life years lost", "Maximum life years lost"),
                     labels = c("Minimum life years lost", "Average life years lost", "Maximum life years lost")) +
  labs(title = expression("Minimum, Average, and Maximum Life years lost to" ~ PM[2.5] ~ "Air Pollution"),
       x = "Province", y = "Life Years Lost") +
  coord_flip() +
  theme(legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 8, 1), limits = c(0, 8)) +
  ggthemes::theme_clean() +
  theme(legend.position = "bottom", 
        axis.title.y = element_text(margin = margin(r = 0.8, unit = "cm"), size = 16), 
        axis.title.x = element_text(margin = margin(t = 0.8, b = 1, unit = "cm"), size = 16), 
        axis.text = element_text(size = 14), 
        plot.title = element_text(hjust = 0.5, size = 18, margin = margin(b = 0.3, unit = "cm")),
        legend.box.background = element_rect(color = "black"),
        strip.text = element_text(size = 18, margin = margin(t = 1, b = 0.5, unit = "cm")),
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 0.8, unit = "cm"), face = "italic"),
        plot.caption = element_text(hjust = 0, size = 9, face = "italic", margin = margin(t = 1.3, unit = "cm")), 
        plot.background = element_rect(fill = "white", color = "white"))  

# save figure 4
# ggsave("~/Desktop/AQLI/REPO/annual.updates/july.2024/nepal.policy.analysis/fig4.png", nepal_fig4, height = 8, width = 10)


##### Other factsheet calculations #####
view(gadm2_aqli_2021 %>% filter(country=="Nepal"))

gadm0_aqli_2021 %>%
  filter(country == "Nepal") %>%
  select(country, population, pm2021, pm2020, pm2010, pm1998) %>%
  mutate(pm_diff_2020 = pm2021-pm2020,
         pm_diff_1998 = pm2021-pm1998,
         pm_diff_2010 = pm2021-pm2010,
         pct_change_2020 = 100*pm_diff_2020/pm2020,
         pct_change_2010 = 100*pm_diff_2010/pm2010,
         pct_change_1998 = 100*pm_diff_1998/pm1998)

# Nepal LYL- WHO guideline
round(le_constant*(51.71-who_guideline), 1)
# Nepal LYL- WHO interim target 1
round(le_constant*(51.71-35), 1)
# Nepal LYL- WHO interim target 2
round(le_constant*(51.71-25), 1)

# most polluted
gadm2_aqli_2021 %>%
  filter(country == "Nepal") %>%
  select(country, name_1, name_2, population, pm2021) %>%
  slice_max(pm2021, n = 10) %>%
  mutate(tot_pop = sum(population, na.rm = TRUE),
         pop_weights = population/tot_pop,
         pm2021_pop_weighted = sum(pop_weights*pm2021),
         llpp_who_2021_pop_weighted = le_constant*(pm2021_pop_weighted-who_guideline))

# least polluted
gadm2_aqli_2021 %>%
  filter(country == "Nepal") %>%
  select(country, name_1, name_2, population, pm2021) %>%
  slice_min(pm2021, n = 10) %>%
  mutate(tot_pop = sum(population, na.rm = TRUE),
         pop_weights = population/tot_pop,
         pm2021_pop_weighted = sum(pop_weights*pm2021),
         llpp_who_2021_pop_weighted = le_constant*(pm2021_pop_weighted-who_guideline))

# most polluted (Mahottari) LYL
le_constant*(81.69-who_guideline)
# least polluted (Humla) LYL
le_constant*(17.89-who_guideline)