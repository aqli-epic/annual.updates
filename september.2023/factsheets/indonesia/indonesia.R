library('tidyverse')
library('ggnewscale')
library('glue')
library('fabricatr')
library('reshape2')
library('ggpubr')
library('sf')
library('igisci')
library('geodata')
library('svglite')

# set directory
dir <- ("~/Desktop/AQLI/Factsheets 2023")

# global operations
`%notin%` <- Negate(`%in%`)

# read shapefiles and filter for indonesia
gadm2_aqli_2021 <- st_read(glue("{dir}/gadm_shapefiles/gadm2/aqli_gadm2_final_june2023.shp")) 
gadm1_aqli_2021 <- st_read(glue("{dir}/gadm_shapefiles/gadm1/aqli_gadm1_final_june2023.shp"))
gadm0_aqli_2021 <- st_read(glue("{dir}/gadm_shapefiles/gadm0/aqli_gadm0_final_june2023.shp"))

# global variables
who_guideline <- 5
le_constant <- 0.098

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

# read and filter AQLI data
aqli_2021 <- read.csv(glue("{dir}/[June302023]aqli_internal_col_names_CSVs/[june302023]gadm2_aqli_2021_internal_col_names.csv")) %>%
  filter(country == "Indonesia")

# read and filter gbd results for indonesia
gbd_results_indonesia <- read.csv(glue("{dir}/gbd_results_master.csv")) %>%
  filter(country == "Indonesia")

# indonesia fs fig 1 data
indonesia_fs_fig1_dataset <- aqli_2021 %>%
  left_join(gadm2_aqli_2021, by = c("objectid_gadm2" = "obidgadm2")) %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2021', 'llpp_who_2021', 'geometry') %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# indonesia fs figure 1
indonesia_fs_fig1 <- indonesia_fs_fig1_dataset %>%
  ggplot() +
  geom_sf(mapping = aes(fill = reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2021 %>% filter(name0 == "Indonesia"), color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = gadm0_aqli_2021 %>% filter(name0 == "Indonesia"), color = "cornsilk4", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() + 
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
        legend.key = element_rect(color = "black"), 
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1))

# save indonesia fs figure 1
ggsave(glue("{dir}/indonesia/output/indonesia_fs_fig1.png"), height = 10, width = 15, indonesia_fs_fig1)

svglite("indonesia_fs_fig1")
ggsave(glue("{dir}/indonesia/output/indonesia_fs_fig1.svg"), height = 10, width = 15, dpi = 300, indonesia_fs_fig1)

# indonesia fs fig 2 data
indonesia_fs_fig2_dataset <- aqli_2021 %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2021', 'llpp_who_2021') %>%
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

# save figure 2 data
# indonesia_fs_fig2_dataset %>% write_csv(glue("{dir}/indonesia/output/indonesia_fs_fig2_dataset.csv"))

# indonesia fs figure 2
indonesia_fs_fig2 <- indonesia_fs_fig2_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(name_1, llpp_who_2021), y = llpp_who_2021, fill = lyl_bucket), width = 0.5) +
  labs(x = "Province", y = "Potential Gain in Life Expectancy (Years)", fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0, 3)) +
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

# save indonesia fs figure 2
ggsave(glue("{dir}/indonesia/output/indonesia_fs_fig2.png"), height = 9, width = 12, indonesia_fs_fig2)

svglite("indonesia_fs_fig2")
ggsave(glue("{dir}/indonesia/output/indonesia_fs_fig2.svg"), height = 9, width = 12, dpi = 300, indonesia_fs_fig2)

# filtering out those causes of death that are sort of* covered under PM2.5 in some broad way
colnames(gbd_results_indonesia)[3] <- c("llpp_who_2021")

indonesia_fs_fig3_dataset <- gbd_results_indonesia %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021") %>%
  slice_max(llpp_who_2021, n = 10)

# save figure 3 data
# indonesia_fs_fig3_dataset %>% write_csv(glue("{dir}/indonesia/output/indonesia_fs_fig3_dataset.csv"))

# indonesia factsheet figure 3
indonesia_fs_fig3 <- indonesia_fs_fig3_dataset %>%
  ggplot() + 
  geom_col(mapping = aes(x = reorder(cause_of_death, llpp_who_2021), y = llpp_who_2021, fill = reorder(lyl_bucket, order_lyl_bucket)), width = 0.5, color = "black") +
  labs(x = "Threats to life expectancy", y = "Life Years Lost", fill = "Life years lost") +
  coord_flip() + 
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16, margin = margin(r = 0.6, unit = "cm")),
        axis.title.x = element_text(size = 16, margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm"), face = "italic"),
        plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm"), face = "italic"),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(color = "white"),
        axis.line = element_line(), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(breaks = seq(0, 7, 0.5)) +
  # scale_x_discrete(limits = cause_of_death_ordered[seq(1, length(cause_of_death_ordered), by = 2)]) +
  scale_fill_manual(values = c("0 to < 0.1" = "#ffffff", 
                               "0.1 to < 0.5" = "#ffeda0", 
                               "0.5 to < 1" = "#fed976", 
                               "1 to < 2" = "#feb24c", 
                               "2 to < 3" = "#fd8d3c", 
                               "3 to < 4" = "#fc4e2a", 
                               "4 to < 5" = "#e31a1c", 
                               "5 to < 6" = "#bd0026", 
                               ">= 6" = "#800026")) +
  guides(fill = guide_legend(nrow = 1))

# scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +

# save indonesia fs figure 3
ggsave(glue("{dir}/indonesia/output/indonesia_fs_fig3.png"), height = 9, width = 15, indonesia_fs_fig3)

svglite("indonesia_fs_fig3")
ggsave(glue("{dir}/indonesia/output/indonesia_fs_fig3.svg"), height = 9, width = 15, dpi = 300, indonesia_fs_fig3)

# indonesia factsheet figure 4 dataset
indonesia_fs_fig4_dataset <- aqli_2021 %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

# save figure 4 data
# indonesia_fs_fig4_dataset %>% write_csv(glue("{dir}/indonesia/output/indonesia_fs_fig4_dataset.csv"))

# indonesia factsheet figure 4
indonesia_fs_fig4 <- indonesia_fs_fig4_dataset %>%
  ggplot() +
  geom_line(mapping = aes(x = as.integer(years), 
                          y = as.double(pop_weighted_avg_pm2.5)), 
            lwd = 1.1, color = "#82b5d5") +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  geom_hline(mapping = aes(yintercept = 15), lwd = 0.8, linetype = "dotted") + 
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 30)) +
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
  geom_text(x = 2001, y = 5.6, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2021): 5 µg/m³"), size = 5) +
  geom_text(x = 2006.6, y = 15.6, label = expression("Indonesia National" ~ PM[2.5] ~ "Standard: 15 µg/m³"), size = 5)

# save indonesia fs figure 4
ggsave(glue("{dir}/indonesia/output/indonesia_fs_fig4.png"), height = 9, width = 15, indonesia_fs_fig4)

svglite("indonesia_fs_fig4")
ggsave(glue("{dir}/indonesia/output/indonesia_fs_fig4.svg"), height = 9, width = 15, dpi = 300, indonesia_fs_fig4)

# indonesia factsheet appendix table 
indonesia_fs_appendix_table <- aqli_2021 %>%
  mutate(pm2021 = round(pm2021, 1),
         le_gain_red_to_who = round((pm2021 - who_guideline)*le_constant, 1),
         le_gain_red_to_who = ifelse(le_gain_red_to_who < 0, 0, le_gain_red_to_who),
         le_gain_red_to_nat = round((pm2021 - natstandard)*le_constant, 1),
         le_gain_red_to_nat = ifelse(le_gain_red_to_nat < 0, 0, le_gain_red_to_nat),
         le_gain_red_by_30_percent = round((pm2021*0.3)*le_constant, 1), 
         pop_millions = round(population/1000000, 3)) %>%
  select(name_2, pop_millions, pm2021, le_gain_red_to_who, le_gain_red_to_nat,
         le_gain_red_by_30_percent) %>%
  ungroup() %>%
  slice_max(pop_millions, n = 25) %>%
  mutate(pop_millions = round(pop_millions, 1)) %>%
  rename(`Regency/City` = name_2, 
         `Population (millions)` = pop_millions, 
         `Annual Average 2021 PM2.5 Concentration (µg/m³)` = pm2021,
         `Life Expectancy Gains from reducing PM2.5 from 2021 concentration to WHO PM2.5 guideline of 5 µg/m` = le_gain_red_to_who, 
         `Life Expectancy Gains from reducing PM2.5 from 2021 concentration to National PM2.5 guideline of 15 µg/m³` = le_gain_red_to_nat,
         `Life Expectancy Gains from reducing PM2.5 from 2021 concentration by 30 percent` = le_gain_red_by_30_percent) 


# save indonesia factsheet appendix table
indonesia_fs_appendix_table %>% write_csv(glue("{dir}/indonesia/output/indonesia_fs_appendix_table.csv"))

# other indonesia factsheet calculations
aqli_gadm0 <- read.csv(glue("{dir}/[june302023]gadm0_aqli_2021_internal_col_names.csv")) %>%
  filter(country == "Indonesia")

aqli_gadm1 <- aqli_2021 %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2021', 'llpp_who_2021') %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2021_pop_weighted = pop_weights*pm2021,
         llpp_who_2021_pop_weighted = pop_weights*llpp_who_2021) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), 
            avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2021 - who_guideline)*le_constant,
            llpp_who_2021 = sum(llpp_who_2021_pop_weighted, na.rm = TRUE),
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021")

ten_most_polluted <- aqli_2021 %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2021', 'llpp_who_2021') %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2021_pop_weighted = pop_weights*pm2021,
         llpp_who_2021_pop_weighted = pop_weights*llpp_who_2021) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE), 
            avg_pm2.5_2021 = sum(pm2021_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2021 - who_guideline)*le_constant,
            llpp_who_2021 = sum(llpp_who_2021_pop_weighted, na.rm = TRUE),
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(avg_pm2.5_2021, n = 10) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021")

all_gbd <- gbd_results_indonesia %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2021")

view(aqli_2021 %>% select(name_1, name_2, population, whostandard, natstandard, pm2021, llpp_who_2021, llpp_nat_2021))

sumatra <- aqli_2021 %>%
  filter(name_1 %in% c("Sumatera Selatan", "Sumatera Barat", "Sumatera Utara",
                       "Aceh", "Riau", "Jambi", "Bengkulu", "Lampung")) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

kalimantan <- aqli_2021 %>%
  filter(name_1 %in% c("Kalimantan Selatan", "Kalimantan Barat", "Kalimantan Utara",
                       "Kalimantan Tengah", "Kalimantan Timur")) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2021_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)


