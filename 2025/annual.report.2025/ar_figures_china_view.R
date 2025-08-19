# read in the helper file
source("~/R/july.2025.helper.script.R") 

#### South Asia Figure 5.2 #### 
# # project & fix validity
# india_gadm0_w_ap <- gadm0_aqli_shp %>%
#   filter(name0 == "India") 
# 
# ind <- india_gadm0_w_ap %>%
#   st_transform(3857) %>%
#   st_make_valid()
# 
# ap <- gadm1_aqli_shp %>%
#   filter(name0 == "India", name1 %in% c("Arunachal Pradesh")) %>%
#   st_transform(3857) %>%
#   st_make_valid() %>%
#   st_union() 
# 
# # set tolerance of 100m to account for gaps in shapefiles
# tol <- 100 
# ind <- ind %>% st_set_precision(tol) %>% st_snap(ap, tolerance = tol)
# ap  <- ap  %>% st_set_precision(tol)
# 
# # erase
# ind_minus_ap <- st_difference(ind, ap)
# 
# # keep only polygons and drop specks
# india_gadm0_chnwv <- ind_minus_ap %>%
#   st_collection_extract("POLYGON") %>%   # remove POINT/LINESTRING scraps
#   st_make_valid() %>%
#   mutate(area_m2 = as.numeric(st_area(.))) %>%
#   filter(area_m2 > 1e6) %>%              # drop tiny slivers (< ~1 km^2). Tweak threshold.
#   select(-area_m2)
# 
# india_gadm0_chnwv %>% 
#   st_write("~/Desktop/AQLI/shapefiles/China_Worldview/gadm0_India_ChnWV/india_gadm0.shp")

india_gadm0 <- st_read("~/Desktop/AQLI/shapefiles/China_Worldview/gadm0_India_ChnWV/india_gadm0.shp") %>%
  mutate(name0 = "India",
         obidgadm0 = 101,
         isoalp3 = "IND") %>%
  select(obidgadm0, isoalp3, name0, geometry)

india_gadm0 <- st_transform(india_gadm0, 4326)

test <- ggplot() +
  geom_sf(data = india_gadm0, color = "darkgrey", fill = "white", lwd = 0.3)

ggsave("test.png", test, width = 15, height = 10)

# data
# south asia without india and pakistan
sa_wo_ind <- gadm2_aqli_shp %>%
  filter(name0 %in% c("Afghanistan", "Bangladesh", "Bhutan", "Maldives", "Nepal", 
                      "Pakistan", "Sri Lanka"))

south_asia_gadm2_shp <- gadm2_aqli_shp %>%
  filter(name0 == "India", name1 != "Arunachal Pradesh") %>%
  bind_rows(sa_wo_ind)

south_asia_gadm1_shp <- gadm1_aqli_shp %>%
  filter(name0 == "India", name1 != "Arunachal Pradesh") %>%
  bind_rows(gadm1_aqli_shp %>% filter(name0 %in% c("Afghanistan", "Bangladesh", "Bhutan", "Maldives", 
                                                   "Nepal", "Pakistan", "Sri Lanka")))

sa_wo_ind_gadm0 <- gadm0_aqli_shp %>% 
  filter(name0 %in% c("Afghanistan", "Bangladesh", "Bhutan", "Maldives", 
                      "Nepal", "Pakistan", "Sri Lanka"))

ar_south_asia_fs_fig5.2_data <- gadm2_aqli_2023 %>%
  mutate(who2023 = ifelse(pm2022 <= 5, 0, who2023)) %>%
  filter(!is.na(who2023)) %>%
  inner_join(south_asia_gadm2_shp, by = c("id" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "who2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# south asia fig 5.2
ar_south_asia_fs_fig5.2 <- ar_south_asia_fs_fig5.2_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = south_asia_gadm1_shp, color = "azure4", fill = "transparent", lwd = 0.1) +
  geom_sf(data = india_gadm0, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  geom_sf(data = sa_wo_ind_gadm0, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF", 
                               "0.1 to < 0.5" = "#FFF2E1", 
                               "0.5 to < 1" = "#FFEDD3", 
                               "1 to < 2" = "#FFC97A", 
                               "2 to < 3" = "#FFA521", 
                               "3 to < 4" = "#EB6C2A", 
                               "4 to < 5" = "#D63333", 
                               "5 to < 6" = "#8E2946", 
                               ">= 6" = "#451F59")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years)  ", title = "", 
       subtitle = "") + 
  theme(legend.position = "bottom", 
        legend.justification = "center", 
        legend.background = element_rect(color = "black"), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 15), 
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"), 
        legend.key = element_rect(color = "black"), 
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal", 
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))

#### China Figures #### 
arunachal_gadm2 <- gadm2_aqli_shp %>%
  filter(name0 == "India", name1 %in% c("Arunachal Pradesh")) %>%
  mutate(name0 = "China")

taiwan_gadm2 <- gadm2_aqli_shp %>%
  filter(name0 == "Taiwan") %>%
  mutate(name0 = "China")

china_wv_gadm2 <- gadm2_aqli_shp %>%
  filter(name0 == "China") %>%
  bind_rows(arunachal_gadm2) %>%
  bind_rows(taiwan_gadm2)

arunachal_gadm1 <- gadm1_aqli_shp %>%
  filter(name0 == "India", name1 %in% c("Arunachal Pradesh")) %>%
  mutate(name0 = "China")

taiwan_gadm1 <- gadm1_aqli_shp %>%
  filter(name0 == "Taiwan") %>%
  mutate(name0 = "China")

china_wv_gadm1 <- gadm1_aqli_shp %>%
  filter(name0 == "China") %>%
  bind_rows(arunachal_gadm1) %>%
  bind_rows(taiwan_gadm1)

# sf_use_s2(FALSE) 
# china_wv_gadm0 <- china_wv_gadm1 %>% st_union() 
# china_wv_gadm0 %>% st_write("~/Desktop/AQLI/shapefiles/China_country_ChnWV/china_gadm0.shp")

china_gadm0 <- st_read("~/Desktop/AQLI/shapefiles/China_Worldview/gadm0_China_ChnWV/china_gadm0.shp") %>%
  mutate(name0 = "China",
         obidgadm0 = 46,
         isoalp3 = "CHN") %>%
  select(obidgadm0, isoalp3, name0, geometry)

test2 <- china_wv_gadm2 %>%
  ggplot() +
  geom_sf(color = "darkgrey", fill = "white", lwd = 0.05) +
  geom_sf(data = china_wv_gadm1, color = "black", fill = "transparent", lwd = 0.1) +
  geom_sf(data = china_gadm0, color = "cornsilk4", fill = "transparent", lwd = 0.3)

ggsave("test2.png", test, width = 15, height = 10)

##### Figure 6.1 ##### 
# filter data for china and create shapefile
ar_china_fig6.1_data <- gadm2_aqli_2023 %>%
  mutate(who2023 = ifelse(pm2022 <= 5, 0, who2023)) %>%
  inner_join(china_wv_gadm2, by = c("id" = "obidgadm2")) %>%
  add_aqli_color_scale_buckets("lyl", "who2023") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# plot
ar_china_fig6.1 <- ar_china_fig6.1_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = china_wv_gadm1, color = "azure4", fill = "transparent", lwd = 0.1) +
  geom_sf(data = china_gadm0, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  ggthemes::theme_map() +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF", 
                               "0.1 to < 0.5" = "#FFF2E1", 
                               "0.5 to < 1" = "#FFEDD3", 
                               "1 to < 2" = "#FFC97A", 
                               "2 to < 3" = "#FFA521", 
                               "3 to < 4" = "#EB6C2A", 
                               "4 to < 5" = "#D63333", 
                               "5 to < 6" = "#8E2946", 
                               ">= 6" = "#451F59")) +
  ggthemes::theme_map() +
  labs(fill = "Potential gain in life expectancy (Years)  ", title = "",
       subtitle = "") +
  theme(legend.position = "bottom",
        legend.justification = c(0.5, 3),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 15),
        # legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"),
        legend.box.spacing = unit(2, "cm"),
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))

##### Figure 6.2 ##### 
ar_china_fig6.2_data <- gadm2_aqli_2023 %>%
  mutate(who2023 = ifelse(pm2022 <= 5, 0, who2023)) %>%
  filter(!is.na(who2023)) %>%
  inner_join(china_wv_gadm2, by = c("id" = "obidgadm2")) %>%
  mutate(lyl2014minus2023 = round((pm2014 - pm2023)*0.098, 1)) %>% # to reflect gain in life expectancy
  add_aqli_color_scale_buckets(scale_type = "lyldiff", col_name = "lyl2014minus2023") %>% 
  select(-geometry, geometry) %>%
  st_as_sf()

# china figure 2
ar_china_fig6.2 <- ar_china_fig6.2_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = forcats::fct_reorder(lyldiff_bucket, order_lyldiff_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = china_wv_gadm1, color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = china_gadm0, color = "cornsilk4", fill = "transparent", lwd = 0.5) +
  ggthemes::theme_map() + 
  scale_fill_manual(values = c(">= 2" = "#008fbb",
                               "0.5 to (< 2)" = "#4fb6d3",
                               "0.1 to (< 0.5)" = "#99dbe9",
                               "0 to (< 0.1)" = "#d2eef4",
                               "-0.1 to (< 0)" = "#ffd393",
                               "-0.5 to (< -0.1)" = "#fea222",
                               "-2 to (< -0.5)" = "#ec6f29",
                               "< -2" = "#d63333")) +
  ggthemes::theme_map() +
  labs(fill = "Change in life expectancy between 2014 and 2022 \n(Years; blue values indicate improvement)", title = "") + 
  theme(legend.position = "bottom", 
        legend.justification = "center", 
        legend.background = element_rect(color = "black"), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.box.margin = margin(b = 1, unit = "cm"),
        plot.subtitle = element_text(hjust = 0.5, size = 7), 
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.key = element_rect(color = "black"), 
        legend.box.spacing = unit(0, "cm"), 
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))

#### Global Figures ####
# country_shape_file_china_wv_1 <- gadm0_aqli_shp %>%
#   filter(name0 %notin% c("China", "India"))
# 
# country_shape_file_china_wv <- country_shape_file_china_wv_1 %>%
#   bind_rows(india_gadm0) %>%
#   bind_rows(china_gadm0)
# 
# country_shape_file_china_wv %>% 
#   st_write("~/Desktop/AQLI/shapefiles/China_Worldview/gadm0_ChnWV/country_gadm0.shp")

country_shape_file_china_wv <- st_read("~/Desktop/AQLI/shapefiles/China_Worldview/gadm0_ChnWV/country_gadm0.shp")

test3 <- country_shape_file_china_wv %>%
  ggplot() +
  geom_sf(color = "darkgrey", fill = "white", lwd = 0.3)

ggsave("test3.png", test3, width = 15, height = 10)

##### Figure 2.1 #####
monitoring_data <- read_csv("~/Desktop/AQLI/REPO/opportunity-score/data/input/no_of_monitors_govt_other.csv") %>% 
  # remove rows with missing country names
  filter(!is.na(name)) %>%
  mutate(Monitor_govt_pvt = ifelse(ismonitor == TRUE, "govt", "pvt"))

# Filter government monitors and fix inconsistent country names
monitoring_data_gvt <- monitoring_data %>% 
  # keep only ref grade monitor data
  filter(Monitor_govt_pvt == "govt") %>%
  mutate(name = case_when(
    name == "C√¥te d'Ivoire" ~ "Côte d'Ivoire",
    name == "Bosnia and Herz." ~ "Bosnia and Herzegovina",
    name == "Mexico" ~ "México",
    name == "Dem. Rep. Congo" ~ "Democratic Republic of the Congo",
    name == "United States of America" ~ "United States",
    TRUE ~ name
  ))

# keep Population and assign AQLI regions
aqli_region_countries <- gadm0_aqli_2023 %>%
  select(name, population) %>%
  mutate(`AQLI Region` = case_when(
    name %in% south_asia_def ~ "South Asia",
    name %in% central_and_west_african_countries ~ "Central and west africa",
    name %in% se_asia_vec ~ "South East Asia",
    name %in% latin_america_countries_vec ~ "Latin America",
    name %in% mena_countries ~ "Middle East and North Africa",
    name %in% unlist(european_countries) ~ "Europe",
    name %in% oceania ~ "Oceania",
    name %in% c("United States", "Canada") ~ "US + Canada",
    TRUE ~ "Rest of the World" # Default case for all others
  )) %>%
  filter(`AQLI Region` != "Rest of the World")

# monitor density
monitor_density <- aqli_region_countries %>%
  left_join(monitoring_data_gvt, by = c("name")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  select(`AQLI Region`, name, ismonitor, count, population) %>%
  mutate(`monitor_density (million)` = round(count * 1000000 / population, 3))

# define colour scheme
monitor_colors <- c( "0 to < 5"   = "#e0feff",
                     "5 to < 10"  = "#b7ebf1",
                     "10 to < 30" = "#66c4d6",
                     "30 to < 50" = "#229dbb",
                     "50 to < 60" = "#00518a")

# define factor levels (legend order)
monitor_levels <- names(monitor_colors)

# figure 2.1 data
ar_global_fig2.1_data <- monitor_density %>%
  mutate(monitor_density_bucket = case_when(is.na(`monitor_density (million)`) ~ NA_character_,
                                            `monitor_density (million)` < 5    ~ "0 to < 5",
                                            `monitor_density (million)` < 10   ~ "5 to < 10",
                                            `monitor_density (million)` < 30   ~ "10 to < 30",
                                            `monitor_density (million)` < 50   ~ "30 to < 50",
                                            `monitor_density (million)` < 60   ~ "50 to < 60")) %>%
  mutate(monitor_density_bucket = factor(monitor_density_bucket, levels = monitor_levels)) %>%
  inner_join(country_shape_file_china_wv, by = c("name" = "name0")) %>%
  select(-geometry, geometry, ) %>%
  st_as_sf()

# plot
ar_global_fig2.1 <- ggplot() +
  geom_sf(data = ar_global_fig2.1_data, aes(fill = monitor_density_bucket), color = "cornsilk4", lwd = 0.3) +
  geom_sf(data = country_shape_file_china_wv, color = "cornsilk4", fill = "transparent", lwd = 0.3) +
  # geom_sf(data = china_gadm0, color = "cornsilk4", fill = "white", lwd = 0.3) +
  ggthemes::theme_map() +
  scale_fill_manual(values = monitor_colors,
                    na.value = "grey90", drop = FALSE) +
  labs(fill = "Monitoring Density\n(per million people)") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.direction = "horizontal") 

##### Figure 2.3 #####
public_data <- read_csv("~/Desktop/AQLI/2025 AQLI Update/data/open_data.csv")
awardees <- read_excel("~/Desktop/AQLI/2025 AQLI Update/data/Awardee Info.xlsx", skip = 2)

# figure 2.3 data
ar_global_fig2.3_data <- awardees %>%
  rename("country" = "Country in which project is being executed") %>%
  mutate(country = ifelse(country == "Cote d'Ivoire", "Côte d'Ivoire", country),
         country = ifelse(country == "The Gambia", "Gambia", country)) %>%
  group_by(country) %>%
  summarise(num_awards = n()) %>%
  right_join(public_data, by = c("country" = "Country or Dependency")) %>%
  inner_join(country_shape_file_china_wv, by = c("country" = "name0")) %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# plot
ar_global_fig2.3 <- ggplot() +
  geom_sf(data = ar_global_fig2.3_data, 
          mapping = aes(fill = `Is there any evidence of current government operated AQ monitoring system in 2024?`), 
          color = "white", 
          lwd = 0.3) + 
  geom_sf(data = country_shape_file_china_wv, 
          color = "cornsilk4", 
          fill = "transparent", 
          lwd = 0.3) +
  # geom_sf(data = china_gadm0, color = "cornsilk4", fill = "#5e92a9", lwd = 0.3) +
  geom_sf(data = filter(ar_global_fig2.3_data, !is.na(num_awards)),
          fill = NA,            
          color = "#fed976",     
          lwd = 0.3,           
          show.legend = FALSE) +
  ggthemes::theme_map() +
  scale_fill_manual(name = "Is there any evidence of current government operated AQ monitoring system in 2024?",
                    values = c("No" = "#800026",
                               "Yes" = "#5e92a9")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key = element_rect(color = "black"),
        legend.box.margin = margin(b = 1, unit = "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.direction = "horizontal") 
