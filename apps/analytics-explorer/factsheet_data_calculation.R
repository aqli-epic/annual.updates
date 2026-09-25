###############################
######### Pipe Line ###########

library(arrow)
library(data.table)
library(tidyverse)
gadm2_aqli <- as.data.table(arrow::read_parquet("./data/gadm2_aqli_2024.parquet")) 
gadm1_aqli <- as.data.table(arrow::read_parquet("./data/gadm1_aqli_2024.parquet"))
gadm0_aqli <- as.data.table(arrow::read_parquet("./data/gadm0_aqli_2024.parquet")) 

country_lyl = gadm2_aqli %>% filter(region =="Europe") %>% filter(country %in% europe_countries)%>% arrange(desc(population)) %>% slice_head(n=7) %>% 
  select( "region", "country", "population", "natstandard", "pm2024", "llpp_who_2024", "llpp_nat_2024")

cbind(country_lyl$country, country_lyl$llpp_who_2024)

# for ( i in country_lyl$country) {
#   print(i)
#   top2_name2_each_country_sea <- gadm2_aqli %>%
#     filter(country == i) %>%
#     arrange(desc(pm2024)) %>%
#      mutate(
#       pop_not_met_nat = ifelse(pm2024 > natstandard, population, 0),
#       pop_met_nat     = ifelse(pm2024 <= natstandard, population, 0)
#     ) %>% group_by(country, natstandard)  %>% summarise(
#        perc_count_with_nat  = round(sum(pop_met_nat, na.rm = T)/sum(population, na.rm = T),2),
#        perc_count_without_nat = round(sum(pop_not_met_nat, na.rm = T)/sum(population, na.rm = T),2)
#       )
#   
#   top2_district_area <- gadm2_aqli %>%
#     filter(country == "Myanmar") %>%
#     group_by(country, name_1, name_2) %>%
#     arrange(desc(pm2024))  %>% select( country,name_1, name_2,  natstandard,
#                                            pm2024, llpp_who_2024) %>% 
#     head(n = 2)
#     
#   print(top2_name2_each_country_sea)
#   print(top2_district_area)
#   
# }


region_name = "Europe"
top2_name2_each_country_sea <- gadm2_aqli %>%
  filter(region == region_name) %>%
  mutate(
    pop_not_met_nat = ifelse(pm2024 > natstandard, population, 0),
    pop_met_nat     = ifelse(pm2024 <= natstandard, population, 0)
  ) %>%
  group_by(country, natstandard) %>%
  summarise(
    total_population         = sum(population, na.rm = TRUE),
    pop_met_nat_total        = sum(pop_met_nat, na.rm = TRUE),
    pop_not_met_nat_total    = sum(pop_not_met_nat, na.rm = TRUE),
    perc_count_with_nat      = round(pop_met_nat_total / total_population, 2),
    perc_count_without_nat   = round(pop_not_met_nat_total / total_population, 2),
    .groups = "drop"
  )

top2_name2_each_country_sea <- top2_name2_each_country_sea %>% select(-c("pop_met_nat_total", "pop_not_met_nat_total"))
county_llpp <- gadm0_aqli %>%   filter(region == region_name) %>% select(country, llpp_who_2024)
top2_name2_each_country_sea <- left_join(county_llpp,top2_name2_each_country_sea, by = "country")
# tot_lyl <- sum(top2_name2_each_country_sea$llpp_who_2024, na.rm = T)
# top2_name2_each_country_sea$tot_lyl <- tot_lyl
# top2_name2_each_country_sea <- top2_name2_each_country_sea %>% mutate(perc_lyl = llpp_who_2024*100/tot_lyl)


gadm_aqli_perc <- gadm0_aqli %>% select("region", "country" ,"population", "pm2024", "llpp_who_2024") %>% filter(region == region_name)

gadm_aqli_perc$tot_lyl_gain <- gadm_aqli_perc$population*gadm_aqli_perc$llpp_who_2024
gadm_aqli_perc$tot_gain     <- sum(gadm_aqli_perc$tot_lyl_gain, na.rm = T)
gadm_aqli_perc              <- gadm_aqli_perc %>% mutate(perc_lyl = round(tot_lyl_gain*100/tot_gain, 2))

####################################################

####################################################
top2_district_area <- gadm2_aqli %>%
  filter(region == region_name) %>%
  group_by(country) %>%
  arrange(desc(pm2024), .by_group = TRUE) %>%
  slice_head(n = 2) %>%
  select(country, name_1, name_2, natstandard, pm2024, llpp_who_2024) %>%
  ungroup()

#####################################
value_cols <- grep(
  "^(pm\\d{4}|llpp_who_\\d{4}|llpp_nat_\\d{4})$",
  names(gadm0_aqli),
  value = TRUE
)

gadm0_aqli$region[gadm0_aqli$country=="China"] <- "China"
# gadm0_aqli$region[gadm0_aqli$country=="Canada"] <- "Canada"
# gadm0_aqli$region[gadm0_aqli$country=="United States"] <- "United States"

gadm0_aqli$global <- "Global"
region_aqli <- gadm0_aqli %>%
  group_by(region,
           whostandard) %>%
  summarise(
    across(
      all_of(value_cols),
      ~ round(
        sum(.x * population, na.rm = TRUE) /
          sum(population, na.rm = TRUE),
        2
      )
    ),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  relocate(
    region,
    population, whostandard,
    .before = 1
  )

gadm_aqli_region <- region_aqli %>%
  ungroup() %>%
  mutate(
    across(starts_with("pm"),
           ~if_else(.x > whostandard, round(0.098*(.x - whostandard), 2), 0),
           .names = "llpp_who_{substr(.col, 3, 6)}"))

gadm_aqli_sel <- gadm_aqli_region %>% select("region", "population", "pm2024", "pm2005", "llpp_who_2024", "llpp_who_2005")

aqli <- c("Central and West Africa", "China", "Europe", "Latin America",
          "Middle East and North Africa", "South Asia",                 
          "South East Asia", "US & Canada")

#gadm_aqli_sel$totol_lyl <- gadm_aqli_sel$population*gadm_aqli_sel$llpp_who_2024


gadm_aqli_sel <- gadm_aqli_sel %>%
  mutate(
    region_lyl = population * llpp_who_2024,
    perc_lyl   = round(region_lyl*100 / sum(region_lyl, na.rm = TRUE),2)
  )

gadm_aqli_sel <- gadm_aqli_sel %>% filter(region %in% aqli) %>% select(region, llpp_who_2024, perc_lyl)

final_col <- gadm_aqli_sel %>% select(region, llpp_who_2024, perc_lyl)
final_col <-  final_col %>% filter( !region %in% c("Oceania", "Other"))
###############

china_update <- gadm0_aqli %>% select(country, population, llpp_who_2024, pm2024) %>% 
                                      mutate(
                                      tot_country_lyl = llpp_who_2024*population
                                          )

china_update$tot_lyl <- sum(china_update$tot_country_lyl, na.rm = T)
china_update <- china_update %>% mutate(perc_lyl = round(tot_country_lyl*100/tot_lyl,2))
#####################################################


library(ggplot2)
library(dplyr)

df <- gadm0_aqli %>% select(country, starts_with("llpp_who")) %>% filter(country %in% c("Bangladesh", "Bhutan", "India", 
                                                                              "Nepal", "Pakistan", "Sri Lanka"))



df <- df %>% pivot_longer(!country, names_to = "year", values_to = "lyl")
df$year <- gsub("[^0-9]", "",df$year)
df$year <- as.integer(df$year)

# Make sure country is factor (controls legend order)




df$country <- factor(
  df$country,
  levels = c("Bangladesh", "Bhutan", "India", "Nepal", "Pakistan", "Sri Lanka")
)




plot_lyl_trend <- function(data,
                           x_break_start = 1998,
                           x_break_end   = 2024,
                           x_by = 2) {
  
  # Ensure country is factor
  data$country <- factor(data$country)
  
  n_countries <- length(unique(data$country))
  
  # Dynamic shapes
  shapes <- rep(c(16, 18, 15, 17, 25, 19, 8, 3),
                length.out = n_countries)
  
  # Dynamic Y limit
  y_max <- ceiling(max(data$lyl, na.rm = TRUE))
  
  ggplot(data, aes(x = year, y = lyl,
                   color = country,
                   shape = country)) +
    
    geom_line(linewidth = 1.4) +
    geom_point(size = 3.5) +
    
    scale_color_manual(
      values = get_aqli_palette(n_countries)
    ) +
    
    scale_shape_manual(values = shapes) +
    
    scale_x_continuous(
      breaks = seq(x_break_start, x_break_end, by = x_by)
    ) +
    
    scale_y_continuous(
      limits = c(0, y_max),
      breaks = seq(0, y_max, by = 2),
      expand = c(0, 0)
    ) +
    
    labs(
      x = "Year",
      y = "Potential Gain in Life Expectancy (Years)",
      color = "Country",
      shape = "Country"
    ) +
    
    guides(
      color = guide_legend(nrow = 1, byrow = TRUE),
      shape = guide_legend(nrow = 1, byrow = TRUE)
    ) +
    
    ggthemes::theme_clean() +
    
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 18, color = "#222222"),
      
      axis.title = element_text(size = 18, color = "#222222"),
      axis.title.y = element_text(margin = ggplot2::margin(r = 18)),
      axis.title.x = element_text(margin = ggplot2::margin(t = 18)),
      
      axis.text = element_text(size = 18, color = "#222222"),
      axis.line = element_line(),
      axis.ticks = element_blank(),
      
      legend.box.background = element_rect(color = "black", linewidth = 0.5),
      
      plot.background = element_rect(fill = "white", color = "white")
    )
}


get_aqli_palette <- function(n) {
  base_palette <- c(
    "#3B5B92",
    "#D2693C",
    "#6FA89B",
    "#E6A52E",
    "#7E8A9A",
    "#7A4F45"
  )
  
  if (n <= length(base_palette)) {
    return(base_palette[1:n])
  }
  
  extra_colors <- scales::hue_pal(l = 55, c = 80)(n - length(base_palette))
  
  c(base_palette, extra_colors)
}

plot = plot_lyl_trend(df)


ggsave(
  filename = "~/Desktop/lyl_trend.png",
  plot = plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

###################
europe_countries <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                      "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
                      "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", 
                      "Finland", "France", "Georgia", "Germany", "Greece", 
                      "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", 
                      "Kosovo", "Latvia", "Liechtenstein", "Lithuania", 
                      "Luxembourg", "Macedonia", "Malta", "Moldova", "Monaco", 
                      "Montenegro", "Netherlands", "Northern Cyprus", "Norway", 
                      "Poland", "Portugal", "Romania", "Russia", "San Marino", 
                      "Serbia", "Slovakia", "Slovenia", "Spain", 
                      "Svalbard and Jan Mayen", "Sweden", "Switzerland", "Turkey", 
                      "Ukraine", "United Kingdom", "Vatican City")


country <- gadm0_aqli %>% filter(country==europe_countries) %>% select(country, population,pm2005, pm2024, llpp_who_2005 ,llpp_who_2024)
country <- country  %>% 
  mutate(
    tot_country_lyl = llpp_who_2024*population
  )


country$tot_lyl <- sum(country$tot_country_lyl, na.rm = T)
country <- country %>% mutate(perc_lyl = round(tot_country_lyl*100/tot_lyl,2))

state_county <- country %>% filter(name_2 %in% prd_cities)
#  Beijing-Tianjin-Hebei

prd_cities <- c(
  "Guangzhou",
  "Shenzhen",
  "Dongguan",
  "Foshan",
  "Zhuhai",
  "Zhongshan",
  "Jiangmen",
  "Huizhou",
  "Zhaoqing"
)

europe <- gadm0_aqli %>% filter(region=="Europe") %>% select(region, country, population, pm1998, pm2024) %>% mutate(diff_98_24 = (pm2024-pm1998)*100/pm1998)


