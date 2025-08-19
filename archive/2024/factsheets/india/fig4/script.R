# read in the helper file
source("R/july_2024_helper_script.R")

# Figure 4: Average PM2.5 concentration in India from 1998 to 2022 --------------

# adding a north India/rest of india region column (specifically Indo gangetic plain)
north_india <- c("West Bengal", "Uttar Pradesh", "Punjab", "Haryana", 
                 "Chandigarh", "Bihar", "NCT of Delhi")

india_fs_fig4_data_part1 <- gadm2_aqli_2022 %>%
  filter(country == "India", !is.na(population)) %>%
  mutate(region = ifelse(name_1 %in% north_india, "Northern Plains of India", "All other regions (excluding Northern Plains of India)"))


# creating India's region wise average PM2.5 data from 1998 to 2022 
india_fs_fig4_data_part1 <- india_fs_fig4_data_part1 %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)

# creating a national average PM2.5 trendlines data from 1998 to 2022
india_fs_fig4_data_part2 <- gadm2_aqli_2022 %>%
  filter(country == "India") %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE), 
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years", 
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))), 
         region = "National Average") %>% 
  select(years, region, pop_weighted_avg_pm2.5)

# India factsheet figure 4 dataset
india_fs_fig4_data <- rbind(india_fs_fig4_data_part1, india_fs_fig4_data_part2)

# India factsheet figure 4
# fig4 caption removed: "We define the Indo-Gangetic plain region as containing 
# the following seven states and union territories: Bihar, Chandigarh, NCT of Delhi, 
# Haryana, Punjab, Uttar Pradesh, and West Bengal."

india_fs_fig4_data$region <- factor(india_fs_fig4_data$region, 
                                    levels = c("Northern Plains of India", 
                                               "National Average", 
                                               "All other regions (excluding Northern Plains of India)"))

india_fs_fig4 <- ggplot(india_fs_fig4_data) +
  geom_line(mapping = aes(x = years, y = pop_weighted_avg_pm2.5, 
                          color = interaction(region), 
                          linetype = interaction(region)), lwd = 1.3) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted", color = "lightgrey") +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2022)) +
  scale_color_manual(values = c("Northern Plains of India" = "#1a1638", 
                                "National Average" =  "#3c456f", 
                                "All other regions (excluding Northern Plains of India)" = "#4e5e8b"), 
                     name = "legend") +
  scale_linetype_manual(values = c("Northern Plains of India" = "dashed", 
                                   "National Average" = "solid", 
                                   "All other regions (excluding Northern Plains of India)" = "dotted"), 
                        name = "legend")  +
  ggthemes::theme_tufte() +
  labs(x = "Year", 
       y = expression("Annual Average   " ~ PM[2.5] ~ " Concentration (in µg/m³)"), 
       title = "") +
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
        axis.ticks = element_blank(), 
        legend.key.width = unit(2, "cm")) +
     geom_text(x = 2003.5, y = 7.6, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2022): 5 µg/m³"), size = 4.5) 

