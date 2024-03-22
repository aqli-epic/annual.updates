# read in the helper file
source("R/july.2024.helper.script.R")

# read and filter AQLI data
ne_thai <- c("Amnat Charoen", "Bueng Kan", "Buri Ram", "Chaiyaphum", "Kalasin", "Khon Kaen", "Loei",
             "Maha Sarakham", "Mukdahan", "Nakhon Phanom", "Nakhon Ratchasima", "Nong Bua Lam Phu",
             "Nong Khai", "Roi Et", "Sakon Nakhon", "Si Sa Ket", "Surin", "Ubon Ratchathani",
             "Udon Thani", "Yasothon")

n_thai <- c ("Chiang Mai", "Chiang Rai", "Lampang", "Lamphun", "Mae Hong Son", "Nan", "Phayao", "Phrae",
             "Uttaradit", "Tak", "Kamphaeng Phet", "Phetchabun", "Phichit", "Phitsanulok", "Sukhothai",
             "Nakhon Sawan", "Uthai Thani")

c_thai <- c("Ang Thong", "Bangkok Metropolis", "Chai Nat", "Lop Buri", "Nakhon Pathom", "Nonthaburi", "Pathum Thani",
            "Phra Nakhon Si Ayutthaya", "Samut Prakan", "Samut Sakhon", "Samut Songkhram", "Saraburi",
            "Sing Buri", "Suphan Buri", "Nakhon Nayok", "Chachoengsao", "Chanthaburi", "Chon Buri",
            "Prachin Buri", "Rayong", "Sa Kaeo", "Trat", "Kanchanaburi", "Ratchaburi", "Phetchaburi", "Prachuap Khiri Khan")

s_thai <- c("Chumphon", "Nakhon Si Thammarat", "Narathiwat", "Pattani", "Phatthalung", "Songkhla", "Surat Thani",
            "Yala", "Krabi", "Phangnga", "Phuket", "Ranong", "Satun", "Trang")

thai_aqli_2022 <- gadm2_aqli_2022 %>%
  filter(country == "Thailand") %>%
  mutate(region = case_when(
    name_1 %in% ne_thai ~ "Northeastern",
    name_1 %in% n_thai ~ "Northern",
    name_1 %in% c_thai ~ "Central",
    name_1 %in% s_thai ~ "Southern"))

# thailand factsheet figure 4 dataset
thailand_fs_fig4_regions <- thai_aqli_2022 %>%
  group_by(region) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+")))) %>%
  select(years, region, pop_weighted_avg_pm2.5)

thailand_fs_fig4_nat <- gadm2_aqli_2022 %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         mutate(across(starts_with("pm"), ~.x*pop_weights, .names = "{col}_weighted"))) %>%
  summarise(across(ends_with("weighted"), sum)) %>%
  pivot_longer(cols = pm1998_weighted:pm2022_weighted, names_to = "years",
               values_to = "pop_weighted_avg_pm2.5") %>%
  mutate(years = as.integer(unlist(str_extract(years, "\\d+"))),
         region = "National Average") %>%
  select(years, region, pop_weighted_avg_pm2.5)

thailand_fs_fig4_dataset <- rbind(thailand_fs_fig4_regions, thailand_fs_fig4_nat)

thailand_fs_fig4_dataset$region = factor(thailand_fs_fig4_dataset$region, levels = c('National Average', 'Central', 'Northeastern', 'Northern', 'Southern'))

# thailand factsheet figure 4
thailand_fs_fig4 <- thailand_fs_fig4_dataset %>%
  ggplot() +
  geom_line(mapping = aes(x = as.integer(years),
                          y = as.double(pop_weighted_avg_pm2.5),
                          colour = region, linetype = region), lwd = 1.1) +
  geom_hline(mapping = aes(yintercept = 5), lwd = 0.8, linetype = "dotted") +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 35)) +
  scale_x_continuous(breaks = c(seq(1998, 2019, 3), 2022))  +
  scale_color_manual(values = c("National Average" = "#7197be", "Northeastern" = "#7197be", "Northern" = "#5f7aa5", "Central" = "#7197be", "Southern" = "#CBE8F3")) +
  scale_linetype_manual(values = c("National Average" = "solid", "Northeastern" = "dashed", "Northern" = "dashed", "Central" = "dashed", "Southern" = "dashed")) +
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
  geom_text(x = 2001.15, y = 5.8, label = expression("WHO" ~ PM[2.5] ~ "Guideline (last updated: 2022): 5 µg/m³"), size = 5)
