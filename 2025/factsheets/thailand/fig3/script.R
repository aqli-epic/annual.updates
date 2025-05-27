# read in the helper file
source("~/R/july.2025.helper.script.R")

#Figure 3: Potential gain in life expectancy from reducing PM2.5 from 2022 levels to the WHO guideline in 10 most populous provinces of Thailand

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

thai_aqli_2023 <- gadm2_aqli_2023 %>%
  filter(country == "Thailand") %>%
  mutate(region = case_when(
    name_1 %in% ne_thai ~ "Northeastern",
    name_1 %in% n_thai ~ "Northern",
    name_1 %in% c_thai ~ "Central",
    name_1 %in% s_thai ~ "Southern"))

# thailand fs fig 3 data
thailand_fs_fig3_dataset <- thai_aqli_2023 %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2023', 'llpp_who_2023') %>%
  group_by(name_1) %>%
  mutate(pop_weights = population/sum(population, na.rm = TRUE),
         pm2023_pop_weighted = pop_weights*pm2023,
         llpp_who_2023_pop_weighted = pop_weights*llpp_who_2023) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE),
            avg_pm2.5_2023 = sum(pm2023_pop_weighted, na.rm = TRUE),
            le_gain = (avg_pm2.5_2023 - who_guideline)*le_constant,
            llpp_who_2023 = sum(llpp_who_2023_pop_weighted, na.rm = TRUE),
            le_gain = ifelse(le_gain < 0 , 0, le_gain)) %>%
  slice_max(tot_pop, n = 10) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2023")

# thailand fs figure 3
thailand_fs_fig3 <- thailand_fs_fig3_dataset %>%
  ggplot() +
  geom_col(mapping = aes(x = reorder(name_1, llpp_who_2023), y = llpp_who_2023, fill = lyl_bucket), width = 0.5) +
  labs(x = "Province", y = "Potential Gain in Life Expectancy (Years)", fill = "Potential gain in life expectancy (Years)") +
  scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0, 3)) +
  scale_fill_manual(values = c("0 to < 0.1" = "#FFFFFF", 
                               "0.1 to < 0.5" = "#FFF2E1", 
                               "0.5 to < 1" = "#FFEDD3", 
                               "1 to < 2" = "#FFC97A", 
                               "2 to < 3" = "#FFA521", 
                               "3 to < 4" = "#EB6C2A", 
                               "4 to < 5" = "#D63333", 
                               "5 to < 6" = "#8E2946", 
                               ">= 6" = "#451F59")) +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 24, color="#222222"),
        legend.title = element_text(size = 24, color="#222222"),
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust =  0.5, size = 10, face = "italic", margin = margin(b = 0.8, unit = "cm")),
        plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
        legend.box.background = element_rect(color = "black"),
        plot.background = element_rect(color = "white"),
        axis.line = element_line(),
        axis.text = element_text(size = 20, color="#222222"),
        axis.title = element_text(size = 24, color="#222222"),
        axis.title.y = element_text(margin = margin(r = 0.7, unit = "cm")),
        axis.title.x = element_text(margin = margin(t = 0.6, b = 0.6, unit = "cm")),
        axis.ticks = element_blank())