# read in the helper file
source("C:/Users/Aarsh/Downloads/annual.updates/R/july.2024.helper.script.R")

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

# thailand fs fig 1 data
thailand_fs_fig1_dataset <- thai_aqli_2022 %>%
  left_join(gadm2_aqli_2022_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
  select('country', 'name_1', 'name_2', 'population', 'pm2022', 'llpp_who_2022', 'geometry') %>%
  add_aqli_color_scale_buckets("lyl", "llpp_who_2022") %>%
  select(-geometry, geometry) %>%
  st_as_sf()

# thailand fs figure 1
thailand_fs_fig1 <- thailand_fs_fig1_dataset %>%
  ggplot() +
  geom_sf(mapping = aes(fill = reorder(lyl_bucket, order_lyl_bucket)), color = "aliceblue", lwd = 0.05) +
  geom_sf(data = gadm1_aqli_2022_shp %>% filter(name0 == "Thailand"), color = "azure4", fill = "transparent", lwd = 0.15) +
  geom_sf(data = gadm0_aqli_2022_shp %>% filter(name0 == "Thailand"), color = "cornsilk4", fill = "transparent", lwd = 0.5) +
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
        legend.direction = "horizontal",
        plot.background = element_rect(fill = "white", color = "white")) +
  guides(fill = guide_legend(nrow = 1))
ggsave("C:/Users/Aarsh/Downloads/thailand_fs_fig1.png", thailand_fs_fig1, width = 15, height = 10)
