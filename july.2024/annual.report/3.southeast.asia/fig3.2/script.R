# read in the helper file
source("R/july.2024.helper.script.R")

# southeast asia definition
se_asia_def <-  c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia",
                  "Laos", "Malaysia", "Philippines", "Singapore", "Thailand",
                  "Vietnam")

# figure 3.2 ------------
# southeast asia figure 3.2 data
ar_se_asia_fig3.2_data <- gadm0_aqli_2022 %>% filter(country %in% c("Malaysia","Indonesia"))%>% select(country, pm2022,pm2019) %>%
  group_by(country)
# Assuming 'dat' is your original data frame
ar_se_asia_fig3.2_data <- data.frame(country = ar_se_asia_fig3.2_data$country,
                     pm2022 = ar_se_asia_fig3.2_data$pm2022,
                     pm2019 = ar_se_asia_fig3.2_data$pm2019)

ar_se_asia_fig3.2_data <- gather(ar_se_asia_fig3.2_data, Year, PM2.5, pm2022:pm2019)

# Change x-axis labels to "2022" and "2019"
ar_se_asia_fig3.2_data$Year <- recode(ar_se_asia_fig3.2_data$Year, pm2022 = "2022", pm2019 = "2019")

# southeast asia figure 3.2: 10 most populous regions
ar_se_asia_fig3.2 <- ggplot(ar_se_asia_fig3.2_data, aes(country, PM2.5, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_manual(labels = ~ stringr::str_wrap(.x, width = 30),
                    values = c("#fed976","#fd8d3c","#fc4e2a","#800026")) +
  labs(x = "Country", y = "Annual average PM2.5 concentration", title = "", subtitle = "", caption = "", fill = "Year") +
  themes_aqli_base +
  theme(plot.background = element_rect(color = "white", fill = "white"))


