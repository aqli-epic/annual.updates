# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


#> plot 6: Distribution of pollution in 2021-----------------------
  
col_name <- "pm2021"


plt <- gadm2_aqli_2021 %>%
      filter(country == "Colombia") %>%
      add_aqli_color_scale_buckets(scale_type = "pollution", col_name = "pm2021") %>%
      ggplot() +
      geom_histogram(mapping = aes(x = !!as.symbol(col_name), weight = population, fill = !!as.symbol(col_name), group = !!as.symbol(col_name))) +
  geom_vline(mapping = aes(xintercept = 5), linetype = "dotted") +
  geom_vline(mapping = aes(xintercept = 20), linetype = "dotted") +
      scale_fill_gradient(low = "#a1f5ff",
                          high = "#1a1638") +
  scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  labs(x = expression("Annual Average" ~ PM[2.5] ~ " Concentration (in µg/m³)"), 
       y = "Number of people", 
       fill = expression("Annual Average" ~ PM[2.5] ~ " concentration (in µg/m³)")) +
  themes_aqli_base +
  geom_text(x = 7, y = 10000000, label =  expression("WHO" ~ PM[2.5] ~ "guideline"), size = 5) +
    geom_text(x = 22, y = 10000000, label =  expression("National" ~ PM[2.5] ~ "standard"), size = 5) +
  theme(plot.background = element_rect(color = "white", fill = "white"))



