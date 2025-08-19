# read in the helper file
source("R/july.2024.helper.script.R")

# Figure 3: Comparison of selected major global threats to life expectancy in the 5 most populous countries in Europe

# create a europe AQLI dataset
gadm2_aqli_2022_europe <- gadm2_aqli_2022 %>%
  filter(country %in% european_countries$Country)


# europe pop
europe_pop <- sum(gadm2_aqli_2022_europe$population, na.rm =  TRUE)

# pop living above who guideline
europe_above_who <- gadm2_aqli_2022_europe %>%
  filter(pm2022 > 10) %>%
  summarise(tot_pop = sum(population, na.rm = TRUE))


# country wise average and how many are above WHO
europe_country_wise <- gadm2_aqli_2022_europe %>%
  gadm_level_summary(c("country"), c(2022), 10)


# exclude the following countries to keep the map less wide and to show a stark difference between eastern and western europe
exclude_countries <-  c("Russia", "Turkey", "Sweden", "Finland", "Norway", "Kazakhstan", "Iceland", "Georgia", "Azerbaijan", "Armenia", "Cyprus", "Northern Cyprus", "Svalbard and Jan Mayen")


countries_except_excluded <- european_countries %>%
  filter(country %notin% exclude_countries)




#> europe fig3

diseases_list <- c("PM2.5 relative to WHO guideline",
                               "Diabetes and kidney diseases","HIV/AIDS and sexually transmitted infections","Tobacco", "Transport injuries")

country_list <- c("Russian Federation", "Turkey", "Germany", "United Kingdom", "France")

# GBD results filtered for relevant cause of death (given Europe) and countries
gbd_results_europe_fig3 <- gbd_results_master_2022 %>%
 filter(cause_of_death %in% diseases_list, country %in% country_list)

# making the 'location' column of type factor
gbd_results_europe_fig3$country <- factor(gbd_results_europe_fig3$country, levels = country_list)



# Converting 'cause_of_death' to type factor
gbd_results_europe_fig3$cause_of_death <- factor(gbd_results_europe_fig3$cause_of_death, levels = diseases_list)

# wrapping x-axis labels text
levels(gbd_results_europe_fig3$cause_of_death) <- str_wrap(levels(gbd_results_europe_fig3$cause_of_death), 30)


# reorder within each location as per the total life years lost column
gbd_results_europe_fig3 <- gbd_results_europe_fig3 %>%
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column and save the cwafrica factsheet figure 2
gbd_results_europe_fig3 <- gbd_results_europe_fig3 %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))



# plot
europe_fs_fig3 <- gbd_results_europe_fig3 %>%
  ggplot(mapping = aes(x = reorder_within(cause_of_death, lyl, country), y = lyl)) +
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  facet_wrap(~factor(country, levels = country_list), scales = "free_x", ncol = 5) +
   scale_fill_manual(values = c("#B4CDD9", "#707F8C", "#8F3931",
                                "#ADE9FA", "#CBE8F3")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "",
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
   theme(legend.position = "bottom",
         legend.justification = c(0.5, 3),
         legend.background = element_rect(color = "black"),
         legend.text = element_text(size = 15, color="#222222"),
         axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color="#222222"),
         axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color="#222222"),
         legend.title = element_text(size = 20, color="#222222"),axis.text.x = element_blank(),
         axis.ticks = element_blank(),
         strip.text = element_text(size = 20),
         plot.background = element_rect(fill = "white", color = "white"))

