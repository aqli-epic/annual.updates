# read in the helper file
source("./appPublic/aqli.data.explorer.helper.script.R")


# central africa definition
central_african_countries <- c("Angola", "Burundi", "Cameroon", 
                                        "Central African Republic", "Chad", 
                                        "Republic of the Congo",
                                        "Democratic Republic of the Congo", 
                                        "Equatorial Guinea", "Gabon",
                                        "São Tomé and Príncipe", 
                                        "Rwanda")
# west africa definition
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde", 
                            "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                            "Côte d'Ivoire", "Liberia", "Mali", "Mauritania", 
                            "Niger", "Nigeria", "Senegal", "Sierra Leone", 
                            "Togo")

# central and west africa countries definition, combine in a single vector
central_and_west_african_countries <- c(central_african_countries, west_african_countries)

# central and west african countries
gadm2_aqli_2021_cw_african <- gadm2_aqli_2021 %>%
  filter(country %in% central_and_west_african_countries) %>%
  mutate(name_1_country = str_c(name_1, " (", country, ")", sep = ""), 
         region = ifelse(country %in% central_african_countries, "Central African", 
                         "West African")) 


#> cwafrica fig2

# GBD results filtered for relevant cause of death and countries 
gbd_results_cwafrica_fig2 <- gbd_results_master_2021 %>%
  filter(cause_of_death %in% c("PM2.5 relative to WHO guideline",
                               "Neglected tropical diseases and malaria", "Unsafe water, sanitation, and handwashing", 
                               "HIV/AIDS and sexually transmitted infections"), country %in% c("Nigeria", "Democratic Republic of the Congo", "Angola", "Ghana", "Cameroon"))

# making the 'location' column of type factor
gbd_results_cwafrica_fig2$country <- factor(gbd_results_cwafrica_fig2$country, 
                                        levels = c("Nigeria", "Democratic Republic of the Congo", "Angola", "Ghana", "Cameroon"))

# Rename Democratic Republic of the Congo to DRC
gbd_results_cwafrica_fig2$country <-  str_replace(gbd_results_cwafrica_fig2$country, 
            "Democratic Republic of the Congo", "DR Congo")

# Converting 'cause_of_death' to type factor
gbd_results_cwafrica_fig2$cause_of_death <- as.factor(gbd_results_cwafrica_fig2$cause_of_death)

# Rearranging 'cause of death' levels
levels(gbd_results_cwafrica_fig2$cause_of_death) <- c( "HIV/AIDS and sexually transmitted infections", "Neglected tropical diseases and malaria", "PM2.5 relative to WHO guideline", "Unsafe water, sanitation, and handwashing")

# wrapping x-axis labels text 
levels(gbd_results_cwafrica_fig2$cause_of_death) <- str_wrap(levels(gbd_results_cwafrica_fig2$cause_of_death), 30)

# getting country wise population
country_wise_population <- gadm2_aqli_2021_cw_african %>%
  gadm_level_summary(c("country"), c(2021), 10) %>%
  filter(country %in% c("Cameroon", "Nigeria", "Angola", "Ghana", 
                  "Democratic Republic of the Congo")) %>%
  arrange(desc(population))

# reorder within each location as per the total life years lost column
gbd_results_cwafrica_fig2 <- gbd_results_cwafrica_fig2 %>% 
  mutate(cause_of_death = reorder_within(cause_of_death, lyl, country))

# clean the "cause of death" column and save the cwafrica factsheet figure 2
cwafrica_fs_fig2_dataset <- gbd_results_cwafrica_fig2 %>%
  mutate(cause_of_death = str_remove(cause_of_death, "___.+"))


# plot 
plt <- cwafrica_fs_fig2_dataset %>%
  ggplot(mapping = aes(x = reorder_within(cause_of_death, lyl, country), y = lyl)) + 
  geom_col(mapping = aes(fill = cause_of_death), width = 0.5, color = "white") +
  scale_x_reordered() +
  facet_wrap(~factor(country, levels = c("Angola", "Cameroon", "DR Congo", 
                  "Ghana", "Nigeria")), scales = "free_x", ncol = 5) +
  scale_fill_manual(values = c("#D3D9E0", "#7BC1D9", "#8F3931",
                               "#808A94")) +
  labs(x = "Threats to Life Expectancy", y = "Life Years Lost", title = "", 
       subtitle = "", fill = "Threats to Life Expectancy") +
  themes_aqli_base +
   theme(axis.text.x = element_blank(), legend.position = "bottom", axis.ticks = element_blank(), 
         strip.text = element_text(size = 14), 
         plot.background = element_rect(fill = "white", color = "white")) 


