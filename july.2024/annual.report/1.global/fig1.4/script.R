# read in the helper file
source("R/july.2024.helper.script.R")

# Global section figure 1.4 ============
# Figures 1.4 a,b and c were made in GIS using the data generated below
# Figure 1.4a data 
ar_global_fig1.4a_data <- gadm0_aqli_2022 %>%
  select(iso_alpha3, country, natstandard) %>%
  mutate(aq_std_bucket = if_else(natstandard >= 5 & natstandard <= 10, "5 - 10", NA),
         aq_std_bucket = if_else(natstandard > 10 & natstandard <= 20, "10 - 20", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 20 & natstandard <= 30, "20 - 30", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 30 & natstandard <= 40, "30 - 40", aq_std_bucket),
         aq_std_bucket = if_else(natstandard > 40 & natstandard <= 50, "40 - 50", aq_std_bucket, missing = "Does not have a standard"))

# Figure 1.4b and 1.4c data
ar_global_fig1.4bc_data <- gadm2_aqli_2022 %>%
  select(objectid_gadm2, iso_alpha3, country, name_1, name_2, natstandard, pm2022, llpp_who_2022, llpp_nat_2022) %>%
  add_aqli_color_scale_buckets("lyl", "llpp_nat_2022") %>%
  mutate(`Meets national standard` = if_else(pm2022 > natstandard, "No", "Yes", missing="Does not have a standard"),
         lyl_bucket = if_else(`Meets national standard` == "Does not have a standard", "Does not have a standard", lyl_bucket))

