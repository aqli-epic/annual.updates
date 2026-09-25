# ============================================================
# Author:        Purushottam Gupta
# Organization:  Air Quality Life Index (AQLI)
#                University of Chicago
# Email:         guptap@uchicago.edu
#
# Description:
# Factsheets
# ============================================================

data_reactive <- reactive({
   selected_country <- input$country_gis_fs  # Can be made dynamic with input$country if using Shiny

   # Ensure who_guideline is defined (example value, adjust as needed)
   who_guideline <- 5  # WHO PM2.5 guideline (µg/m³), replace with actual value or input
   # selected_country <- "India"
   
   # Filter data
   district_data <- gadm2_aqli_2023 %>% filter(country == selected_country)
   province_data <- gadm1_aqli_2023 %>% filter(country == selected_country)
   national_data <- gadm0_aqli_2023 %>% filter(country == selected_country)

   ## Most Populus:

   top_three_most_populus_province <- province_data %>%
     arrange(desc(population)) %>%
     slice_head(n = 3) %>%
     select(name_1, llpp_who_2023) %>%
     mutate(
       llpp_who_2023 = if_else(
         llpp_who_2023 < 1,
         paste0(round(llpp_who_2023 * 12, 1), " months"),
         paste0(round(llpp_who_2023, 1), " years")
       )
     )

   top_three_most_populus_district <- district_data %>%
     arrange(desc(population)) %>%
     slice_head(n = 3) %>%
     select(name_1, name_2, llpp_who_2023) %>%
     mutate(
       llpp_who_2023 = if_else(
         llpp_who_2023 < 1,
         paste0(round(llpp_who_2023 * 12, 1), " months"),
         paste0(round(llpp_who_2023, 1), " years")
       )
     )

   province_pm_gone_up_last_decade <- province_data %>%
     mutate(pm_diff_2013_2023 = round((pm2023 - pm2013)*100/pm2023),1) %>%
     filter(pm_diff_2013_2023 > 0) %>%
     arrange(desc(pm_diff_2013_2023)) %>% head(5) %>%
     select(name_1, pm2013, pm2023, pm_diff_2013_2023)

   # Convert llpp_nat_ columns to numeric, with error handling
   district_data <- district_data %>%
     mutate(across(starts_with("llpp_nat_"), ~ as.numeric(as.character(.x))))
   national_data <- national_data %>%
     mutate(across(starts_with("llpp_nat_"), ~ as.numeric(as.character(.x))))

   #national_pm25_standard <- as.integer(unique(national_data$natstandard))
   national_pm25_standard <- national_data %>% pull(natstandard)

   total_population <- sum(district_data$population, na.rm = TRUE)

   anl_avg_pm2.5 <- national_data %>% pull(pm2023)
   # WHO guideline summaries
   national_llpp_if_who_met <- round(national_data$llpp_who_2023, 1)
   total_life_years_gained_if_who_met <- district_data %>%
     filter(pm2023 > who_guideline) %>%
     summarise(total_lyl = round(sum(population * llpp_who_2023, na.rm = TRUE) / 1e6, 1)) %>%
     pull(total_lyl)

   percent_population_exposed_above_who <- district_data %>%
     filter(pm2023 > who_guideline) %>%
     summarise(percent = round(sum(population, na.rm = TRUE) * 100 / total_population, 1)) %>%
     pull(percent)

   # Top 3 most polluted districts
   top3 <- district_data %>% arrange(desc(pm2023)) %>% head(3) %>%
     mutate(llpp_who_2023 = ifelse(llpp_who_2023 < 1,
                                   paste0(round(llpp_who_2023 * 12), " months"),
                                   paste0(round(llpp_who_2023, 1), " years")))

   # Capital city (adjust for Germany, e.g., Berlin)
   capital_llpp_who <- district_data %>%
     filter(name_2 == "Berlin") %>%  # Adjust based on country
     select(name_2, llpp_who_2023) %>%
     mutate(llpp_who_2023 = ifelse(llpp_who_2023 < 1,
                                   paste0(round(llpp_who_2023 * 12), " months"),
                                   paste0(round(llpp_who_2023, 1), " years")))

   # National standard-based summaries
   adjusted_llpp_national_level <- national_data %>%
     mutate(adjusted_llpp = ifelse(llpp_nat_2023 < 1,
                                   paste0(round(llpp_nat_2023 * 12), " months"),
                                   paste0(round(llpp_nat_2023, 1), " years"))) %>%
     pull(adjusted_llpp)

   # percent_population_exposed_above_national <- district_data %>%
   #   filter(pm2023 > national_pm25_standard) %>% select(country, name_1,  name_2,population, whostandard, natstandard, pm2022, pm2023, llpp_who_2023)

   percent_population_exposed_above_national <- district_data %>%
     filter(pm2023 > national_pm25_standard) %>%
     summarise(percent = round(sum(population, na.rm = TRUE) * 100 / total_population, 1)) %>%
     pull(percent)

   total_life_years_lost_if_national_met <- district_data %>%
     filter(pm2023 > national_pm25_standard) %>%
     summarise(total_lyl = round(sum(population * llpp_nat_2023, na.rm = TRUE) / 1e6, 1)) %>%
     pull(total_lyl)

   # Top 2 polluted provinces (National standard)
   top2_polluted_provinces <- province_data %>%
     arrange(desc(pm2023)) %>%
     head(3)

   province_names <- top2_polluted_provinces$name_1
   top2_polluted_provinces$llpp_nat_2023 <- as.numeric(as.character(top2_polluted_provinces$llpp_nat_2023))
   province_llpp_nat <- top2_polluted_provinces %>%
     mutate(adjusted_llpp = ifelse(llpp_nat_2023 < 1,
                                   paste0(round(llpp_nat_2023 * 12), " months"),
                                   paste0(round(llpp_nat_2023, 2), " years"))) %>%
     pull(adjusted_llpp)

   # who
   province_llpp_who <- top2_polluted_provinces %>%
     mutate(adjusted_who_llpp = ifelse(llpp_who_2023 < 1,
                                       paste0(round(llpp_who_2023 * 12), " months"),
                                       paste0(round(llpp_who_2023, 2), " years"))) %>%
     pull(adjusted_who_llpp)

   # Life expectancy difference
   lyl1998 <- national_data %>% pull(llpp_who_1998)
   lyl2023 <- national_data %>% pull(llpp_who_2023)
   lyl_who_diff_1998_2023_num <- round(lyl2023 - lyl1998, 1)

   # Formatted version for display
   lyl_who_diff_1998_2023 <- if (abs(lyl_who_diff_1998_2023_num) < 1) {
     paste0(abs(round(lyl_who_diff_1998_2023_num * 12)), " months")
   } else {
     paste0(abs(lyl_who_diff_1998_2023_num), " years")
   }

   # PM2.5 percent difference
   pm1998 <- national_data %>% pull(pm1998)
   pm2023 <- national_data %>% pull(pm2023)
   pm_diff_1998_2023_num <- round((pm2023 - pm1998) * 100 / pm1998, 1)

   # Formatted version
   pm_diff_1998_2023 <- paste0(abs(pm_diff_1998_2023_num), "%")

   # Trend direction
   pm_trend <- if (pm_diff_1998_2023_num > 0) "increased" else if (pm_diff_1998_2023_num < 0) "decreasing" else "remained stable"
   lyl_trend <- if (lyl_who_diff_1998_2023_num > 0) "decreased" else if (lyl_who_diff_1998_2023_num < 0) "increased" else "remained stable"

   # GBD Ranking
   selected_causes <- c(
     "Ambient ozone pollution", "Child and maternal malnutrition", "Childhood sexual abuse and bullying",
     "Dietary risks", "Drug use", "HIV/AIDS and sexually transmitted infections", "High alcohol use",
     "Intimate partner violence", "Low physical activity", "Neglected tropical diseases and malaria",
     "Nutritional deficiencies", "Occupational risks", "PM2.5 relative to WHO guideline",
     "Self-harm and interpersonal violence", "Substance use disorders", "Tobacco", "Transport injuries",
     "Non-optimal temperature", "Other environmental risks", "Unintentional injuries",
     "Unsafe sex", "Unsafe water, sanitation, and handwashing"
   )

   ranked_data <- gbd_results_master_2025 %>%
     filter(country == selected_country, cause_of_death %in% selected_causes) %>%
     arrange(desc(lyl)) %>%
     mutate(rank = dense_rank(desc(lyl)))

   who_row <- ranked_data %>% filter(cause_of_death == "PM2.5 relative to WHO guideline")
   who_rank <- who_row$rank
   who_lyl <- who_row$lyl

   format_years_months <- function(lyl_val) {
     if (is.na(lyl_val)) return("N/A")
     if (lyl_val < 1) paste0(round(lyl_val * 12), " months") else paste0(round(lyl_val, 1), " years")
   }

   if (length(who_rank) > 0 && !is.na(who_rank)) {
     if (who_rank == 1) {
       comparison_causes <- ranked_data %>% filter(rank %in% c(2, 3))
     } else if (who_rank == 2) {
       comparison_causes <- ranked_data %>% filter(rank %in% c(1, 3))
     } else if (who_rank == 3) {
       comparison_causes <- ranked_data %>% filter(rank %in% c(who_rank - 1, who_rank - 2))
     } else {
       comparison_causes <- ranked_data %>% head(2)

     }

     cause1 <- comparison_causes$cause_of_death[1]
     loss1 <- format_years_months(comparison_causes$lyl[1])
     cause2 <- comparison_causes$cause_of_death[2]
     loss2 <- format_years_months(comparison_causes$lyl[2])

     who_loss <- format_years_months(who_lyl)

     threat_phrase <- if (who_rank == 1) {
       "greatest external health threat to life expectancy in the country followed by "
     } else if (who_rank == 2) {
       "second greatest external health threat after "
     } else {
       paste0(who_rank, "th greatest external health threat after ")
     }
   } else {
     cause1 <- cause2 <- loss1 <- loss2 <- who_loss <- threat_phrase <- "N/A"
   }

   list(
     country = selected_country,
     anl_avg_pm2.5 = anl_avg_pm2.5,
     national_pm25_standard= national_pm25_standard,
     national_llpp_if_who_met = national_llpp_if_who_met,
     total_life_years_gained_if_who_met = total_life_years_gained_if_who_met,
     percent_population_exposed_above_who = percent_population_exposed_above_who,
     top3 = top3,
     capital_llpp_who = capital_llpp_who,
     adjusted_llpp_national_level = adjusted_llpp_national_level,
     percent_population_exposed_above_national = percent_population_exposed_above_national,
     total_life_years_lost_if_national_met = total_life_years_lost_if_national_met,
     province_names = province_names,
     province_llpp_nat = province_llpp_nat,
     who_rank = who_rank,
     who_loss = who_loss,
     cause1 = cause1,
     loss1 = loss1,
     cause2 = cause2,
     loss2 = loss2,
     threat_phrase = threat_phrase,
     pm_diff_1998_2023 = pm_diff_1998_2023,
     lyl_who_diff_1998_2023 = lyl_who_diff_1998_2023,
     pm_trend = pm_trend,
     lyl_trend = lyl_trend,
     province_pm_gone_up_last_decade = province_pm_gone_up_last_decade,
     province_llpp_who = province_llpp_who,
     top_three_most_populus_district = top_three_most_populus_district,
     top_three_most_populus_province = top_three_most_populus_province
   )
 })


 output$summary_text <- renderUI({




   if (input$switch_over_btn_fs == "tblfs") {
     d <- data_reactive()

     top5_provinces_html <- if (exists("province_pm_gone_up_last_decade", d) && nrow(d$province_pm_gone_up_last_decade) > 0) {
       rows <- paste0(
         "<tr><td style='padding:6px;'>", d$province_pm_gone_up_last_decade$name_1[1:5], "</td>",
         "<td style='padding:6px;'><b>", round(d$province_pm_gone_up_last_decade$pm_diff_2013_2023[1:5], 1), "%</b></td></tr>",
         collapse = ""
       )
       paste0(
         "<b>Provinces with Increased PM<sub>2.5</sub> (2013–2023):</b>",
         "<table style='width:100%; border-collapse:collapse; margin-top:5px;'>",
         "<thead><tr style='background:#f2f2f2'><th style='text-align:left; padding:6px;'>Province</th><th style='text-align:left; padding:6px;'>% Increase</th></tr></thead>",
         "<tbody>", rows, "</tbody></table><br>"
       )
     } else {
       "<b>Provinces with Increased PM<sub>2.5</sub> (2013–2023):</b><br>None<br><br>"
     }

     HTML(paste0(
       "<div style='font-family: roboto, sans-serif; font-size:15px; line-height:1.6;'>",
       "<div class='responsive-two-column'>",

       # LEFT BLOCK: Air Quality Profile
       "<div>",
       "<div style='font-size: 25px; font-weight: bold; margin-bottom: 8px;'>Air Quality Profile: ", d$country, "</div>",
       "<table style='width:100%; border-collapse:collapse;'>",
       "<tr><td>PM<sub>2.5</sub> WHO Guideline</td><td><b>5 µg/m³</b></td></tr>",
       "<tr style='background:#f9f9f9;'><td>National Standard</td><td><b>", d$national_pm25_standard, " µg/m³</b></td></tr>",
       "<br>",
       "<tr><td colspan='2'><b>Exposure to PM<sub>2.5</sub></b></td></tr>",
       "<tr><td>Annual Average PM<sub>2.5</sub> Concentration</td><td><b>", d$anl_avg_pm2.5, " µg/m³</b></td></tr>",
       "<tr style='background:#f9f9f9;'><td>Population Above WHO</td><td><b>", d$percent_population_exposed_above_who, "%</b></td></tr>",
       "<tr><td>Population Above National Standard</td><td><b>", d$percent_population_exposed_above_national, "%</b></td></tr>",
       "<tr><td colspan='2'><b>Life Expectancy Gains</b></td></tr>",
       "<tr><td>If WHO Guideline Met</td><td><b>", d$national_llpp_if_who_met, " years</b></td></tr>",
       "<tr style='background:#f9f9f9;'><td>Total Years Gained (WHO)</td><td><b>", d$total_life_years_gained_if_who_met, " million</b></td></tr>",
       "<tr><td>If National Standard Met</td><td><b>", d$adjusted_llpp_national_level, " </b></td></tr>",
       "<tr style='background:#f9f9f9;'><td>Total Years Gained (National)</td><td><b>", d$total_life_years_lost_if_national_met, " million</b></td></tr>",
       "<tr><td colspan='2'><b>Historical Trends (1998–2023)</b></td></tr>",
       "<tr><td>PM<sub>2.5</sub> Change</td><td><b>", d$pm_trend, " by ", d$pm_diff_1998_2023, "</b></td></tr>",
       "<tr style='background:#f9f9f9;'><td>Life Expectancy Impact</td><td><b>", d$lyl_trend, " by ", d$lyl_who_diff_1998_2023, "</b></td></tr>",
       "</table><br>",
       top5_provinces_html,
       "</div>",

       # RIGHT BLOCK: Most Polluted Regions
       "<div>",
       "<div style='font-size: 18px; font-weight: bold; margin-bottom: 8px;'>Most Polluted Regions</div>",
       "<table style='width:100%; border-collapse:collapse;'>",

       "<tr><th colspan='2' style='background:#f2f2f2; text-align:left; padding:6px;'>Top Provinces (Potential Gain in Life Expectancy if WHO Guideline Met)</th></tr>",
       paste0(
         "<tr><td style='padding:6px;'>", d$province_names[1:3], "</td>",
         "<td style='padding:6px;'><b>", d$province_llpp_who[1:3], "</b></td></tr>", collapse = ""
       ),

       "<tr><th colspan='2' style='background:#f2f2f2; text-align:left; padding:6px;'>Top Provinces (Potential Gain in Life Expectancy if National Standard Met)</th></tr>",
       paste0(
         "<tr><td style='padding:6px;'>", d$province_names[1:3], "</td>",
         "<td style='padding:6px;'><b>", d$province_llpp_nat[1:3], "</b></td></tr>", collapse = ""
       ),

       "<tr><th colspan='2' style='background:#f2f2f2; text-align:left; padding:6px;'>Top Districts (Potential Gain in Life Expectancy if WHO Guideline Met)</th></tr>",
       paste0(
         "<tr><td style='padding:6px;'>", d$top3$name_2[1:3], "</td>",
         "<td style='padding:6px;'><b>", d$top3$llpp_who_2023[1:3], "</b></td></tr>", collapse = ""
       ),
       "<tr><td style='padding:6px;'>Capital City</td><td style='padding:6px;'><b>", if (nrow(d$capital_llpp_who) > 0) d$capital_llpp_who$llpp_who_2023 else "N/A", "</b></td></tr>",

       "<tr><th colspan='2' style='background:#f2f2f2; text-align:left; padding:6px;'>Most Populous Provinces (Potential Gain in Life Expectancy if WHO Guideline Met)</th></tr>",
       paste0(
         "<tr><td style='padding:6px;'>", d$top_three_most_populus_province$name_1[1:3], "</td>",
         "<td style='padding:6px;'><b>", d$top_three_most_populus_province$llpp_who_2023[1:3], "</b></td></tr>", collapse = ""
       ),

       "<tr><th colspan='2' style='background:#f2f2f2; text-align:left; padding:6px;'>Most Populous Districts (Potential Gain in Life Expectancy if WHO Guideline Met)</th></tr>",
       paste0(
         "<tr><td style='padding:6px;'>", d$top_three_most_populus_district$name_2[1:3], " (", d$top_three_most_populus_district$name_1[1:3], ")</td>",
         "<td style='padding:6px;'><b>", d$top_three_most_populus_district$llpp_who_2023[1:3], "</b></td></tr>", collapse = ""
       ),
       "</table><br>",

       "<b>Health Threat Ranking</b><br>",
       "<p style='margin-top:0;'>Particulate pollution is the <b>", d$threat_phrase, "</b> ", d$cause1, ".<br>",
       "While it takes <b>", d$who_loss, "</b> years off the life of an average ", d$country, " resident, ",
       "threats like <b>", d$cause1, "</b> and <b>", d$cause2, "</b> take off <b>", d$loss1, "</b> and <b>", d$loss2, "</b> years, respectively.</p>",
       "</div>",

       "</div></div>" # closing .responsive-two-column and outer wrapper
     ))}



   else {
     d <- data_reactive()

     top5_provinces_html <- if (exists("province_pm_gone_up_last_decade", d) && nrow(d$province_pm_gone_up_last_decade) > 0) {
       paste0(
         "<div style='margin-bottom: 12px;'>",
         "<div style='font-weight: bold; font-size: 16px; margin-bottom: 6px;'>Provinces with Increased PM<sub>2.5</sub> (2013–2023)</div>",
         paste0(
           "<div style='margin-left: 12px;'>• ",
           d$province_pm_gone_up_last_decade$name_1[1:min(5, nrow(d$province_pm_gone_up_last_decade))],
           " – <b>",
           round(d$province_pm_gone_up_last_decade$pm_diff_2013_2023[1:min(5, nrow(d$province_pm_gone_up_last_decade))], 1),
           "%</b></div>",
           collapse = ""
         ),
         "</div>"
       )
     } else {
       "<div><b>Provinces with Increased PM<sub>2.5</sub> (2013–2023):</b><br>None</div>"
     }

     HTML(paste0(
       "<div style='font-family: Arial, sans-serif; font-size: 15px; line-height: 1.6;'>",
       "<table width='100%'><tr><td style='vertical-align:top; width:49%; padding-right:2%;'>",

       "<div style='font-size: 20px; font-weight: bold; margin-bottom: 8px;'>Air Quality Profile: ", d$country, "</div>",

       "<div style='margin-bottom: 8px;'>",
       "• <b>WHO PM<sub>2.5</sub> Guideline:</b> 5 µg/m³<br>",
       "• <b>National Standard:</b> ", d$national_pm25_standard, " µg/m³",
       "</div>",

       "<div style='font-size: 16px; font-weight: bold; margin-top: 12px;'>Exposure to PM<sub>2.5</sub></div>",
       "<div style='margin-left: 12px;'>",
       "• Annual Avg: <b>", d$anl_avg_pm2.5, " µg/m³</b><br>",
       "• % Population > WHO Guideline: <b>", d$percent_population_exposed_above_who, "%</b><br>",
       "• % Population > National Standard: <b>", d$percent_population_exposed_above_national, "%</b>",
       "</div>",

       "<div style='font-size: 16px; font-weight: bold; margin-top: 12px;'>Potential Gains in Life Expectancy</div>",
       "<div style='margin-left: 12px;'>",
       "• If WHO met: <b>", d$national_llpp_if_who_met, " years</b><br>",
       "• Total life years gained (WHO): <b>", d$total_life_years_gained_if_who_met, " million</b><br>",
       "• If National met: <b>", d$adjusted_llpp_national_level, "</b><br>",
       "• Total life years gained (National): <b>", d$total_life_years_lost_if_national_met, " million</b>",
       "</div>",

       "<div style='font-size: 16px; font-weight: bold; margin-top: 12px;'>Historical Trends (1998–2023)</div>",
       "<div style='margin-left: 12px;'>",
       "• PM<sub>2.5</sub> has <b>", d$pm_trend, "</b> by <b>", d$pm_diff_1998_2023, "</b><br>",
       "• Life expectancy has <b>", d$lyl_trend, "</b> by <b>", d$lyl_who_diff_1998_2023, "</b>",
       "</div>",

       top5_provinces_html,

       "</td><td style='vertical-align:top; width:49%; padding-left:2%;'>",

       "<div style='font-size: 16px; font-weight: bold;'>Most Polluted Regions</div><br>",

       "<b>Top Provinces (Potential Gain in Life Expectancy if WHO Guideline Met):</b><br>",
       "- ", d$province_names[1], ": <b>", d$province_llpp_who[1], "</b><br>",
       "- ", d$province_names[2], ": <b>", d$province_llpp_who[2], "</b><br>",
       "- ", d$province_names[3], ": <b>", d$province_llpp_who[3], "</b><br><br>",

       "<b>Top Provinces (Potential Gain in Life Expectancy if National Standard Met):</b><br>",
       "- ", d$province_names[1], ": <b>", d$province_llpp_nat[1], "</b><br>",
       "- ", d$province_names[2], ": <b>", d$province_llpp_nat[2], "</b><br>",
       "- ", d$province_names[3], ": <b>", d$province_llpp_nat[3], "</b><br><br>",

       "<b>Top Districts (Potential Gain in Life Expectancy if WHO Guideline Met):</b><br>",
       "- ", d$top3$name_2[1], ": <b>", d$top3$llpp_who_2023[1], "</b><br>",
       "- ", d$top3$name_2[2], ": <b>", d$top3$llpp_who_2023[2], "</b><br>",
       "- ", d$top3$name_2[3], ": <b>", d$top3$llpp_who_2023[3], "</b><br>",
       "- Capital City: <b>", if (nrow(d$capital_llpp_who) > 0) d$capital_llpp_who$llpp_who_2023 else "N/A", "</b><br><br>",

       "<b>Most Populous Provinces (Potential Gain in Life Expectancy if WHO Guideline Met):</b><br>",
       "- ", d$top_three_most_populus_province$name_1[1], ": <b>", d$top_three_most_populus_province$llpp_who_2023[1], "</b><br>",
       "- ", d$top_three_most_populus_province$name_1[2], ": <b>", d$top_three_most_populus_province$llpp_who_2023[2], "</b><br>",
       "- ", d$top_three_most_populus_province$name_1[3], ": <b>", d$top_three_most_populus_province$llpp_who_2023[3], "</b><br><br>",

       "<b>Most Populous Districts (Potential Gain in Life Expectancy if WHO Guideline Met):</b><br>",
       "- ", d$top_three_most_populus_district$name_2[1], " (", d$top_three_most_populus_district$name_1[1], "): <b>", d$top_three_most_populus_district$llpp_who_2023[1], "</b><br>",
       "- ", d$top_three_most_populus_district$name_2[2], " (", d$top_three_most_populus_district$name_1[2], "): <b>", d$top_three_most_populus_district$llpp_who_2023[2], "</b><br>",
       "- ", d$top_three_most_populus_district$name_2[3], " (", d$top_three_most_populus_district$name_1[3], "): <b>", d$top_three_most_populus_district$llpp_who_2023[3], "</b><br><br>",

       "<b>Health Threat Ranking</b><br>",
       "<p style='margin-top:0;'>Particulate pollution is the <b>", d$threat_phrase, "</b> ", d$cause1,
       ".<br>While it takes <b>", d$who_loss, "</b> years off the life of an average ", d$country,
       " resident, threats like <b>", d$cause1, "</b> and <b>", d$cause2, "</b> take off <b>",
       d$loss1, "</b> and <b>", d$loss2, "</b> years, respectively.</p>",
       # "</td></tr></table></div>"
       #
       # "<div style='font-size: 16px; font-weight: bold;'>Health Threat Ranking</div>",
       # "Particulate pollution is the <b>", d$threat_phrase, "</b> in ", d$country, ".<br>",
       # "While particulate pollution takes <b>", d$who_loss, "</b> years off life expectancy, <br>",
       # "other threats like <b>", d$cause1, "</b> and <b>", d$cause2, "</b> take off <b>", d$loss1, "</b> and <b>", d$loss2, "</b> years respectively.",
       #
       "</td></tr></table></div>"
     ))
   }
 })


 #   # Reactive dataset for selected country
 # fs_fig1_dataset_reactive <- reactive({
 #   gadm2_aqli_2023 %>%
 #     filter(country %in% input$country_gis_fs) %>%
 #     left_join(gadm2_aqli_2023_shp, by = c("objectid_gadm2" = "obidgadm2")) %>%
 #     add_aqli_color_scale_buckets("lyl", "llpp_who_2023") %>%
 #     select(-geometry, geometry) %>%
 #     st_as_sf()
 # })
 #
 #   # Plot
 #   fs_fig1_plot <- reactive({
 #     fs_fig1_dataset <- fs_fig1_dataset_reactive()
 #
 #     ggplot(fs_fig1_dataset) +
 #       geom_sf(
 #         aes(fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)),
 #         color = "aliceblue", lwd = 0.05
 #       ) +
 #       geom_sf(
 #         data = gadm2_aqli_2023_shp %>% filter(name0 == input$country_gis_fs),
 #         color = "azure4", fill = "transparent", lwd = 0.1
 #       ) +
 #       # geom_sf(
 #       #   data = gadm0_aqli_2023_shp %>% filter(name0 == input$country_gis_fs),
 #       #   color = "cornsilk4", fill = "transparent", lwd = 0.3
 #       # ) +
 #       ggthemes::theme_map() +
 #       scale_fill_manual(values = c(
 #         "0 to < 0.1" = "#FFFFFF",
 #         "0.1 to < 0.5" = "#FFF2E1",
 #         "0.5 to < 1" = "#FFEDD3",
 #         "1 to < 2"   = "#FFC97A",
 #         "2 to < 3"   = "#FFA521",
 #         "3 to < 4"   = "#EB6C2A",
 #         "4 to < 5"   = "#D63333",
 #         "5 to < 6"   = "#8E2946",
 #         ">= 6"       = "#451F59"
 #       )) +
 #       labs(
 #         fill = "Potential gain in\nlife expectancy (Years)",
 #         title = NULL,
 #         subtitle = NULL
 #       ) +
 #       theme(
 #         legend.position = "bottom",
 #         legend.justification = c(0.5, 0),
 #         legend.background = element_rect(color = "black"),
 #         legend.text = element_text(size = 23, color = "#222222"),
 #         legend.title = element_text(size = 23, color = "#222222"),
 #         plot.title = element_text(hjust = 0.5, size = 16),
 #         plot.subtitle = element_text(hjust = 0.5, size = 7),
 #         plot.caption = element_text(hjust = 0.7, size = 9, face = "italic"),
 #         legend.key = element_rect(color = "black"),
 #         legend.box.spacing = unit(0, "cm"),
 #         legend.direction = "horizontal",
 #         plot.background = element_rect(fill = "white", color = "white")
 #       ) +
 #       guides(fill = guide_legend(nrow = 1))
 #   })
 #
 #   # Render plot in UI
 #
 #   output$fs_fig1_plot <- renderPlot({
 #     fs_fig1_plot()
 #   })  # Adjust as needed
 #   # Download as PNG
 #   output$download_png <- downloadHandler(
 #     filename = function() {
 #       paste0("potential_life_expectancy_gain_", input$country_gis_fs, ".png")
 #     },
 #     content = function(file) {
 #       ggsave(file, plot = fs_fig1_plot(), device = "png", width = 16, height = 12, dpi = 300)
 #     }
 #   )
 #
 #   # Download as SVG
 #   output$download_svg <- downloadHandler(
 #     filename = function() {
 #       paste0("potential_life_expectancy_gain_", input$country_gis_fs, ".svg")
 #     },
 #     content = function(file) {
 #       ggsave(file, plot = fs_fig1_plot(), device = "svg", width = 16, height = 12)
 #     }
 #   )
 #
 #
 #
 #   ranked_data <- gbd_results_master_2025 %>%
 #     filter(country == input$country_gis_fs) %>%
 #     filter(cause_of_death %in% selected_causes) %>%
 #     arrange(desc(lyl)) %>%
 #     mutate(rank = dense_rank(desc(lyl)))
 #
 #   fs_fig2_data <- ranked_data %>%
 #     filter(
 #       cause_of_death %in% selected_causes
 #     ) %>%
 #     slice_max(lyl, n = 10)
 #
 #   # Rename column for consistency with AQLI functions
 #   colnames(fs_fig2_data)[3] <- "llpp_who_2023"
 #
 #   # Apply AQLI-style color bucket classification
 #   fs_fig2_data <- fs_fig2_data %>%
 #     add_aqli_color_scale_buckets("lyl", "llpp_who_2023")
 #
 #
 #   # --- 3. Plot: Figure 2 – Top 10 Life Expectancy Threats -----------------------
 #
 #   fs_fig2_data %>%
 #     ggplot() +
 #     geom_col(
 #       aes(
 #         x = forcats::fct_reorder(cause_of_death, llpp_who_2023),
 #         y = llpp_who_2023,
 #         fill = forcats::fct_reorder(lyl_bucket, order_lyl_bucket)
 #       ),
 #       width = 0.5
 #     ) +
 #
 #     # Axis labels and titles
 #     labs(
 #       x = "Threats to Life Expectancy",
 #       y = "Life Years Lost",
 #       fill = "Life years lost",
 #       title = ""
 #     ) +
 #
 #     coord_flip() +  # Flip coordinates for horizontal bar chart
 #
 #     # Apply Tufte-style and AQLI base theme
 #     ggthemes::theme_tufte() +
 #     themes_aqli_base +
 #
 #     theme(
 #       legend.position = "bottom",
 #       legend.text = element_text(size = 24, color = "#222222"),
 #       legend.title = element_text(size = 24, color = "#222222"),
 #       axis.text = element_text(size = 20, color = "#222222"),
 #       axis.title.y = element_text(size = 24, margin = margin(r = 0.6, unit = "cm"), color = "#222222"),
 #       axis.title.x = element_text(size = 24, margin = margin(t = 0.6, b = 0.6, unit = "cm"), color = "#222222"),
 #       axis.ticks.y = element_blank(),
 #       axis.line = element_line(),
 #       panel.grid.major.y = element_blank(),
 #       plot.background = element_rect(fill = "white", color = "white"),
 #       legend.box.background = element_rect(color = "black"),
 #       plot.caption = element_text(hjust = 0, size = 8, margin = margin(t = 0.8, unit = "cm")),
 #       plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 0.8, unit = "cm")),
 #       plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 0.8, unit = "cm"))
 #     ) +
 #
 #     # Y-axis range and tick marks
 #     scale_y_continuous(
 #       limits = c(0, ceiling(max(fs_fig2_data$llpp_who_2023, na.rm = TRUE))),
 #       breaks = scales::pretty_breaks(n = 5)
 #     ) +
 #     # scale_y_continuous(breaks = seq(0, 4, 1), limits = c(0, 4)) +
 #
 #     # Custom color scale based on AQLI buckets
 #     scale_fill_manual(values = c(
 #       "0 to < 0.1" = "#FFFFFF",
 #       "0.1 to < 0.5" = "#FFF2E1",
 #       "0.5 to < 1" = "#FFEDD3",
 #       "1 to < 2" = "#FFC97A",
 #       "2 to < 3" = "#FFA521",
 #       "3 to < 4" = "#EB6C2A",
 #       "4 to < 5" = "#D63333",
 #       "5 to < 6" = "#8E2946",
 #       ">= 6" = "#451F59"
 #     )) +
 #
 #     # Legend layout
 #     guides(fill = guide_legend(nrow = 1))


