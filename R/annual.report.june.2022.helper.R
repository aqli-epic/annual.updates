## Annual Report Utility Functions

#' Get the average PM2.5 pollution and corresponding LE gains of a given region
#'
#' This function gets the average pollution and LE gains number for a given gadm level 0, 1 and 2 region in a given year
#'
#' @import dplyr
#' @import stringr
#'
#'
#' @param df master color dataset
#' @param gadm_0 country name, which corresponds to gadm level 0 column in the gadm shape file
#' @param gadm_1 corresponds to the gadm level 1 region in the gadm shape file
#' @param gadm_2 corresponds to gadm level 2 region in the gadm shape file
#' @param year the year for which we want the average PM2.5 and life expectancy gains for the region in question, deafults to 2020
#' @param red_to the amount to which pm2.5 pollution should be reduced, by deafult this is the 5 micrograms per cubic meter
#' @param le_constant its the constant with which we multiply the pm2.5 concentrations value by to get the life expectancy gains in years
#'
#' @return a summary table with an average PM2.5 column and the corresponding Life Expectancy Gains column
#'
#' @examples
#'
#' get_pm2.5_le_gains(color_2020, gadm_0 == "India") # gives India's average PM2.5 and LE gains for India for 2020
#'
#' @note To get the results for a gadm level 2 region, one has to specify the gadm 0 region.
#'       Similarly, to get the results for a gadm level 2 region, one has to specify both gadm level 0
#'       and gadm level 1 regions.But, to get the results for a gadm level 1 region, there is no need to
#'       specify the gadm level 1 and 2 regions.
#'
#' @export


get_pm2.5_le_gains <- function(df, gadm_0 = "", gadm_1 = "", gadm_2 = "", year = 2020, red_to = 5, le_constant = 0.098) {

  #> construct pm2.5 column name corresponding from the year
  pm2.5_col_name <- stringr::str_c("pm", year)
  print(pm2.5_col_name)
  print(name_0)

  #> generate summary table for the region in question

  # if its a country, keep only the country and then calculate the population weighted average pm2.5 and corresponding le gains
  if(gadm_0 != "" & gadm_1 == "" & gadm_2 == ""){
    print("i1")
    df_summary <- df %>%
      dplyr::filter(country == gadm_0) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             pm2.5_pop_weighted = pop_weights*(!!as.symbol(pm2.5_col_name))) %>%
      dplyr::summarise(avg_pm2.5 = round(sum(pm2.5_pop_weighted, na.rm = TRUE), 1),
                       le_gains_pm2.5_red_to_given_standard = round((avg_pm2.5 - red_to)*le_constant, 1),
                       le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))
    return(df_summary)

  # if its a state within a country, filter for that state and then calcualte population weighted average PM2.5 and corresponding le gains
  } else if(gadm_0 != "" & gadm_1 != "" & gadm_2 == "") {
    df_summary <- df %>%
      dplyr::filter(country == gadm_0, name_1 == gadm_1) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                    pm2.5_pop_weighted = pop_weights*(!!as.symbol(pm2.5_col_name))) %>%
      dplyr::summarise(avg_pm2.5 = round(sum(pm2.5_pop_weighted, na.rm = TRUE), 1),
                       le_gains_pm2.5_red_to_given_standard = round((avg_pm2.5 - red_to)*le_constant, 1),
                       le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))
    return(df_summary)

  # if its a district within a state, within a country, report the pm2.5 value as stored in the color dataset and then calculate the corresponding le gains
  } else if(gadm_0 != "" & gadm_1 != "" & gadm_2 != ""){
    df_summary <- df %>%
      dplyr::filter(country == gadm_0, name_1 == gadm_1, name_2 == gadm_2) %>%
      dplyr::select(country, name_1, name_2, !!as.symbol(pm2.5_col_name)) %>%
      dplyr::mutate(le_gains_pm2.5_red_to_given_standard = round(((!!as.symbol(pm2.5_col_name) - red_to)*le_constant), 1))

    return(df_summary)
  }

}
