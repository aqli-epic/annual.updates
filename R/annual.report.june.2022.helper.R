## Annual Report Utility Functions---------------------------------------------

#' Get the average PM2.5 pollution and corresponding LE gains of a given region
#'
#' This function gets the average pollution and LE gains number for a given gadm level 0, 1 and 2 region in a given year
#'
#' @import dplyr
#' @import stringr
#' @import purrr
#'
#'
#' @param df master color dataset
#'
#' @param gadm_0 country name, which corresponds to gadm level 0 column in the gadm shape file.
#'               This can also be a vector of length > 1, but in that case both gadm_1 and gadm_2 vector length should be <= 1.

#' @param gadm_1 corresponds to the gadm level 1 region in the gadm shape file.This can also be a vector of length > 1,
#'               but in that case both gadm_1 and gadm_2 vector length should be <= 1.
#'
#' @param gadm_2 corresponds to gadm level 2 region in the gadm shape file. This can also be a vector of length > 1,
#'               but in that case both gadm_1 and gadm_2 vector length should be <= 1.
#'
#' @param year the year for which we want the average PM2.5 and life expectancy gains for the region in question, deafults to 2020
#'
#' @param red_to the amount to which pm2.5 pollution should be reduced, by deafult this is the 5 micrograms per cubic meter
#'
#' @param le_constant its the constant with which we multiply the pm2.5 concentrations value by to get the life expectancy gains in years
#'
#' @return a summary table with an average PM2.5 column and the corresponding Life Expectancy Gains column
#'
#' @examples
#'
#' get_pm2.5_le_gains(color_2020, gadm_0 == "India") # gives India's average PM2.5 and LE gains for India for 2020
#'
#'
#' @export


get_pm2.5_le_gains <- function(df = NULL, gadm_0 = NULL, gadm_1 = NULL, gadm_2 = NULL, year = 2020, red_to = 5, le_constant = 0.098) {

  #> sanity checks

  # if only gadm_0 and gadm_2 filled, ask to fill gadm_1
  if(is_empty(gadm_0) == FALSE){
    if(is_empty(gadm_1) == TRUE & is_empty(gadm_2) == FALSE){
      print("Please also specify the gadm level 1 region to proceed")
      stop()
    } else {
      # continue
    }
  }

  # if only gadm_1 specified, ask to fill gadm_0
  if(is_empty(gadm_0) == TRUE & (is_empty(gadm_1) == FALSE)){
    print("Please specify the gadm level 0 (country) to proceed")
    stop()
  } else {
    # continue
  }

  # if only gadm_2 filled, ask to fill both gadm_0 and gadm_1
  if(is_empty(gadm_0) == TRUE & is_empty(gadm_1) == TRUE & is_empty(gadm_2) == FALSE){
    print("Please specify the gadm level 0 and gadm level 1 region to proceed")
  } else {
    # continue
  }

  #> construct pm2.5 column name corresponding from the year
  pm2.5_col_name <- stringr::str_c("pm", year)

  #> generate summary table for the region in question

  # if its a country, keep only the country and then calculate the population weighted average pm2.5 and corresponding le gains
  if((is_empty(gadm_0) == FALSE) & (is_empty(gadm_1) == TRUE) & (is_empty(gadm_2) == TRUE)){
    print("i1")
    df_summary <- df %>%
      dplyr::filter(country %in% gadm_0) %>%
      dplyr::group_by(country) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
             pm2.5_pop_weighted = pop_weights*(!!as.symbol(pm2.5_col_name))) %>%
      dplyr::summarise(avg_pm2.5 = round(sum(pm2.5_pop_weighted, na.rm = TRUE), 1),
                       le_gains_pm2.5_red_to_given_standard = round((avg_pm2.5 - red_to)*le_constant, 1),
                       le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))
    return(df_summary)

  # if its a state within a country, filter for that state and then calcualte population weighted average PM2.5 and corresponding le gains
  } else if((is_empty(gadm_0) == FALSE) & (is_empty(gadm_1) == FALSE) & (is_empty(gadm_2) == TRUE)) {
    df_summary <- df %>%
      dplyr::filter(country %in% gadm_0, name_1 %in% gadm_1) %>%
      group_by(name_1) %>%
      dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                    pm2.5_pop_weighted = pop_weights*(!!as.symbol(pm2.5_col_name))) %>%
      dplyr::summarise(avg_pm2.5 = round(sum(pm2.5_pop_weighted, na.rm = TRUE), 1),
                       le_gains_pm2.5_red_to_given_standard = round((avg_pm2.5 - red_to)*le_constant, 1),
                       le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))
    return(df_summary)

  # if its a district within a state, within a country, report the pm2.5 value as stored in the color dataset and then calculate the corresponding le gains
  } else if((is_empty(gadm_0) == FALSE) & (is_empty(gadm_1) == FALSE) & (is_empty(gadm_2) == FALSE)){
    df_summary <- df %>%
      dplyr::filter(country == gadm_0, name_1 == gadm_1, name_2 %in% gadm_2) %>%
      dplyr::select(country, name_1, name_2, !!as.symbol(pm2.5_col_name)) %>%
      dplyr::mutate(le_gains_pm2.5_red_to_given_standard = round(((!!as.symbol(pm2.5_col_name) - red_to)*le_constant), 1))

    return(df_summary)
  }

}

#' Get all sub-regions average pm2.5 pollution and life expectancy gains within a specified region
#'
#' This function calculates the average PM2.5 and corresponding life expectancy gains for all sub-regions within
#' a given specified region.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @param df master color dataset
#' @param global defaults to FALSE, but if TRUE, it gives us a average for all countries (which are global sub-regions)
#' @param gadm_0 defaults to NULL, but if not NULL it gives us the average PM2.5 and le gains of the gadm level 1 regions of the gadm_0 region specified
#' @param gadm_1 defaults to NULL, but if not NULL it gives us the average PM2.5 and le gains of the gadm level 2 regions of the gadm_0 and gadm_1 regions specified.
#' @param gadm_2 defaults to NULL, but if not NULL it gives us the average PM2.5 and le gains of the gadm level 2 region, which is present in the gadm_1 region,
#'               which itself is present in the  gadm_0 region.
#' @param level This can be either "gadm_1" or "gadm_2", and applies only to the case when gadm_0 information is needed.
#'              This determines the level at which the output will be supplied
#' @param year this is the year in question for which we need the average PM2.5 and le gains.
#' @param red_to what's the threshold relative to which we want to calculate the le gains, default to WHO guideline of 5 micrograms per cubic meter
#' @param le_constant defaults to 0.098, this stays the same. It is the finding from the recent AQLI paper.
#'
#' @return summary dataset containing average PM2.5 and le gains for the region in question.
#'
#'
#' @note Please note that this function provides the average PM2.5 and LE gains for all sub-regions
#'       within the region specified in the function's parameters.
#'
#' @export

get_pm2.5_le_gains_all_sub_regions <- function(df, global = FALSE, gadm_0 = NULL, gadm_1 = NULL, gadm_2 = NULL, level = NULL, year = 2020, red_to = 5, le_constant = 0.098){

  #> construct pm2.5 column name corresponding from the year
  pm2.5_col_name <- stringr::str_c("pm", year)

  # get the summary table for the sub-regions of the specified region

  if((global == TRUE)){
    if(is_empty(gadm_0) == FALSE | is_empty(gadm_1) == FALSE | is_empty(gadm_2) == FALSE){
      print("To get sub-national level information, enter 'Global = FALSE' and fill in the relevant gadm region for which you want the average PM2.5 and Life Expectancy Gains")
      stop()
    } else {
      df_summary <- df %>%
        group_by(country) %>%
        dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                      pm2.5_pop_weighted = pop_weights*(!!as.symbol(pm2.5_col_name))) %>%
        dplyr::summarise(avg_pm2.5 = round(sum(pm2.5_pop_weighted, na.rm = TRUE), 1),
                         le_gains_pm2.5_red_to_given_standard = round((avg_pm2.5 - red_to)*le_constant, 1),
                         le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))

      return(df_summary)
    }

  } else if (global == FALSE) {


    #> sanity checks

    if(is_empty(gadm_0) == TRUE & is_empty(gadm_1) == TRUE & is_empty(gadm_2) == TRUE){
      print("Please provide a region to proceed")
      stop()
    }

    # if only gadm_0 and gadm_2 filled, ask to fill gadm_1
    if(is_empty(gadm_0) == FALSE){
      if(is_empty(gadm_1) == TRUE & is_empty(gadm_2) == FALSE){
        print("Please also specify the gadm level 1 region to proceed")
        stop()
      } else {
        # continue
      }
    }

    # if only gadm_1 specified, ask to fill gadm_0
    if(is_empty(gadm_0) == TRUE & (is_empty(gadm_1) == FALSE)){
      print("Please specify the gadm level 0 (country) to proceed")
      stop()
    } else {
      # continue
    }

    # if only gadm_2 filled, ask to fill both gadm_0 and gadm_1
    if(is_empty(gadm_0) == TRUE & is_empty(gadm_1) == TRUE & is_empty(gadm_2) == FALSE){
      print("Please specify the gadm level 0 and gadm level 1 region to proceed")
    } else {
      # continue
    }

    if((is_empty(gadm_0) == FALSE) & (is_empty(gadm_1) == TRUE) & (is_empty(gadm_2) == TRUE)){
      if(is_empty(level) == TRUE){
        print(stringr::str_c("Please specify the level at which you want to see", gadm_0, "information. The available options are: gadm_1, gadm_2.", sep = " "))
      } else {
        if(level == "gadm_1"){
          df_summary <- df %>%
            dplyr::filter(country == gadm_0) %>%
            dplyr::group_by(name_1) %>%
            dplyr::mutate(pop_weights = population/sum(population, na.rm = TRUE),
                          pm2.5_pop_weighted = pop_weights*(!!as.symbol(pm2.5_col_name))) %>%
            dplyr::summarise(avg_pm2.5 = round(sum(pm2.5_pop_weighted, na.rm = TRUE), 1),
                             le_gains_pm2.5_red_to_given_standard = round((avg_pm2.5 - red_to)*le_constant, 1),
                             le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))
          return(df_summary)

        } else if (level == "gadm_2") {
          df_summary <- df %>%
            dplyr::filter(country == gadm_0) %>%
            dplyr::select(country, name_1, name_2, !!as.symbol(pm2.5_col_name)) %>%
            dplyr::mutate(le_gains_pm2.5_red_to_given_standard = round((!!as.symbol(pm2.5_col_name) - red_to)*le_constant, 1),
                          le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))
          return(df_summary)

        }
      }
    } else if((is_empty(gadm_0) == FALSE) & (is_empty(gadm_1) == FALSE) & (is_empty(gadm_2) == TRUE)){
      df_summary <- df %>%
        dplyr::filter(country == gadm_0, name_1 == gadm_1) %>%
        dplyr::select(country, name_1, name_2, !!as.symbol(pm2.5_col_name)) %>%
        dplyr::mutate(le_gains_pm2.5_red_to_given_standard = round((!!as.symbol(pm2.5_col_name) - red_to)*le_constant, 1),
                      le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))
      return(df_summary)

    } else if((is_empty(gadm_0) == FALSE) & (is_empty(gadm_1) == FALSE) & (is_empty(gadm_2) == FALSE)){
      df_summary <- df %>%
        dplyr::filter(country == gadm_0, name_1 == gadm_1, name_2 == gadm_2) %>%
        dplyr::select(country, name_1, name_2, !!as.symbol(pm2.5_col_name)) %>%
        dplyr::mutate(le_gains_pm2.5_red_to_given_standard = round((!!as.symbol(pm2.5_col_name) - red_to)*le_constant, 1),
                      le_gains_pm2.5_red_to_given_standard = ifelse(le_gains_pm2.5_red_to_given_standard < 0, 0, le_gains_pm2.5_red_to_given_standard))
      return(df_summary)

  }

  }

}
