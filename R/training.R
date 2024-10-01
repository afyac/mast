#' Function to clean metadata smart surveys
#'
#' @details Function to clean metadata smart surveys
#' @param metadata SMART Surveys Metadata
#' @param ts dataframe contraining time units and admin2
#' @importFrom rlang .data
#' @export
f_clean_metadata <- function(metadata, ts){
  ts <- ts |> dplyr::select(year, month, time_unit) |> dplyr::distinct()
  # Generate necessary time quantities for calculation
  metadata$end_recall_date <- as.Date(metadata$end_date) -
    (as.Date(metadata$end_date) - as.Date(metadata$start_date))/2
  metadata$start_recall_date <- as.Date(metadata$end_recall_date) - metadata$recall_days
  metadata$year_end_recall_date <- lubridate::year(metadata$end_recall_date)
  metadata$month_end_recall_date <- lubridate::month(metadata$end_recall_date)
  metadata$year_start_recall_date <- lubridate::year(metadata$start_recall_date)
  metadata$month_start_recall_date <- lubridate::month(metadata$start_recall_date)
  metadata$days_in_month_start <- lubridate::days_in_month(metadata$start_recall_date)
  metadata$days_in_month_end <- lubridate::days_in_month(metadata$end_recall_date)
  metadata$day_start <- lubridate::day(metadata$start_recall_date)
  metadata$day_end <- lubridate::day(metadata$end_recall_date)

  # merge metadata with time units - to have a start and end time unit
  metadata <- merge(metadata, ts, by.y=c('year', 'month'),
                    by.x=c('year_start_recall_date', 'month_start_recall_date')) |>
    dplyr::rename(time_unit_recall_start = time_unit)

  metadata <- merge(metadata, ts, by.y=c('year', 'month'),
                    by.x=c('year_end_recall_date', 'month_end_recall_date')) |>
    dplyr::rename(time_unit_recall_end = time_unit)

  # merge metadata with time units - to have a mid time unit
  metadata$recall_mid <- as.Date(metadata$end_date) - metadata$recall_days/2
  metadata$year_recall_mid <- lubridate::year(metadata$recall_mid)
  metadata$month_recall_mid <- lubridate::month(metadata$recall_mid)
  metadata <- merge(x=metadata, y=ts, by.x=c('year_recall_mid', 'month_recall_mid'),
                    by.y = c('year', 'month'), all.x=TRUE) |>
    dplyr::rename(mid_time_unit = time_unit)

  return(metadata)
}

#' Function to clean metadata smart surveys
#'
#' @details Function to clean metadata smart surveys
#' @param f_surveys_cov Surveys Coverage
#' @param f_df dataframe
#' @importFrom rlang .data
#' @export
## Function to calculate surveys coverage
f_calc_days <- function(f_surveys_cov, f_df) {
  # select survey
  s <- subset(f_df, survey_id == f_surveys_cov["surveyId"])
  tm_now <- as.integer(f_surveys_cov["time_unit"])
  c1 <- as.integer(s["time_unit_recall_start"]) - tm_now
  c2 <- as.integer(s["time_unit_recall_end"])- tm_now
  x1 <- 0

  # calculate proportion of the month's days that are covered by the survey's recall period
  if (c1 > 0) { x1 <- 0.0 }
  if (c1 == 0) { x1 <- (as.integer(s["days_in_month_start"]) - as.integer(s["day_start"]) ) /
    as.integer(s["days_in_month_start"]) }
  if (c1 < 0 & c2 > 0) { x1 <- 1.0 }
  if (c2 == 0) { x1 <- as.integer(s["day_end"]) / as.integer(s["days_in_month_end"]) }
  if (c2 < 0) { x1 <- 0.0 }

  return(x1)
}


#' Function to join smart surveys and predictors
#'
#' @details Function to join smart surveys and predictors by weights
#' @param predictors_data Predictor Datasets
#' @param ts dataframe contraining time units and admin2
#' @param surveys_cov Coverage of the SMART Surveys
#' @param hh_obs_complete Data Frame of the COMPLETE SMART SURVEYS
#' @param col_admin2 name of the admin2 column (most of the time 'district')
#' @importFrom rlang .data
#' @export
f_calculate_weighted_predictors <- function(predictors_data, ts, surveys_cov,
                                            hh_obs_complete, col_admin2){
  # select only the numeric data
  variables <- colnames(predictors_data |>
                          dplyr::select(where(is.numeric), -colnames(ts)))

  # calculate a weighted predictors using the month coverage and the different predictors
  data_final <- data.frame()
  for(survey_id in unique(surveys_cov$surveyId)){
    ## select one smart surveys
    sub_hh_obs <- hh_obs_complete |> dplyr::filter(surveyId == survey_id)
    if(nrow(sub_hh_obs) != 0){
      ## select the time covered by this smart surveys
      sub_cov <- surveys_cov |> dplyr::filter(surveyId == survey_id &
                                                time_unit %in% seq(unique(sub_hh_obs$time_unit_recall_start),
                                                                   unique(sub_hh_obs$time_unit_recall_end)))

      ## Select the predictor variables during these time and localisation
      sub_pred <- subset(predictors_data,
                         predictors_data[, col_admin2] == unique(sub_hh_obs[, col_admin2]) &
                           predictors_data$time_unit %in%
                           seq(unique(sub_hh_obs$time_unit_recall_start), unique(sub_hh_obs$time_unit_recall_end)))

      if(nrow(sub_pred)!=0){
        ## sum the different values for the different variables
        aggr <- data.frame(colSums(sub_pred[,variables]*sub_cov$month_coverage,
                                   na.rm=TRUE))
        ## divide by the weight = month coverage
        aggr <- data.frame(aggr/sum(sub_cov$month_coverage,na.rm=TRUE))
        aggr <- cbind('names' = rownames(aggr), aggr)
        rownames(aggr) <- 1:nrow(aggr)
        colnames(aggr) <- c('names', 'values')
        aggr$surveyId <- survey_id
        if(nrow(data_final) == 0){
          data_final <- aggr
        }else{
          data_final <- rbind(data_final, aggr)
        }
      }
    }

  }
  data_final <- data_final |> tidyr::spread(names, values)
  return(data_final)
}

