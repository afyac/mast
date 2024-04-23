#' Transform Variables into Categorical Variables
#'
#' @details Modify the variable into transformed variables
#' @param data DataFrame
#' @param breaks_and_labels Breaks and Label named
#' @return [data.frame] DataFrame with the addition of categorical variables
#' @export
f_categorise <- function(data, breaks_and_labels) {

  # Function to apply the transformation
  cut_variable <- function(x, breaks, labels) {
    cut(x, breaks = breaks, labels = labels, right = FALSE,
        include.lowest = TRUE)
  }

  # Function to select columns based on prefix and exclusion of "_scn"
  select_columns <- function(data, prefix) {
    col_names <- names(data)
    selected_cols <- col_names[startsWith(col_names, prefix) &
                                 !stringr::str_detect(col_names, "_scn")]
    selected_cols
  }

  # Iterate over breaks_and_labels to apply the transformation
  data_transformed <- data

  for (prefix in names(breaks_and_labels)) {
    selected_columns <- select_columns(data_transformed, prefix)
    data_transformed <- data_transformed |>
      dplyr::mutate(aplyr::across(
        .cols = dplyr::all_of(selected_columns),
        .fns = ~ cut_variable(.x,
                              breaks = breaks_and_labels[[prefix]]$breaks,
                              labels = breaks_and_labels[[prefix]]$labels),
        .names = "{.col}_cat"
      ))
  }

  # Check for missing values in newly created variables
  new_vars <- grep("_cat$", names(data_transformed), value = TRUE)
  if (any(sapply(data_transformed[new_vars], function(x) any(is.na(x))))) {
    message("Warning: Some variables have not been properly categorized and contain missing values.")
  }

  return(data_transformed)
}

#' Create complete dataset by using mice
#'
#' @details Modify the dataset in order to complete the variables
#' @param data DataFrame
#' @param variable Variable to impute
#' @param col_admin2 colnames of the admin2 level
#' @return [data.frame] DataFrame completed
#' @importFrom rlang .data
#' @export
f_complete_mice <- function(data, variable, col_admin2){
  # impute water prices (seems decent!)
  mice_mod <- mice::mice(data, m = 1, method = "rf", seed = 500)

  # get complete data and remove the predictors to impute water price
  predictors_complete <- mice::complete(mice_mod)

  # smooth water prices over times
  predictors_complete <- predictors_complete |>
    dplyr::group_by(dplyr::across(col_admin2)) |>
    dplyr::mutate(
      water_price_smooth = predict(
        smooth.spline(na.omit(cbind(time_unit, variable)),
                      spar = 0.4), time_unit)$y
    ) |>
    dplyr::ungroup()

  return(predictors_complete)
}

#' Create rate dataset using the population data
#'
#' @details Modify the dataset in order to rate some variables
#' @param data DataFrame
#' @param pred_rates predictors to be rated
#' @param col_admin2 colnames of the admin2 level
#' @param col_pop colnames of the population
#' @return [data.frame] DataFrame completed
#' @importFrom rlang .data
#' @export
f_rate_variables <- function(data, pred_rates, col_admin2, col_pop){
  data <- data |>
    dplyr::group_by(dplyr::across(c(col_admin2, date))) |>
    dplyr::mutate(dplyr::across(
      .cols = tidyr::all_of(pred_rates),
      .fns = ~ (.x * 1e5) / !!rlang::sym(col_pop),
      .names = "{.col}_rate"
    )) |>  dplyr::ungroup()
  return(data)
}

#' Create lags
#'
#' @details Modify the dataset in order to lags some variable
#' @param data DataFrame
#' @param vars predictors to be lags
#' @param lags lagged time
#' @param col_admin2 colnames of the admin2 level
#' @param col_admin1 colnames of the admin1 level
#' @return [data.frame] DataFrame completed
#' @importFrom rlang .data
#' @export
f_create_lags <- function(data, vars, col_admin2, col_admin1, lags = 1:6) {
  data <- data |>
    dplyr::mutate(date = as.Date(paste(year, month, "01", sep="-"), "%Y-%m-%d")) |>
    dplyr::arrange(dplyr::across(c(col_admin1, col_admin2, date)))
  print('changement')
  for (var in vars) {
    for (lag in lags) {
      data <- data |>
        dplyr::group_by(dplyr::across(c(col_admin2, col_admin1))) |>
        dplyr::mutate(!!paste0(var, "_lag", lag) := dplyr::lag(.data[[var]], lag))
    }
  }
  data <- data |> dplyr::ungroup()

  return(data)
}

#' Create categories
#'
#' @details Modify the dataset in order to categorize some variables
#' @param data DataFrame
#' @param breaks_and_labels predictors and there breaks
#' @return [data.frame] DataFrame completed
#' @importFrom rlang .data
#' @export
f_categorise <- function(data, breaks_and_labels) {

  # Function to apply the transformation
  cut_variable <- function(x, breaks, labels) {
    cut(x, breaks = breaks, labels = labels, right = FALSE,
        include.lowest = TRUE)
  }

  # Function to select columns based on prefix and exclusion of "_scn"
  select_columns <- function(data, prefix) {
    col_names <- names(data)
    selected_cols <- col_names[startsWith(col_names, prefix) &
                                 !stringr::str_detect(col_names, "_scn")]
    selected_cols
  }

  # Iterate over breaks_and_labels to apply the transformation
  data_transformed <- data

  for (prefix in names(breaks_and_labels)) {
    selected_columns <- select_columns(data_transformed, prefix)
    data_transformed <- data_transformed |>
      dplyr::mutate(dplyr::across(
        .cols = tidyr::all_of(selected_columns),
        .fns = ~ cut_variable(.x,
                              breaks = breaks_and_labels[[prefix]]$breaks,
                              labels = breaks_and_labels[[prefix]]$labels),
        .names = "{.col}_cat"
      ))
  }

  # Check for missing values in newly created variables
  new_vars <- grep("_cat$", names(data_transformed), value = TRUE)
  if (any(sapply(data_transformed[new_vars], function(x) any(is.na(x))))) {
    message("Warning: Some variables have not been properly categorized and contain missing values.")
  }

  return(data_transformed)
}

#' Create scale and normalize predictors
#'
#' @details Modify the dataset in order to norm some variable
#' @param data DataFrame
#' @param preds_scn predictors to be normalized
#' @return [data.frame] DataFrame completed
#' @importFrom rlang .data
#' @export
f_norm_preds <- function(data, preds_scn) {
  for (i in preds_scn) {
    # training data
    x <- scales::rescale(tibble::deframe(data[, i]), to=c(0,1))
    # apply same scaling parameters to prediction data
    data[, paste(i, "_scn", sep = "")] <- x
  }
  return(data)
}
