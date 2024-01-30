#' Generate time units cross district dataframe
#'
#' @details Generate time unit cross district dataframe
#' @param admin2_f district dataset containing all the different admin2 level of the country
#' @param y_start Year study started. Defaults 2014.
#' @param y_end Year study ended. Defaults 2023
#' @param m_start Month study started. Defaults 1.
#' @param m_end Month study ended. Defaults 12
#' @param burn_in_period by how many years to extend period backwards. Defaults 1.
#' @param burn_out_period by how many years to extend period forward.Defaults 0.
#' @return [data.frame] cross join time units and admin2 levels
#' @export
f_gen_ts <- function(admin2_f, y_start = 2014,
                     y_end = 2023, m_start = 1, m_end=12,
                     burn_in_period = 1,
                     burn_out_period = 0) {

  ## Combinations of all the district/date
  when <- list(y_start, y_end, m_start, m_end, burn_in_period,
               burn_out_period)
  names(when) <- (c("y_start", "y_end", "m_start", "m_end", "burn_in_period",
                    "burn_out_period"))

  # create a time unit variable tm (from month 1 to month T of period)
  tm <- seq(1, (( when$y_end + when$burn_out_period - when$y_start +
                    when$burn_in_period ) * 12 + when$m_end - when$m_start + 1 ), 1)

  # Create a time series of district-year-months
  ts <- expand.grid(sort(unique(admin2_f$district)), tm)
  colnames(ts) <- c("district", "time_unit")

  # Add regions and mega-regions
  ts <- merge(ts, admin2_f, by = "district", all.x = TRUE)

  # Work out corresponding year, month and date values
  ts$year <- floor( (ts$time_unit + when$m_start - 2) / 12) + (when$y_start -
                                                                   when$burn_in_period)
  ts$month <- (ts$time_unit + when$m_start - 1) - (ts$year - (when$y_start -
                                                                  when$burn_in_period) ) * 12
  ts$date <- lubridate::ymd(paste(ts$year, ts$month, "1", sep = "-"))

  # Sort time series
  ts <- ts[order(ts[, "district"], ts[, "time_unit"]), ]

  return(ts)
}
