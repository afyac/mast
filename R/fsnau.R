#' Download FSNAU Dashboard
#'
#' @details Downloads the FSNAU Dashboard available at
#'   \href{https://dashboard.fsnau.org/dashboard}{FSNAU Dashboard}
#'   for the years requested.
#' @param years Years to fetch. Default = 2013:2022
#' @return [data.frame] of all requested FSNAU year data
#' @importFrom rlang .data
#' @export
fsnau_dashboard <- function(years = 2013:2022) {

  months <- substr(month.name, 1, 3)
  datis <- list()
  count <- 1

  pb <- progress::progress_bar$new(
    format = "  Downloading FSNAU [:bar] :percent eta: :eta",
    total = length(years) * 12, clear = FALSE, width= 60)

  for (y in years) {

    for(m in seq_along(months)) {

      pb$tick()
      url <- paste0("https://dashboard.fsnau.org/dashboard/index/01-", months[m], "-", y)
      file <- xml2::read_html(url)
      tables <- rvest::html_nodes(file, "table")
      table1 <- rvest::html_table(tables[1], fill = TRUE, na.strings = "")

      datis[[count]] <- table1[[1]] %>%
        dplyr::rename(district = .data$Districts) %>%
        dplyr::mutate(m = m, y = y) %>%
        dplyr::select(.data$district, m, y, tidyselect::everything())

      count <- count + 1
    }

  }

  # bind and then convert chars to nums
  res <- do.call(rbind, datis)
  res <- res %>%
    dplyr::mutate(dplyr::across(.cols = 5:dplyr::last_col(), chr_to_num))
  return(res)

}

chr_to_num <- function(x) {

  if(is.character(x)) {
    return(as.numeric(gsub(",", "", x)))
  } else {
    return(x)
  }

}
