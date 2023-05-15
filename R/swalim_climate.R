#' Download climate data from FAO SWALIM
#'
#' This function downloads climate data for a specified range of years from
#' the FAO SWALIM (Somali Water and Land Information Management) website.
#' It takes a vector of years as an argument and returns a combined dataset
#' containing climate data for all the years within the specified range.
#' Note: As per FAO SWALIM, the current values of CDI do not include the NDVI
#' values, but rather a combination of temperature and rainfall. Since the
#' beginning of 2021, there has been a poor correlation between the NDVI and
#' ground information, resulting in false values of CDI. In this regard, the
#' use of NDVI in generating CDI values has been halted by FAO SWALIM
#' (see here: https://cdi.faoswalim.org/index/cdi).
#'
#' The following datasets are downloaded for all districts in Somalia across
#' the given years:
#' - CDI: Combined Drought Index
#' - PDI: Precipitation Drought Index
#' - TDI: Temperature Drought Index
#' - NDVI: Normalized Difference Vegetation Index (not included in CDI
#' since 2021)
#' - RFE: Rainfall Estimates
#'
#' @param years A vector of years for which the climate data is to be
#' downloaded (default is 2015 to 2023). Data only goes as far back as 2002
#' @return A data frame containing the combined climate data for the specified
#' years.
#'
#' @examples
#'
#' # Download climate data for the years 2015 to 2023
#' climate_data_SOM <- swalim_climate(years = 2015:2023)
#'
#' @export

swalim_climate <- function(years = 2015:2023) {

  # import JSON string containing district information from the FAO SWALIM
  # website
  url <- "https://cdi.faoswalim.org/data/tdi/"

  # convert JSON string to a list and process it to obtain a data frame
  # containing district codes and names
  id_df <- jsonlite::fromJSON(url, simplifyVector = TRUE) %>%
    purrr::transpose() %>%
    as.data.frame() %>%
    dplyr::select(dplyr::contains("district")) %>%
    tidyr::gather(key = "ID", value = "district") %>%
    dplyr::mutate(ID = stringr::str_remove(ID, "district\\."))

  swalim_district_code <-  as.vector(id_df$ID)

  download_and_read_csv <- function(url) {
    # download the file
    response <- httr::GET(url)
    httr::stop_for_status(response)

    # write the CSV data to a temporary file
    tmp_file <- tempfile()
    on.exit(unlink(tmp_file)) # Remove the temporary file when done
    writeBin(httr::content(response, "raw"), tmp_file)

    # read the CSV file and return the dataset
    dataset <- readr::read_csv(tmp_file,
                               col_types = readr::cols(),
                               progress = TRUE)
    return(dataset)
  }

  pb <- progress::progress_bar$new(
    format = "Downloading SWALIM climate data [:bar] :percent eta: :eta",
    total = length(swalim_district_code), clear = FALSE, width = 60)

  # download and read the csv files
  datasets <- lapply(swalim_district_code, function(swalim_district_code) {
    url <- paste0("https://cdi.faoswalim.org/outside/getcsv/",
                  swalim_district_code)
    pb$tick()
    dataset <- download_and_read_csv(url)
    return(dataset)
  })

  # combine the datasets
  combined_dataset <- do.call(rbind, datasets)

  # filter the combined dataset by the selected years
  combined_dataset <- dplyr::filter(combined_dataset, year %in% years)

  # return the combined dataset
  return(combined_dataset)
}
