context("Test swalim_climate function")


# Test case 1:------------------------------------------------------------------
# Test if the URL for district information is valid
test_that("swalim_climate downloads district information from a valid URL", {
  skip_on_cran() # skip this test on CRAN (requires internet access)
  url <- "https://cdi.faoswalim.org/data/tdi/"
  response <- httr::GET(url)
  expect_equal(response$status_code, 200) # Use http_status() to access status code
})

# Test case 2:------------------------------------------------------------------
# Test if the district information can be extracted from JSON and if the information
# is in the correct format
test_that("swalim_climate extracts district ID for webscraping correctly", {
  skip_on_cran() # Skip this test on CRAN because it requires internet access
  url <- "https://cdi.faoswalim.org/data/tdi/"
  response <- httr::GET(url)
  id_df <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE) %>%
    purrr::transpose() %>%
    as.data.frame() %>%
    dplyr::select(dplyr::contains("district")) %>%
    tidyr::gather(key = "ID", value = "district") %>%
    dplyr::mutate(ID = stringr::str_remove(ID, "district\\.")) %>%
    dplyr::arrange(district)

  # Validate the output
  expect_true(all(id_df$ID >= 1101 & id_df$ID <= 2804))
  expect_equal(id_df$district[1], "Adan Yabaal")
  expect_equal(tail(id_df$district, n = 1), "Zeylac")
})

# Test case 3:------------------------------------------------------------------
# Test if the CSV files can be downloaded and sense-check the values are in the right range
test_that("swalim_climate downloads and reads CSV files accurately,
          with valid structure and plausible value ranges", {
            skip_on_cran() # Skip this test on CRAN because it requires internet access
            # Call the swalim_climate function to retrieve the climate data
            climate_data <- swalim_climate()
            # Define the expected range of years
            expected_years <- 2002:format(Sys.Date(), "%Y")
            # Validate the structure of the dataset
            expect_equal(ncol(climate_data), 11)
            expect_equal(colnames(climate_data), c(
              "year", "month", "district", "pcode",
              "rfe", "ndvi", "temp", "pdi", "tdi",
              "vdi", "cdi"
            ))
            expect_true(all(unique(climate_data$year) %in% expected_years))
            expect_equal(sort(unique(climate_data$month)), 1:12)
            expect_equal(length(unique(climate_data$district)), 73)
          })
