context("utils")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("read_mast_file", {

  # check these idential reads works
  ch1 <- read_mast_file("som_idp_prmn_counterfactuals.csv")
  ch2 <- read_mast_file("som_idp_prmn_counterfactuals.rds")

  expect_equal(ch1, ch2)
  expect_true(class(ch1) == "data.frame")

  # check the sheet option works
  ch3 <- read_mast_file("som_analysis_parameters.xlsx")
  ch4 <- read_mast_file("som_analysis_parameters.xlsx", "dictionary")
  ch5 <- read_mast_file("som_analysis_parameters.xlsx", "general_parameters")
  expect_true(!identical(ch3, ch4))
  expect_true(identical(ch3, ch5))

  expect_error(read_mast_file("test.csv"))
  expect_error(read_mast_file("test.test"), "file extension")

})

test_that("mast quiet messaging", {
  expect_invisible(quiet_message("msg"))
  Sys.setenv("MAST_LOUD" = "TRUE")
  expect_message(quiet_message("msg"))
  Sys.setenv("MAST_LOUD" = "FALSE")
})
