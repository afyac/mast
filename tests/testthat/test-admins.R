test_that("admin match works", {
  admins <- c("ADALE", "JIBBER")
  expect_message(new <- admin_match(admins))
  expect_true(all(new == c("Cadale", "Berbera")))
})
