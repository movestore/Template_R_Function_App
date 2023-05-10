sut()

test_data <- test_data("input3_stork.rds")

test_that("smoke test", {
  actual <- rFunction(data = test_data, sdk = "unit test", year = 2005)
  expect_equal(unique(lubridate::year(actual@timestamps)), 2005)
})