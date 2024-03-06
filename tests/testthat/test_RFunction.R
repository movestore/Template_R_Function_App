test_data <- test_data("input3_move2loc_LatLon.rds")

test_that("happy path", {
  actual <- rFunction(data = test_data, sdk = "unit test", year = 1998)
  # TODO: how to select the column of timestamps in a move2 object?
  expect_equal(unique(lubridate::year(actual@timestamps)), 1998)
})

test_that("year not included", {
  actual <- rFunction(data = test_data, sdk = "unit test", year = 1900)
  expect_null(actual)
})