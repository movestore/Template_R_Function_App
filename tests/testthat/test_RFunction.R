sut()

test_data <- test_data("input3_stork.rds")

test_that("smoke test", {
  actual <- rFunction(data = test_data, sdk = "unit test", year = 2022)
  print(actual)
})