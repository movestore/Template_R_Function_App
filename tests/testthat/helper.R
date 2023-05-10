sut <- function() {
  source(file.path("..", "..", "src", "common", "logger.R"))
  source(file.path("..", "..", "src", "io", "app_files.R"))
  source(file.path("..", "..", "src", "io", "io_handler.R"))
  source(file.path("..", "..", "./RFunction.R"))
}

test_data <- function(test_file) {
    test_data_root_dir <- test_path("data")
    readRDS(file = file.path(test_data_root_dir, test_file))
}