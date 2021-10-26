dummy_data <- data.frame(use_open_source = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"),
                         open_source_code = c("Never", "All the time", "Regularly", "Sometimes", "Rarely"),
                         version_control = c("Rarely", "Never" ,"All the time", "Regularly", "Sometimes"),
                         code_reviews = c("Sometimes", "Rarely", "Never", "All the time", "Regularly"),
                         AQUA_book = c("Regularly", "Sometimes", "Rarely", "Never", "All the time"),
                         readme = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"),
                         code_comments = c("Never", "All the time", "Regularly", "Sometimes", "Rarely"))

dummy_output <- derive_basic_rap_scores(dummy_data)

test_that("output is a dataframe", expect_s3_class(dummy_output, "data.frame"))

new_cols <- c("use_open_source_score",
              "open_code_score",
              "version_control_score",
              "peer_review_score", 
              "AQUA_book_score", 
              "doc_score", 
              "basic_rap_score")

test_that("output has new columns", expect_true(identical(new_cols, colnames(dummy_output[8:14]))))

test_that("output does not contain missing values", expect_false(any(is.na(dummy_output))))

test_that("Check number of rows in output", expect_equal(nrow(dummy_output), 5))

test_that("Check number of columns in output", expect_equal(ncol(dummy_output), 14))

test_that("output values are correct", {
  expect_equal(dummy_output[[8]], c(1, 1, 0, 0 ,0))
  expect_equal(dummy_output[[9]], c(0, 1, 1, 0, 0))
  expect_equal(dummy_output[[10]], c(0, 0, 1, 1, 0))
  expect_equal(dummy_output[[11]], c(0, 0, 0, 1, 1))
  expect_equal(dummy_output[[12]], c(1, 0, 0, 0, 1))
  expect_equal(dummy_output[[13]], c(0, 1, 0, 0, 0))
  expect_equal(dummy_output[[14]], c(2, 3, 2, 2, 2))
  })
