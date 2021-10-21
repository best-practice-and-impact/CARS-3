dummy_data <- data.frame(column1 = c("Dummy1", "Dummy1", "Dummy2"),
                         column2 = c("Dummy2", "Dummy2", "Dummy3"), 
                         column3 = c("Dummy3", "Dummy3", "Dummy1")
                         )
levels <- c("Dummy3", "Dummy2", "Dummy1")
labels <- c("column2", "column1", "column3")

dummy_output <- calc_multi_col_freqs(dummy_data, levels, labels, calc_props = FALSE)


test_that("Check output is dataframe" , expect_s3_class(dummy_output, "data.frame"))

test_that("output does not contain missing values", expect_false(any(is.na(dummy_output))))

test_that("Check the number of columns in output", expect_equal(ncol(dummy_output), 4) )

test_that("Check the number of rows in output", expect_equal(nrow(dummy_output), 3))

test_that("Check the values in output", {
  expect_equal(dummy_output[[2]], c(0, 1, 2))
  expect_equal(dummy_output[[3]], c(1, 2, 0))
  expect_equal(dummy_output[[4]], c(2, 0, 1))
})

test_that("Check the row labels are correctly ordered", expect_equal(dummy_output[[1]], 
                                                                     c("column2", "column1", "column3")))

test_that("Check the column names in output", expect_equal(colnames(dummy_output),
                                                           c("labels", "Dummy3", "Dummy2", "Dummy1")))


dummy_output <- calc_multi_col_freqs(dummy_data, levels, labels, calc_props = TRUE)

test_that("Check the values in output", {
  expect_equal(dummy_output[[2]], c(0, 1/3, 2/3))
  expect_equal(dummy_output[[3]], c(1/3, 2/3, 0))
  expect_equal(dummy_output[[4]], c(2/3, 0, 1/3))
})

integration test with a table functions
