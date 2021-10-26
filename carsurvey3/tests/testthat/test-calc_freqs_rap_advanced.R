dummy_data <- data.frame(advanced_rap_score <- c(0, 
                                              rep(1,2),
                                              rep(2,3), 
                                              rep(3,4), 
                                              rep(4,5), 
                                              rep(5,6), 
                                              rep(6,7), 
                                              rep(7,8)))

dummy_output <- calc_freqs_rap_advanced(dummy_data)

test_that("Check output is dataframe" , expect_s3_class(dummy_output, "data.frame"))

test_that("output does not contain missing values", expect_false(any(is.na(dummy_output))))

test_that("Check number of rows in output", expect_equal(nrow(dummy_output), 8))

test_that("Check number of columns in output", expect_equal(ncol(dummy_output), 2))

test_that("Output column names are correct", expect_equal(colnames(dummy_output), c("Advanced RAP score", "Count")))


test_that("Values in output are correct", expect_equal(dummy_output[[2]], c(1, 2, 3, 4, 5, 6, 7, 8))) 

# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(carsurvey2::plot_freqs(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})
