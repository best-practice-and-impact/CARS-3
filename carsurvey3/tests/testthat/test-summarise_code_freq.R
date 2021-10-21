# Coding frequency table
# Frequency table should not include missing values and should include counts of 0

dummy_data <- data.frame(code_freq = c(rep("Sometimes", 3), rep("All the time", 2), "Never", NA))

dummy_output <- summarise_code_freq(dummy_data)

test_that("output is a dataframe", expect_s3_class(dummy_output, "data.frame"))

test_that("output has four columns", expect_equal(ncol(dummy_output), 2))

test_that("output does not contain missing values", expect_false(any(is.na.data.frame(dummy_output))))

test_that("labels are in the correct order", {
  expect_identical(dummy_output[[1]], 
               factor(c("Never",
                        "Rarely",
                        "Sometimes",
                        "Regularly",
                        "All the time"), 
                      levels = c("Never",
                                 "Rarely",
                                 "Sometimes",
                                 "Regularly",
                                 "All the time"))
  )
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[2]], c(1, 0, 3, 0, 2))
})

# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(plot_freqs(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})