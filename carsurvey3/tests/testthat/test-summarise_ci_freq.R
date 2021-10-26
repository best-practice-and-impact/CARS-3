dummy_data <- data.frame(CI = c(NA, 
                                rep("Yes", 2), 
                                rep("No", 3), 
                                rep("I don't know what continuous integration is", 4)))

dummy_output <- summarise_ci_freq(dummy_data)

test_that("output is a dataframe", expect_s3_class(dummy_output, "data.frame"))

test_that("output has two columns", expect_equal(ncol(dummy_output), 2))

test_that("output does not contain missing values", expect_false(any(is.na.data.frame(dummy_output))))

test_that("labels are in the correct order", {
  expect_identical(dummy_output[[1]], 
                   factor(c("Yes",
                            "No",
                            "I don't know what continuous integration is"), 
                          levels = c("Yes",
                                     "No",
                                     "I don't know what continuous integration is"))
  )
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[2]], c(2, 3, 4))
})

# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(plot_freqs(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})
