# Data operations table
# Frequency table should not include missing values and should include counts of 0

dummy_data <- data.frame(operations_analysis = c(rep("I do some or all of this by coding", 3), rep("I don't do this", 2), "I do this without coding", NA),
                         operations_data_cleaning = c(rep("I do some or all of this by coding", 2), rep("I don't do this", 3), rep("I do this without coding", 2)), 
                         operations_data_linking = c(rep("I do some or all of this by coding", 1), rep("I don't do this", 2), rep("I do this without coding", 4)),
                         operations_data_transfer = c(rep("I do some or all of this by coding", 4), rep("I don't do this", 3)),
                         operations_data_vis = c(rep("I do some or all of this by coding", 3), rep("I don't do this", 2), rep("I do this without coding", 2)),
                         operations_machine_learning = c(rep("I do some or all of this by coding", 2), rep("I don't do this", 3), rep("I do this without coding", 2)),
                         operations_modelling = c(rep("I do some or all of this by coding", 3), rep("I don't do this", 2), rep("I do this without coding", 2))
)

dummy_output <- summarise_operations(dummy_data)

test_that("output is a dataframe", expect_s3_class(dummy_output, "data.frame"))

test_that("output has three columns", expect_equal(ncol(dummy_output), 3))

test_that("output does not contain missing values", expect_false(any(is.na.data.frame(dummy_output))))

test_that("output has the correct column order", {
  expect_equal(colnames(dummy_output), c("Data operation", "I do some or all of this by coding", "I do this without coding"))
})

test_that("output has the correct question names", {
  expect_equal(dummy_output[[1]], c("Analysis", "Data cleaning", "Data linking", "Data transfer", "Data visualisastion", "Machine learning", "Modelling"))
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[2]], c(3, 2, 1, 4, 3, 2, 3))
  expect_equal(dummy_output[[3]], c(1, 2, 4, 0, 2, 2, 2))
})

# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(plot_stacked(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})