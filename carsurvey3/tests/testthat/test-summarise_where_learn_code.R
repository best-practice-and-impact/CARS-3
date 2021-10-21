dummy_data <- data.frame(code_freq = c(rep("Never", 8), "Sometimes", "Regularly"),
                         learn_before_current_role = c(rep("Yes", 8), NA, "No"), 
                         where_learned_to_code = c(rep("Self-taught" , 3),
                                                  rep( "In public sector employment", 3),
                                                  rep("other" , 1),
                                                  rep(NA , 3)))

dummy_output <- summarise_where_learned_code(dummy_data)

test_that("Check output is dataframe" , expect_s3_class(dummy_output, "data.frame"))

test_that("output does not contain missing values", expect_false(any(is.na(dummy_output))))

test_that("Check number of rows in output", expect_equal(nrow(dummy_output), 6))

test_that("Check number of columns in output", expect_equal(ncol(dummy_output), 2))

test_that("Output column names are correct", expect_equal(colnames(dummy_output), c("First coding experience", "Count")))

test_that("Labels are in correct order",{
  expect_identical(dummy_output[[1]], 
                   factor(c("In current role",
                            "In education",
                            "In private sector employment",
                            "In public sector employment",
                            "Self-taught",
                            "Other"), 
                          levels = c("In current role",
                                     "In education",
                                     "In private sector employment",
                                     "In public sector employment",
                                     "Self-taught",
                                     "Other"))
                   )
  })

 
test_that("Values in output are correct", expect_equal(dummy_output[[2]], c(2, 0, 0, 3, 3, 1))) 

# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(carsurvey2::plot_freqs(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})
