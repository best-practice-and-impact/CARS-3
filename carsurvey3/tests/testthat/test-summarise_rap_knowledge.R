dummy_data <- data.frame(heard_of_RAP = c("No", rep("Yes", 13)), 
                         RAP_knowledge = c(rep("I don't know what a RAP champion is", 2),
                                           rep("I know what a RAP champion is but don't know who the RAP champion in my department is", 3),
                                           rep("I know what a RAP champion is and there is no RAP champion in my department", 4),
                                           rep("I know who the RAP champion in my department is", 5)
                         ))

dummy_output <- summarise_rap_knowledge(dummy_data)

test_that("Check output is dataframe" , expect_s3_class(dummy_output, "data.frame"))

test_that("output does not contain missing values", expect_false(any(is.na(dummy_output))))

test_that("Check number of rows in output", expect_equal(nrow(dummy_output), 5))

test_that("Check number of columns in output", expect_equal(ncol(dummy_output), 2))

test_that("Output column names are correct", expect_equal(colnames(dummy_output), c("RAP champion knowledge", "Count")))

test_that("Labels are in correct order",{
  expect_identical(dummy_output[[1]], 
                   factor(c("Have not heard of RAP",
                            "Heard of RAP, have not heard of RAP champions",
                            "Heard of RAP, does not know department champion",
                            "Heard of RAP champions, no champion in department",
                            "Knows department RAP champion"), 
                          levels = c("Have not heard of RAP",
                                     "Heard of RAP, have not heard of RAP champions",
                                     "Heard of RAP, does not know department champion",
                                     "Heard of RAP champions, no champion in department",
                                     "Knows department RAP champion")))
})
  
  
  test_that("Check output values are correct",{
    
    expect_equal(dummy_output[[2]], c(1, 1, 3, 4, 5))
    
  })
  
  # Integration test with plot function 
  
  test_that("output can be used with plotting function", {
    expect_s3_class(carsurvey2::plot_freqs(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
  })
  