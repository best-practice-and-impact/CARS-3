dummy_data <- data.frame(code_freq = c("Never", rep("Sometimes", 5)), 
                         code_comments = c("Sometimes", "Never", "Never", "Regularly", "All the time", "I don't understand this question"), 
                         function_docs = c("All the time", "I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly"), 
                         readme = c("Regularly", "All the time", "I don't understand this question", "Never", "Rarely", "Sometimes"), 
                         desk_notes = c("I don't understand this question", "Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         registers = c("Rarely", "Sometimes", "Regularly", "All the time", "I don't understand this question", "Never"), 
                         AQA_logs = c("Sometimes", "Regularly", "All the time", "I don't understand this question", "Never", "Rarely"),
                         flow_charts = c("Never", NA, "Rarely", "Sometimes", "Regularly", "All the time")
                         )

dummy_output <- summarise_doc(dummy_data) 

test_that("output is a dataframe", expect_s3_class(dummy_output, "data.frame"))

test_that("output has seven columns", expect_equal(ncol(dummy_output), 7))

test_that("output has seven rows", expect_equal(nrow(dummy_output), 7))

test_that("output does not contain missing values", expect_false(any(is.na.data.frame(dummy_output))))

test_that("output has the correct column order", {
  expect_equal(colnames(dummy_output), c("Question",
                                         "I don't understand this question",
                                         "Never",
                                         "Rarely",
                                         "Sometimes",
                                         "Regularly",
                                         "All the time"))
})

test_that("output has the correct question names", {
  expect_equal(dummy_output[[1]], c("Code comments",
                                    "Documentation for each function or class",
                                    "README files",
                                    "Desk notes",
                                    "Data or assumptions registers",
                                    "Analytical Quality Assurance (AQA) logs",
                                    "Flow charts"))
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[2]], c(0.2, 0.2, 0.2, 0.2, 0.2, 0, 0))
  expect_equal(dummy_output[[3]], c(0.2, 0.2, 0.2, 0.4, 0.2, 0.2, 0))
  expect_equal(dummy_output[[4]], c(0.2, 0.2, 0.2, 0, 0, 0.2, 0.25))
  expect_equal(dummy_output[[5]], c(0, 0.2, 0.2, 0, 0.2, 0.2, 0.25))
  expect_equal(dummy_output[[6]], c(0.2, 0.2, 0, 0.2, 0.2, 0.2, 0.25))
  expect_equal(dummy_output[[7]], c(0.2, 0, 0.2, 0.2, 0.2, 0.2, 0.25))
})

# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(carsurvey2::plot_stacked(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})

