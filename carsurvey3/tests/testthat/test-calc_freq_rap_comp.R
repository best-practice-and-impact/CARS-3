dummy_data <- data.frame(use_open_source_score = c(1, 3, 5, 7),
                         open_code_score = c(2, 4, 6, 8),
                         version_control_score = c(2, 4, 6, 8),
                         peer_review_score = c(1, 3, 5, 7), 
                         AQUA_book_score = c(2, 4, 6, 8), 
                         doc_score = c(1, 3, 5, 7),
                         function_score = c(2, 4, 6, 8), 
                         unit_test_score = c(1, 3, 5, 7), 
                         function_doc_score = c(2, 4, 6, 8), 
                         package_score = c(1, 3, 5, 7), 
                         code_style_score = c(2, 4, 6, 8), 
                         cont_integreation_score = c(1, 3, 5, 7), 
                         dep_management_score = c(2, 4, 6, 8))

dummy_output <- calc_freq_rap_comp(dummy_data)


test_that("Check output is dataframe" , expect_s3_class(dummy_output, "data.frame"))

test_that("output does not contain missing values", expect_false(any(is.na(dummy_output))))

test_that("Check number of rows in output", expect_equal(nrow(dummy_output), 12))

test_that("Check number of columns in output", expect_equal(ncol(dummy_output), 3))

test_that("Output column names are correct", expect_equal(colnames(dummy_output), c("Component",
                                                                                    "Type",
                                                                                    "Count")))

test_that("Labels are in correct order",{
  expect_identical(dummy_output[[1]], 
                   factor(c("Documentation",
                            "Peer review",
                            "Team open source code",
                            "Use open source software",
                            "Version control",
                            "Code packages",
                            "Continuous integration",
                            "Dependency management",
                            "Follow code style guidelines",
                            "Function documentation",
                            "Functions",
                            "Unit testing"), 
                          levels = c("Documentation",
                                     "Peer review",
                                     "Team open source code",
                                     "Use open source software",
                                     "Version control",
                                     "Code packages",
                                     "Continuous integration",
                                     "Dependency management",
                                     "Follow code style guidelines",
                                     "Function documentation",
                                     "Functions",
                                     "Unit testing")))
})


test_that("Check output values are correct",{
  expect_equal(dummy_output[[2]], c("Basic", "Basic", "Basic", "Basic", "Basic",
                                    "Advanced", "Advanced", "Advanced", "Advanced", "Advanced", "Advanced", "Advanced"))
  expect_equal(dummy_output[[3]], c(16, 16, 20, 16, 20, 16, 16, 20, 20, 20, 20, 16))
  
})

# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(carsurvey2::plot_grouped(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})
