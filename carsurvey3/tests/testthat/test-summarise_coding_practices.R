dummy_data <- data.frame(use_open_source = c(rep("Never", 3), rep("Sometimes", 2), rep(NA, 1)),
                         open_source_code = c(rep("Sometimes", 3), rep("I don't understand this question", 2), rep("All the time", 1)),
                         version_control = c(rep("Rarely", 3), rep("All the time", 2), rep("Never", 1)),
                         code_reviews = c(rep("Regularly", 3), rep("All the time", 2), rep("Never", 1)),
                         functions = c(rep("I don't understand this question", 3), rep("Never", 2), rep("Rarely", 1)),
                         unit_tests = c(rep("All the time", 3), rep("Rarely", 2), rep("Never", 1)),
                         packages = c(rep("Never", 3), rep("Sometimes", 2), rep("Rarely", 1)), 
                         standard_dir_structure = c(rep("Sometimes", 3), rep("Rarely", 2), rep("Never", 1)), 
                         follow_code_guidelines = c(rep("Rarely", 3), rep("Never", 2), rep("Sometimes", 1)), 
                         automated_QA = c(rep("Regularly", 3), rep("Sometimes", 2), rep("Never", 1)),
                         AQUA_book = c(rep("I don't understand this question", 3), rep("Never", 2), rep("Sometimes", 1)))

dummy_output <- summarise_coding_practices(dummy_data)

test_that("output is a dataframe", expect_s3_class(dummy_output, "data.frame"))

test_that("output has seven columns", expect_equal(ncol(dummy_output), 7))

test_that("output has eleven rows", expect_equal(nrow(dummy_output), 11))

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
  expect_equal(dummy_output[[1]], c("I use open source software when programming",
                                    "My team open sources its code",
                                    "I use a source code version control system e.g. Git",
                                    "Code my team writes is reviewed by a colleague",
                                    "I write repetitive elements in my code as functions",
                                    "I unit test my code",
                                    "I collect my code and supporting material into packages",
                                    "I follow a standard directory structure when programming",
                                    "I follow coding guidelines or style guides when programming",
                                    "I write code to automatically quality assure data",
                                    "My team applies the principles set out in the Aqua book when carrying out analysis as code"))
})

test_that("frequencies are correct", {
  expect_equal(dummy_output[[2]], c(0, 2, 0, 0, 3, 0, 0, 0, 0, 0, 3)/8)
  expect_equal(dummy_output[[3]], c(3, 0, 1, 1, 2, 1, 3, 1, 2, 1, 2)/17)
  expect_equal(dummy_output[[4]], c(0, 0, 3, 0, 1, 2, 1, 2, 3, 0, 0)/12)
  expect_equal(dummy_output[[5]], c(2, 3, 0, 0, 0, 0, 2, 3, 1, 2, 1)/14)
  expect_equal(dummy_output[[6]], c(0, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0)/6)
  expect_equal(dummy_output[[7]], c(0, 1, 2, 2, 0, 3, 0, 0, 0, 0, 0)/8)
})

# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(carsurvey2::plot_stacked(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})

