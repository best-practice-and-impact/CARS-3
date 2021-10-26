# Data operations table
# Frequency table should not include missing values and should include counts of 0

dummy_data <- data.frame(
  knowledge_R = c("Yes", "Yes"),
  access_R = c("No", "No"),
  knowledge_SQL = c("Yes", "Yes"),
  access_SQL = c("Don't know", "No"),
  knowledge_SAS = c("Yes", "Yes"),
  access_SAS = c("Yes", "Yes"),
  knowledge_VBA = c("No", "No"),
  access_VBA = c("Yes", "Yes"),
  knowledge_python = c("No", "No"),
  access_python = c("Don't know", "Don't know"),
  knowledge_SPSS = c("No", "No"),
  access_SPSS = c("No", "Yes"),
  knowledge_stata = c("Don't know", "No"),
  access_stata = c("Yes", "No"),
  knowledge_JS = c("Don't know", "Yes"),
  access_JS = c("No", "Yes"),
  knowledge_java_scala = c("Don't know", "Yes"),
  access_java_scala = c("Don't know", "No"),
  knowledge_C = c("Yes", "Don't know"),
  access_C = c("No", "Yes"),
  knowledge_matlab = c("Yes", "No"),
  access_matlab = c("Yes", "No")
)

languages <- c(
  "C++ / C#",
  "Java / Scala",
  "Javascript / Typescript",
  "Matlab",
  "Python",
  "R",
  "SAS",
  "SPSS",
  "SQL",
  "Stata",
  "VBA"
)

dummy_data <- derive_language_status(dummy_data)

dummy_output <- summarise_language_status(dummy_data)

test_that("output is a dataframe", expect_s3_class(dummy_output, "data.frame"))

test_that("output has four columns", expect_equal(ncol(dummy_output), 4))

test_that("output does not contain missing values", expect_false(any(is.na.data.frame(dummy_output))))

test_that("column names are correct", expect_equal(colnames(dummy_output), c("Programming language", "Access only", "Access and knowledge", "Knowledge only")))

test_that("cell values are correct", {
  selected_data <- dplyr::select(dummy_data, status_R:status_matlab)
  
  selected_data <- selected_data[order(colnames(selected_data))]
  
  expect_equal(dummy_output[[1]], languages)
  expect_true(all(dummy_output[[2]] == colSums(selected_data == "access")))
  expect_true(all(dummy_output[[3]] == colSums(selected_data == "both")))
  expect_true(all(dummy_output[[4]] == colSums(selected_data == "knowledge")))
})


# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(plot_stacked(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})
