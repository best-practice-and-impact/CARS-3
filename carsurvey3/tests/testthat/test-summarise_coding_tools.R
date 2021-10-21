# Coding tools frequency tables (access or knowledge)

dummy_data <- data.frame(
  knowledge_R = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_R = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_SQL = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_SQL = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_SAS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_SAS = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_VBA = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_VBA = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_python = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_python = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_SPSS = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_SPSS = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_stata = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_stata = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_JS = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_JS = c("Yes", rep("No", 3), rep("Don't Know", 2)),
  knowledge_java_scala = c(rep("Yes", 2), rep("No", 3), "Don't Know"),
  access_java_scala = c(rep("Yes", 3), "No", rep("Don't Know", 2)),
  knowledge_C = c("Yes", rep("No", 2), rep("Don't Know", 3)),
  access_c = c(rep("Yes", 2), "No", rep("Don't Know", 3)),
  knowledge_matlab = c(rep("Yes", 3), rep("No", 2), "Don't Know"),
  access_matlab = c("Yes", rep("No", 5), rep("Don't Know", 0)) # Used to check zero counts aren't missing
)

dummy_data_missing_values <- dummy_data
dummy_data_missing_values[1,] <- NA

dummy_knowledge_output <- summarise_coding_tools(dummy_data, "knowledge")
dummy_access_output <- summarise_coding_tools(dummy_data, "access")
dummy_output_missing_values <- summarise_coding_tools(dummy_data_missing_values, "knowledge")
  
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

test_that("output is a dataframe", {
  expect_s3_class(dummy_knowledge_output, "data.frame")
  expect_s3_class(dummy_access_output, "data.frame")
})

test_that("output has four columns", {
  expect_equal(ncol(dummy_knowledge_output), 4)
  expect_equal(ncol(dummy_access_output), 4)
})

test_that("output contains no missing values", expect_false(any(is.na(dummy_output_missing_values))))

test_that("output has the correct column names", {
  expect_equal(colnames(dummy_access_output), c("Programming language", "Yes", "Don't Know", "No"))
  expect_equal(colnames(dummy_knowledge_output), c("Programming language", "Yes", "Don't Know", "No"))
})

test_that("programming language names are correct", {
  expect_equal(dummy_access_output[[1]], languages)
  expect_equal(dummy_knowledge_output[[1]], languages)
})

test_that("Frequencies are correct", {
  dummy_data <- dummy_data[order(colnames(dummy_data))] # Sort alphabetically
  access_data <- dummy_data[1:11]
  knowledge_data <- dummy_data[12:22]
  
  expect_equal(dummy_knowledge_output[[2]], unname(colSums(knowledge_data == "Yes")))
  expect_equal(dummy_knowledge_output[[3]], unname(colSums(knowledge_data == "Don't Know")))
  expect_equal(dummy_knowledge_output[[4]], unname(colSums(knowledge_data == "No")))
  
  expect_equal(dummy_access_output[[2]], unname(colSums(access_data == "Yes")))
  expect_equal(dummy_access_output[[3]], unname(colSums(access_data == "Don't Know")))
  expect_equal(dummy_access_output[[4]], unname(colSums(access_data == "No")))
})


# Integration test with plot function 

test_that("output can be used with plotting function", {
  expect_s3_class(plot_stacked(dummy_knowledge_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
  expect_s3_class(plot_stacked(dummy_access_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})