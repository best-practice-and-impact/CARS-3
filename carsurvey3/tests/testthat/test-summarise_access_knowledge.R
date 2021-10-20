# Data operations table
# Frequency table should not include missing values and should include counts of 0

dummy_data <- data.frame(
  knowledge_R = c(),
  access_R = c(),
  knowledge_SQL = c(),
  access_SQL = c(),
  knowledge_SAS = c(),
  access_SAS = c(),
  knowledge_VBA = c(),
  access_VBA = c(),
  knowledge_python = c(),
  access_python = c()),
  knowledge_SPSS = c(),
  access_SPSS = c(),
  knowledge_stata = c(),
  access_stata = c(),
  knowledge_JS = c(),
  access_JS = c(),
  knowledge_java_scala = c(),
  access_java_scala = c(),
  knowledge_C = c(),
  access_c = c(),
  knowledge_matlab = c(),
  access_matlab = c()
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
  expect_s3_class(carsurvey2::plot_stacked(dummy_output, xlab = "xlab", ylab = "ylab", n = 6), c("plotly", "htmlwidget"))
})