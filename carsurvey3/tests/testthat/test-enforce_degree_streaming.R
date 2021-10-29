dummy_data <- data.frame(highest_qualification = c("Doctoral degree (or equivalent)", 
                                                   "Master's degree (or equivalent)", 
                                                   "Bachelor's degree (or equivalent)",
                                                   NA,
                                                   "A-level",
                                                   "test"),
                         degree_maths = c("Yes", "Yes", "No", "Yes", "Yes", "No"),
                         degree_english = c("Yes", "Yes", "No", "Yes", "Yes", "No"),
                         dummy = c("Yes", "Yes", "No", "Yes", "Yes", "No")
)

expected_output <- data.frame(highest_qualification = dummy_data$highest_qualification,
                              degree_maths = c("Yes", "Yes", "No", "Yes", "No", "No"),
                              degree_english = c("Yes", "Yes", "No", "Yes", "No", "No"),
                              dummy = dummy_data$dummy
)

dummy_output <- enforce_degree_streaming(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})
