dummy_data <- data.frame(heard_of_RAP = c("Yes", "No"),
                         RAP_confident = c("Strongly Agree", "Strongly Disagree"),
                         RAP_supported = c("Strongly Agree", "Strongly Disagree"),
                         RAP_resources = c("Strongly Agree", "Strongly Disagree"),
                         RAP_understand_key_components = c("Strongly Agree", "Strongly Disagree"),
                         RAP_important = c("Strongly Agree", "Strongly Disagree"), 
                         RAP_implementing = c("Strongly Agree", "Strongly Disagree"), 
                         RAP_planning_to_implement = c("Strongly Agree", "Strongly Disagree"),
                         RAP_comments = c("Strongly Agree", "Strongly Disagree")
                         )

expected_output <- data.frame(heard_of_RAP = c("Yes", "No"),
                         RAP_confident = c("Strongly Agree", NA),
                         RAP_supported = c("Strongly Agree", NA),
                         RAP_resources = c("Strongly Agree", NA),
                         RAP_understand_key_components = c("Strongly Agree", NA),
                         RAP_important = c("Strongly Agree", NA), 
                         RAP_implementing = c("Strongly Agree", NA), 
                         RAP_planning_to_implement = c("Strongly Agree", NA),
                         RAP_comments = c("Strongly Agree", NA)
                         )

dummy_output <- enforce_rap_state_streaming(dummy_data)

test_that("Check dummy_output and expected_output are identical",{
  expect_true(identical(dummy_output, expected_output))
})