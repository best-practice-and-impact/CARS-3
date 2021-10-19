dummy_data <- data.frame(learn_before_current_role = c("No", "Yes"),
                         where_learned_to_code = c("Self-taught", "In education"))
  
expected_output <- data.frame(learn_before_current_role = dummy_data$learn_before_current_role,
                              where_learned_to_code = c(NA, "In education"))

dummy_output <- enforce_prev_exp_streaming(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})