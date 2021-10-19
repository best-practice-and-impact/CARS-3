dummy_data <- data.frame(experience_outside_role = c("No", "Yes"), 
                         ability_change = c("Significantly worse", "Significantly better"), 
                         learn_before_current_role = c("Yes", "No"), 
                         where_learned_to_code = c("In public sector employment", "Self-taught"),
                         dummy_variable = c("dummy1", "dummy2"))
  
expected_output <- data.frame(experience_outside_role = c("No", "Yes"), 
                              ability_change = c(NA, "Significantly better"), 
                              learn_before_current_role = c(NA, "No"), 
                              where_learned_to_code = c(NA, "Self-taught"),
                              dummy_variable = c("dummy1", "dummy2"))
  
dummy_output <- enforce_prev_exp_streaming(dummy_data)
  
test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})