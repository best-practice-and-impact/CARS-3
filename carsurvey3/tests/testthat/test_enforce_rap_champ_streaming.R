dummy_data <- data.frame(RAP_champion = c("Yes", "No"), 
                         RAP_knowledge = c("I know who the RAP champion in my department is",
                                           "I don't know what a RAP champion is"),
                         dummy = c("dummy1", "dummy2")
)

expected_output <- data.frame(RAP_champion = c("Yes", "No"), 
                              RAP_knowledge = c(NA,
                                                "I don't know what a RAP champion is"),
                              dummy = c("dummy1", "dummy2")
)

dummy_output <- enforce_rap_champ_streaming(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})