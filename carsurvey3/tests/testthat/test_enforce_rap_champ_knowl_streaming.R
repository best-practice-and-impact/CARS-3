dummy_data <- data.frame(heard_of_RAP = c("Yes", "No"), 
                         RAP_champion = c("No", "Yes"),
                         RAP_knowledge = c("I know who the RAP champion in my department is",
                                           "I don't know what a RAP champion is"))

expected_output <- data.frame(heard_of_RAP = dummy_data$heard_of_RAP, 
                            RAP_champion = c("No", NA),
                            RAP_knowledge = c("I know who the RAP champion in my department is",
                                              NA))

dummy_output <- enforce_rap_champ_knowl_streaming(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})
