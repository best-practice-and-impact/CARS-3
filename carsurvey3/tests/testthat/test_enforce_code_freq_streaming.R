#Enforce code frequency streaming on coding practices

dummy_data <- data.frame(code_freq = c("Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         use_open_source = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"), 
                         AQUA_book = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"), 
                         variable_3 = c("All the time", "Regularly", "Sometimes", "Rarely", "Never")
                         )
  
expected_output <- data.frame(code_freq = dummy_data$code_freq,
                                use_open_source = c(NA, "Regularly", "Sometimes", "Rarely", "Never"), 
                                AQUA_book = c(NA, "Regularly", "Sometimes", "Rarely", "Never"), 
                                variable_3 = dummy_data$variable_3
                                )

dummy_output <- enforce_codefreq_coding_prac(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})

#Enforce code frequency streaming on documentation questions

dummy_data <- data.frame(code_freq = c("Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         desk_notes = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"), 
                         other_docs = c("All the time", "Regularly", "Sometimes", "Rarely", "Never"), 
                         variable_3 = c("All the time", "Regularly", "Sometimes", "Rarely", "Never")
                         )

expected_outcomes <- data.frame(code_freq = dummy_data$code_freq,
                                desk_notes = c(NA, "Regularly", "Sometimes", "Rarely", "Never"), 
                                other_docs = c(NA, "Regularly", "Sometimes", "Rarely", "Never"), 
                                variable_3 = dummy_data$variable_3
                                )

dummy_outcome <- enforce_codefreq_doc(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})

#Enforce code frequency streaming on continuous integration questions

dummy_data <- data.frame(code_freq = c("Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         CI = c("Yes", "No", "I don't know what continuous integration is", "Yes", "No"), 
                         variable_3 = c("Yes", "No", "I don't know what continuous integration is", "Yes", "No")
                         )

expected_outcomes <- data.frame(code_freq = dummy_data$code_freq,
                                CI = c(NA,  "No", "I don't know what continuous integration is", "Yes", "No"), 
                                variable_3 = dummy_data$variable_3
                                )


dummy_outcome <- enforce_codefreq_ci(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})

#Enforce code frequency streaming on dependency management questions

dummy_data <- data.frame(code_freq = c("Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         depdendency_management = c("Yes", "No", "I don't know what dependency management is", "Yes", "No"), 
                         variable_3 = c("Yes", "No", "I don't know what dependency management is", "Yes", "No")
                         )

expected_outcomes <- data.frame(code_freq = dummy_data$code_freq,
                                depdendency_management = c(NA,  "No", "I don't know what dependency management is", "Yes", "No"), 
                                variable_3 = dummy_data$variable_3
                                )


dummy_outcome <- enforce_codefreq_dep_man(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})

#Enforce code frequency streaming on reproducible workflow questions

dummy_data <- data.frame(code_freq = c("Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         reproducible_workflow = c("Yes", "No", "I don't know what reproducible workflows are", "Yes", "No"), 
                         variable_3 = c("Yes", "No", "I don't know what reproducible workflows are", "Yes", "No")
                         )

expected_outcomes <- data.frame(code_freq = dummy_data$code_freq,
                                reproducible_workflow = c(NA,  "No", "I don't know what reproducible workflows are", "Yes", "No"), 
                                variable_3 = dummy_data$variable_3
                                )


dummy_outcome <- enforce_codefreq_rep_wf(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})

#Enforce code frequency streaming on extra comment question

dummy_data <- data.frame(code_freq = c("Never", "Rarely", "Sometimes", "Regularly", "All the time"),
                         coding_practices_comments = c("comment1", "comment2", "comment3", "comment4", "comment5"), 
                         variable_3 = c("comment1", "comment2", "comment3", "comment4", "comment5")
                         )

expected_outcomes <- data.frame(code_freq = dummy_data$code_freq,
                                coding_practices_comments = c(NA,  "comment2", "comment3", "comment4", "comment5"), 
                                variable_3 = dummy_data$variable_3
                                )


dummy_outcome <- enforce_codefreq_comment(dummy_data)

test_that("output values match expected values", {
  expect_true(identical(dummy_output, expected_output))
})

