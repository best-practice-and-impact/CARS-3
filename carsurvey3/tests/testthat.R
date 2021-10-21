library(testthat)
library(carsurvey3)

tryCatch(
  error = function(e) {
    library(carsurvey2, lib.loc = "/Users/runner/work/_temp/Library")
  }
)

test_check("carsurvey3")