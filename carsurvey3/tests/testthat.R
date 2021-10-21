library(testthat)
library(carsurvey3)

tryCatch(
  library(carsurvey2),
  error = function() {
    library(carsurvey2, lib.loc = "/Users/runner/work/_temp/Library")
  }
)

test_check("carsurvey3")