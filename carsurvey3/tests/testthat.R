library(testthat)
library(carsurvey3)

tryCatch(
  library(package = carsurvey2),
  error = function(e) {
    library(carsurvey2, lib.loc = "/Users/runner/work/_temp/Library")
  }
)

test_check("carsurvey3")