library(testthat)
library(carsurvey3)

tryCatch(
  library(package = carsurvey2),
  error = function(e) {
    print("carsurvey2 not installed")
  }
)

test_check("carsurvey3")