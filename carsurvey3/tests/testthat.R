library(testthat)
library(carsurvey3)

tryCatch(
  library(package = carsurvey2),
  error = function(e) {
    devtools::load_all("../../../.github/CARS2-master") # Used by GitHub actions
  }
)

test_check("carsurvey3")