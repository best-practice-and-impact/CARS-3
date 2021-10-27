# Generrate dummy dataset for CARS 2021

library(magrittr)

# Ingest data

raw_data <- carsurvey2::ingest(survey = "961613", export = "2028104", check_hash = "e95fc0c3dc2514a0ef1966902221cc13bdbd9224")

tidied_data <- carsurvey2::convert_raw(raw_data) %>% carsurvey2::tidy_ingest()

data <- tidied_data %>% carsurvey3::rename_cols() %>% carsurvey3::enforce_streaming() %>% carsurvey3::derive_vars()

# Functions


extract_vals <- function(data, col_name) {
  
  counts <- data.frame(table(data[col_name]))
  
  if (nrow(counts) == 0) {
    return("empty")
  }
  
  counts <- counts[counts[[2]] > 20, ]
  
  if (nrow(counts) == 0 | col_name == "tracking.link") {
    return("text")  
  } else {
    return(counts[[1]])
  }
}

sample_vals <- function(vals, n_rows) {
  
  if (vals == "text") {
    return(sample(lexicon::cliches, n_rows, replace=TRUE))
  } else if (vals == "empty") {
    return(rep(NA, n_rows))
  } else {
    set.seed(c(1:n_rows))
    vals <- as.character(vals)
    return(sample(vals, n_rows, replace=TRUE))
  }
  
}

# Generate dataset

cols <- colnames(data)
vals <- lapply(cols, function(x) extract_vals(data, x))

dummy_data <- data.frame(lapply(vals, function(x) sample_vals(x, 100)))

colnames(dummy_data) <- cols