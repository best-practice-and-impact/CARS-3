# Ingest data

raw_data <- carsurvey2::ingest(survey = "961613", export = "2012405", check_hash = FALSE)
tidied_data <- carsurvey2::tidy_ingest(carsurvey2::convert_raw(raw_data))

data <- carsurvey3::rename_cols(tidied_data)