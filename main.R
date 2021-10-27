library(magrittr)

# Ingest data

raw_data <- carsurvey2::ingest(survey = "961613", export = "2028104", check_hash = "e95fc0c3dc2514a0ef1966902221cc13bdbd9224")

tidied_data <- carsurvey2::convert_raw(raw_data) %>% carsurvey2::tidy_ingest()

data <- tidied_data %>% carsurvey3::rename_cols() %>% carsurvey3::enforce_streaming() %>% carsurvey3::derive_vars()

# Render site

carsurvey2::render_navbar("rmarkdown/main/_site.yml") %>% carsurvey2::save_navbar("rmarkdown/main")
