#' @title Render main site
#'
#' @description Processes the Smart survey data to generate a series of tables. 
#' Then removes the old site and uses the generated tables to render a new site.
#' Uses the markdown files located in the internal var markdown_file_path.
#'
#' @param data This is generated using the carsurvey2::data_ functions.
#' @param markdown_file_path the path containing the rmarkdown site documents
#'
#' @export 


render_main_site <- function(data, markdown_file_path = "rmarkdown/main") {
  
  knitr::opts_chunk$set(warning = FALSE)
  
  samples <- list(
    all = nrow(data),
    coders = sum(data$code_freq != "Never"),
    heard_of_rap = sum(data$heard_of_RAP == "Yes"),
    code_outside_current_role = sum(data$experience_outside_role == "Yes"),
    any_code_experience = sum(data$experience_outside_role == "Yes" | data$code_freq != "Never")
  )
  
  # Remove old site and knit
  knitr::opts_chunk$set(message = FALSE, warning = FALSE)
  rmarkdown::clean_site(markdown_file_path, preview = FALSE)
  rmarkdown::render_site(markdown_file_path, quiet = TRUE)
  
}