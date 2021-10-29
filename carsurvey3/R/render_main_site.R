# High Level functions that are called to build the site. Prefixed render_

#' @title Render main site
#'
#' @description Processes the Smart survey data to generate a series of tables. 
#' Then removes the old site and uses the generated tables to render a new site.
#' Uses the markdown files located in the internal var markdown_file_path.
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#' @param markdown_file_path the path containing the rmarkdown site documents
#'
#' @export 

render_main_site <- function(data, markdown_file_path = "rmarkdown/main") {
  
  knitr::opts_chunk$set(warning = FALSE)
   
  samples <- list(
    all = nrow(data),
    coders = sum(data$code_freq != "Never"),
    heard_of_RAP = sum(data$heard_of_RAP == "Yes"),
    code_outside_current_role = sum(data$experience_outside_role == "Yes"),
    any_code_experience = sum(data$experience_outside_role == "Yes" | data$code_freq != "Never")
  )
  
  # Remove old site and knit
  knitr::opts_chunk$set(message = FALSE, warning = FALSE)
  rmarkdown::clean_site(markdown_file_path, preview = FALSE)
  rmarkdown::render_site(markdown_file_path, quiet = TRUE)
  
}

#' @title Render navigation bar
#'
#' @description Creates the site navbar.
#' 
#' @param yml_path Path to the rmarkdown site yml config
#' 
#' @return navbar_page html code for navbar
#' 
#' @export

render_navbar <- function(yml_path = "rmarkdown/main/_site.yml") {
  
  # Create navigation bar
  navbar_info <- read_site_yml(yml_path)
  navbar_page <- build_navbar(navbar_info)
  
  return(navbar_page)
}

#' @title Save navigation bar
#'
#' @description Saves the site navbar.
#' 
#' @param code html code (string)
#' @param path path for saving the navigation bar (excluding file name)
#' 
#' @export

save_navbar <- function(code, path) {
  filename <- paste(path, "_navbar.html", sep = "/")
  write(code, filename)
}