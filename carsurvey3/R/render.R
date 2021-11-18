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
    heard_of_rap = sum(data$heard_of_RAP == "Yes"),
    code_outside_current_role = sum(data$experience_outside_role == "Yes"),
    any_code_experience = sum(data$experience_outside_role == "Yes" | data$code_freq != "Never")
  )
  
  # Remove old site and knit
  knitr::opts_chunk$set(message = FALSE, warning = FALSE)
  rmarkdown::clean_site(markdown_file_path, preview = FALSE)
  rmarkdown::render_site(markdown_file_path, quiet = TRUE)
  
}


#' @title Render filtered pages
#' 
#' @description Creates pages by filter (e.g. by department/profession/grade).
#' 
#' @param data CARS wave 3 dataset.
#' @param filter_variable The variable that the data is filtered on. This is given as a string such as "dept". 
#' The data is filtered by getting all values in the filter_variable greater than 20 and subsetting the data.
#' @param page_title This is ONLY PART of the title. page_title , " profile: ", filter -- 
#' where filter is one element of the list as described above in filter_variable (values over 20) and the page title is this argument.
#' @param output_folder Folder the site is built and saved to
#' @param template_path The path to the template that gets render for each department
#' @export


render_filtered_pages <- function(data,
                                  filter_variable,
                                  page_title = "",
                                  output_folder = "docs",
                                  template_path = "rmarkdown/summary_template/summary.rmd") {
  
  if(!sum(colnames(data) == filter_variable) == 1) stop("filter column: ", filter_variable,
                                                        " doesn't exist in the data provided. \nCheck that filter is equal to a valid column name")
  
  # get grade with sample > 20
  filter_table <- data.frame(
    table(data[filter_variable])
  )
  filter_over_20 <- filter_table[filter_table[2] >= 20, ]
  filter_list = as.character(filter_over_20$Var1)
  
  for (filter in filter_list) {
    
    file_path <- carsurvey2::format_filter_path(filter)
    message("Writing page for ", file_path)
    
    # filter data to just the department
    filtered_data <- data[data[filter_variable] == filter, ]
    if(!nrow(filtered_data) == filter_over_20$Freq[filter_over_20$Var1 == filter]) stop("Filtered data row number is not equal to number of : ", file_path)
    
    title <- paste0(page_title , " profile: ", filter)
    
    filtered_tables <- summarise_all(filtered_data)
    
    samples <- list(
      all = nrow(filtered_data),
      coders = sum(filtered_data$code_freq != "Never"),
      heard_of_rap = sum(filtered_data$heard_of_RAP == "Yes"),
      code_outside_current_role = sum(filtered_data$experience_outside_role == "Yes"),
      any_code_experience = sum(filtered_data$experience_outside_role == "Yes" | filtered_data$code_freq != "Never")
    )
    
    knitr::opts_chunk$set(warning = FALSE)
    
    dir.create(paste0(output_folder, "/", page_title))
    
    rmarkdown::render(template_path, 
                      output_file = paste0("../../", output_folder, "/", page_title, "/", file_path),
                      quiet = TRUE,
                      params = list(
                        title = title
                      ))
  } 
}

#' @title Render profession pages
#' 
#' @description Creates pages by multi-column filter (profession)
#' 
#' @param data CARS wave 3 dataset.
#' @param page_title This is ONLY PART of the title. The title is generated from "DRAFT: ", page_title , " profile: ", filter -- 
#' where filter is one element of the list as described above in filter_variable (values over 20) and the page title is this argument.
#' @param output_folder Folder the site is built and saved to
#' @param template_path The path to the template that gets render for each department
#' @export


render_prof_pages <- function(data,
                              page_title = "",
                              output_folder = "docs",
                              template_path = "rmarkdown/summary_template/summary.rmd") {
  
  if(!exists("data")) stop("Dataframe called data not available. This should be in the function enviroment of render_main_site. Check that this is available in this enviroment.")
  
  data$prof_DS <- ifelse(data$prof_DS_GSG_GORS == "Yes" | data$prof_DS_other == "Yes", "Yes", "No")
  
  profs <- dplyr::select(data, prof_non_CS, prof_DDAT:prof_none_CS, prof_DS)
  
  prof_freqs <- calc_multi_col_freqs(profs, c("Yes", "No"))
  prof_freqs <- prof_freqs[c(1,2)]
  
  prof_freqs <- prof_freqs[prof_freqs[2] >= 20, ]
  prof_freqs[1] <- as.character(prof_freqs[[1]])
  colnames(prof_freqs) <- c("Profession", "Sample size")
  
  recode_vals <- c(
    prof_non_CS = "Non-Civil Service",
    prof_DS = "Government Data Scientist",
    prof_DDAT = "Digital, Data and Technology Profession",
    prof_GAD = "Government Actuarial Profession",
    prof_GES = "Government Economic Service",
    prof_finance = "Government Finance Profession",
    prof_geography = "Government Geography Profession",
    prof_GORS = "Government Operational Research Service",
    prof_GSR = "Government Social Research",
    prof_GSG = "Government Statistician Group",
    prof_none_CS = "Civil Service, no profession membership"
  )
  
  filter_list <- prof_freqs$Profession
  filter_names <- dplyr::recode(prof_freqs$Profession, !!!recode_vals)
  
  for (i in c(1:length(filter_list))) {
    
    filter_col <- filter_list[i]
    name <- filter_names[i]
    
    file_path <- carsurvey2::format_filter_path(name)
    message("Writing page for ", file_path)
    
    filtered_data <- data[data[filter_col] == "Yes", ]
    
    title <- paste0(page_title , " profile: ", name)
    
    filtered_tables <- summarise_all(filtered_data)
    
    samples <- list(
      all = nrow(filtered_data),
      coders = sum(filtered_data$code_freq != "Never"),
      heard_of_rap = sum(filtered_data$heard_of_RAP == "Yes"),
      code_outside_current_role = sum(filtered_data$experience_outside_role == "Yes"),
      any_code_experience = sum(filtered_data$experience_outside_role == "Yes" | filtered_data$code_freq != "Never")
    )
    
    knitr::opts_chunk$set(warning = FALSE)
    
    dir.create(paste0(output_folder, "/profession"))
    
    rmarkdown::render(template_path, 
                      output_file = paste0("../../", output_folder, "/profession/", file_path),
                      quiet = TRUE,
                      params = list(
                        title = title
                      ))
  } 
}
