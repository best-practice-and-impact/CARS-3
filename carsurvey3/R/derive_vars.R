#' Derive variables
#'
#' @description API function for deriving additional variables.
#' 
#' @param data tidied and relabelled CARS wave 3 dataset.
#' 
#' @return data (data.frame).
#' 
#' @export

derive_vars <- function(data) {
  data <- data %>%
    derive_language_status %>%
    derive_rap_score
}

#' Derive language status
#'
#' @description Derve the status of each programmming language as "access" (access only), "knowledge" (knowledge only), "both" or "neither".  
#'
#' @param data tidied CARS wave 3 data (data.frame).
#'
#' @return data.frame

derive_language_status <- function(data) {
  lang_list <- colnames(data)[grepl("access_", colnames(data))]
  
  lang_list <- gsub("access_", "", lang_list)
  
  new_vars <- sapply(lang_list, function(lang) {
    access_col <- data[paste0("access_", lang)]
    
    knowledge_col <- data[paste0("knowledge_", lang)]
    
    dplyr::case_when(access_col == "Yes" & knowledge_col == "Yes" ~ "both",
                     access_col == "Yes" & knowledge_col != "Yes" ~ "access",
                     access_col != "Yes" & knowledge_col == "Yes" ~ "knowledge",
                     access_col != "Yes" & knowledge_col!= "Yes" ~ "neither")
  })
  
  colnames(new_vars) <- paste0("status_", lang_list)
  
  return(data.frame(data, new_vars))
}

#'@title Derive RAP scores
#'
#'@description Derive basic and advanced RAP score columns from existing variables and add to the dataframe.
#'
#'@param data a date frame containing cleaned CARS wave 3 data
#'
#'@return dataframe containing the additional RAP score columns
#'

derive_rap_score <- function(data){
  data <- derive_basic_rap_scores(data)
  data <- derive_advanced_rap_scores(data)
  
  return(data)
}


#'@title Derive basic RAP scores
#'
#'@description Derive basic RAP score columns from existing variables and add to the dataframe.
#'
#'@param data a date frame containing cleaned CARS wave 3 data
#'
#'@return dataframe containing the additional basic RAP score columns
#'

derive_basic_rap_scores <- function(data) {
  
  expected_columns <- c("use_open_source",
                        "open_source_code",
                        "version_control",
                        "code_reviews",
                        "AQUA_book",
                        "readme", 
                        "code_comments")
  
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_columns %in% colnames(data))) {
    missing <- paste(expected_columns[!(expected_columns %in% colnames(data))], collapse = "\n")
    stop(
      paste0("Unexpected input - missing column names: ", missing)
    )
  }
  
  high_vals <- c("Regularly", "All the time")
  
  data$use_open_source_score <- ifelse(data$use_open_source %in% high_vals, 1, 0)
  data$open_code_score <- ifelse(data$open_source_code %in% high_vals, 1, 0)
  data$version_control_score <- ifelse(data$version_control %in% high_vals, 1, 0)
  data$peer_review_score <- ifelse(data$code_reviews %in% high_vals, 1, 0)
  data$AQUA_book_score <- ifelse(data$AQUA_book %in% high_vals, 1, 0)
  data$doc_score <- ifelse(data$readme %in% high_vals & data$code_comments %in% high_vals, 1, 0)
  
  data$basic_rap_score <- rowSums(data[,c("use_open_source_score",
                                          "open_code_score",
                                          "version_control_score",
                                          "peer_review_score", 
                                          "AQUA_book_score", 
                                          "doc_score")])
  return(data)
  
}


#'@title Derive advanced RAP scores
#'
#'@description Derive advanced RAP score columns from existing variables and add to the dataframe.
#'
#'@param data a date frame containing cleaned CARS wave 3 data
#'
#'@return dataframe containing the additional advanced RAP score columns
#'

derive_advanced_rap_scores <- function(data) {
  
  expected_columns <- c("functions",
                        "unit_tests",
                        "function_docs",
                        "packages",
                        "follow_code_guidelines",
                        "CI", 
                        "dependency_management")
  
  if (!is.data.frame(data)) {
    stop("Unexpected input - data should be a data frame")
  } else if (!all(expected_columns %in% colnames(data))) {
    missing <- paste(expected_columns[!(expected_columns %in% colnames(data))], collapse = "\n")
    stop(
      paste0("Unexpected input - missing column names: ", missing)
    )
  }
  
  high_vals <- c("Regularly", "All the time")
  
  data$function_score <- ifelse(data$functions %in% high_vals, 1, 0)
  data$unit_test_score <- ifelse(data$unit_tests %in% high_vals, 1, 0)
  data$function_doc_score <- ifelse(data$function_docs %in% high_vals, 1, 0)
  data$package_score <- ifelse(data$packages %in% high_vals, 1, 0)
  data$code_style_score <- ifelse(data$follow_code_guidelines %in% high_vals, 1, 0)
  data$cont_integreation_score <- ifelse(data$CI == "Yes", 1, 0)
  data$dep_management_score <- ifelse(data$dependency_management == "Yes", 1, 0)
  
  data$advanced_rap_score <- rowSums(data[,c("function_score", 
                                             "unit_test_score", 
                                             "function_doc_score", 
                                             "package_score", 
                                             "code_style_score", 
                                             "cont_integreation_score", 
                                             "dep_management_score")])
  return(data)
  
}