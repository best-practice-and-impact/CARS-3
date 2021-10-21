#' Summarise coding frequency
#'
#' @description calculate frequency table for coding frequency. 
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return
#' @export
#'
#' @examples
summarise_code_freq <- function(data) {
  
  # Validation checks
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq")
  }
  
  data$code_freq <- factor(data$code_freq, levels = c("Never",
                                                      "Rarely",
                                                      "Sometimes", 
                                                      "Regularly",
                                                      "All the time"))
  
  freqs <- data.frame(table(data$code_freq))
  
  colnames(freqs) <- c("Coding frequency", "Count")
  return(freqs)
}


#' Summarise data operations
#'
#' @description calculate frequency table for data operations
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return
#' @export
#'

summarise_operations <- function(data) {
  
  selected_data <- dplyr::select(data, .data$operations_analysis:.data$operations_modelling)
  
  frequencies <- apply(selected_data, 2, function(x) {
    x <- factor(x, levels = c("I do some or all of this by coding", "I do this without coding"))
    
    table(x)
  })
  
  labels <- c("Analysis", "Data cleaning", "Data linking", "Data transfer", "Data visualisastion", "Machine learning", "Modelling")
  
  frequencies <- data.frame("Data operation" = labels, t(frequencies))
  
  # Replace full stops with spaces
  colnames(frequencies) <- gsub("[.]", " ", colnames(frequencies))
  
  rownames(frequencies) <- NULL
  
  return(frequencies)
  
}


#' Summarise coding tools
#'
#' @description calculate frequency table coding tools (knowledge or access)
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#' @param type type of table (knowledge or access)
#'
#' @return
#' @export
#'

summarise_coding_tools <- function(data, type = list("knowledge", "access")) {
  type <- match.arg(type, several.ok = FALSE)
  
  selected_data <- data[grepl(paste0(type, "_"), colnames(data))]
  
  frequencies <- apply(selected_data, 2, function(x) {
    x <- factor(x, levels = c("Yes", "Don't Know", "No"))
    
    table(x)
  })
    
  languages <- c(
    "R", 
    "SQL",
    "SAS",
    "VBA",
    "Python", 
    "SPSS", 
    "Stata",
    "Javascript / Typescript",
    "Java / Scala",
    "C++ / C#",
    "Matlab"
  )
  
  frequencies <- data.frame("Programming language" = languages, t(frequencies), check.names = FALSE)
  
  rownames(frequencies) <- NULL
  
  frequencies <- frequencies[order(languages), ]
  
  return(frequencies)
}

#'@title Summarise where respondents learned to code 
#'
#'@description calculate frequency table of where respondents learned to code
#'
#'@param data full CARS wave 3 data.frame after preprocessing
#'
#'@return frequency table
#'@export 
#'

summarise_where_learned_code <- function(data){
  
  # Validation checks
  if (!"where_learned_to_code" %in% colnames(data)) {
    stop("unexpected_input: no column called 'where_learned_to_code")
  }
  if (!"where_learned_to_code" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq")
  }
  if (!"where_learned_to_code" %in% colnames(data)) {
    stop("unexpected_input: no column called 'learn_before_current_role")
  }
  
  
  data$where_learned_to_code[(is.na(data$learn_before_current_role) |
                               (data$learn_before_current_role == "No")) &
                               data$code_freq != "Never"] <- "In current role"
  
  levels = c("In current role",
             "In education",
             "In private sector employment",
             "In public sector employment",
             "Self-taught",
             "Other")
  
  data$where_learned_to_code[!is.na(data$where_learned_to_code) &
                               !(data$where_learned_to_code %in% levels)] <- "Other"
  
  data$where_learned_to_code <- factor(data$where_learned_to_code, levels = levels)
  
  freqs <- data.frame(table(data$where_learned_to_code))
  
  colnames(freqs) <- c("First coding experience", "Count")
  
  return(freqs)
}


#' @title Summarise data practices questions
#'
#' @description calculate frequency table for data practices
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table 
#' @export
#'

summarise_coding_practices <- function(data) {
  
  selected_data <- dplyr::select(data, .data$use_open_source:.data$AQUA_book )
  
  levels <- c("I don't understand this question",
              "Never",
              "Rarely",
              "Sometimes",
              "Regularly",
              "All the time")
  
  labels <- c("I use open source software when programming",
              "My team open sources its code",
              "I use a source code version control system e.g. Git",
              "Code my team writes is reviewed by a colleague",
              "I write repetitive elements in my code as functions",
              "I unit test my code",
              "I collect my code and supporting material into packages",
              "I follow a standard directory structure when programming",
              "I follow coding guidelines or style guides when programming",
              "I write code to automatically quality assure data",
              "My team applies the principles set out in the Aqua book when carrying out analysis as code")
  
  frequencies <- carsurvey3::calc_multi_col_freqs(data = selected_data, levels = levels, labels = labels, calc_props = TRUE)
 
  colnames(frequencies) <- c("Question", levels)
  
  return(frequencies)
  
}