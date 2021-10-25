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
  
  selected_data <- dplyr::select(data, .data$use_open_source:.data$automated_QA )
  
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
              "I write code to automatically quality assure data")
  
  frequencies <- calc_multi_col_freqs(data = selected_data, levels = levels, labels = labels, calc_props = TRUE)
 
  colnames(frequencies) <- c("Question", levels)
  
  return(frequencies)
  
}

#' @title Summarise basic rap score
#'
#' @description calculate frequency table for basic rap scores
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table 
#' @export
#'

calc_freqs_rap_basic <- function(data){
  
  basic_freqs <- data.frame(table(data$basic_rap_score))
  colnames(basic_freqs) <- c("Basic RAP score", "Count")
  
  return(basic_freqs)
  
}

#' @title Summarise Advanced rap score
#'
#' @description calculate frequency table for Advanced rap scores
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table 
#' @export
#'

calc_freqs_rap_advanced <- function(data){
  
  advanced_freqs <- data.frame(table(data$advanced_rap_score))
  colnames(advanced_freqs) <- c("Advanced RAP score", "Count")
  
  return(advanced_freqs)
  
}

#' @title Knowledge of RAP
#' 
#' @description Create a frequency table of knowledge of RAP
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return data.frame
#' @export

calc_freqs_knowledge_of_rap <- function(data){
  
  data$RAP_knowledge[data$heard_of_RAP == "No"] <- "Have not heard of RAP"
  
  data$RAP_knowledge <- factor(data$RAP_knowledge, levels = c(
    "Have not heard of RAP",                                     
    "I don't know what a RAP champion is",                          
    "I know what a RAP champion is but don't know who the RAP champion in my department is",
    "I know what a RAP champion is and there is no RAP champion in my department",
    "I know who the RAP champion in my department is"
  ))
  
  rap_knowledge <- data.frame(table(data$RAP_knowledge))
  
  colnames(rap_knowledge) <- c("RAP champion knowledge", "Count")
  
  rap_knowledge[1] <- c("Have not heard of RAP",
                        "Heard of RAP, have not heard of RAP champions",
                        "Heard of RAP, does not know department champion",
                        "Heard of RAP champions, no champion in department",
                        "Knows department RAP champion")
  
  rap_knowledge[[1]] <- factor(rap_knowledge[[1]], levels = rap_knowledge[[1]])
  
  return(rap_knowledge)
}

#' @title Opinions of RAP
#' 
#' @description Create frequency table of opinions of RAP
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return data.frame
#' @export

calc_freqs_opinions_of_rap <- function(data) {
  
  opinion_rap_data <- data[data$heard_of_RAP == "Yes", ]
  opinion_rap_data <- dplyr::select(opinion_rap_data, "RAP_confident":"RAP_planning_to_implement")
  
  levels = c("Strongly Disagree",
             "Disagree", 
             "Neutral", 
             "Agree", 
             "Strongly Agree")
  
  labels = c("I feel confident implementing RAP in my work",
             "I feel supported to implement RAP in my work",
             "I know where to find resources to help me implement RAP",
             "I understand what the key components of the RAP methodology are",
             "I think it is important to implement RAP in my work",
             "I and/or my team are currently implementing RAP",
             "I or my team are planning on implementing RAP in the next 12 months")
  
  freq_rap_opinions <- calc_multi_col_freqs(opinion_rap_data, levels = levels, labels = labels, calc_props = TRUE)
  
  colnames(freq_rap_opinions) <- c("Question",
                                   "Strongly disagree",
                                   "Disagree",
                                   "Neutral",
                                   "Agree",
                                   "Strongly agree")
  
  return(freq_rap_opinions)
  
}
