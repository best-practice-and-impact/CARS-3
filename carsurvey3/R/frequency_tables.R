#' Produce all summary tables
#' 
#' @description Produce all summary tables and return as a named list.
#' 
#' @param data full CARS wave 3 data.frame after preprocessing
#' 
#' @return list of frequency tables
#' 
#' @export

summarise_all <- function(data) {
  output_list <- list(
    code_freq = summarise_code_freq(data),
    operations = summarise_operations(data),
    knowledge = summarise_coding_tools(data, "knowledge"),
    access = summarise_coding_tools(data, "access"),
    language_status = summarise_language_status(data),
    where_learned = summarise_where_learned_code(data),
    ability_change = summarise_ability_change(data),
    coding_practices = summarise_coding_practices(data),
    rap_knowlegde = summarise_rap_knowledge(data),
    rap_opinions = summarise_rap_opinions(data),
    basic_rap_scores = summarise_rap_basic(data),
    advanced_rap_scores = summarise_rap_advanced(data),
    rap_components = summarise_rap_comp(data),
    ci = summarise_ci(data),
    dependency_management = summarise_dep_man(data),
    rep_workflow = summarise_rep_workflow(data)
  )
  
  return(output_list)
}


#' Summarise coding frequency
#'
#' @description calculate frequency table for coding frequency. 
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table (data.frame)
#' 

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
#' @return frequency table (data.frame)
#' 
#' @importFrom rlang .data

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
#' @return frequency table (data.frame)
#' 

summarise_coding_tools <- function(data, type = list("knowledge", "access")) {
  type <- match.arg(type, several.ok = FALSE)
  
  selected_data <- data[grepl(paste0(type, "_"), colnames(data))]
  
  frequencies <- data.frame(apply(selected_data, 2, function(x) {
    x <- factor(x, levels = c("Yes", "Don't Know", "No"))
    
    table(x)
  }))
  
  frequencies <- frequencies[order(colnames(frequencies))]

  languages <- c(
    "C++ / C#",
    "Java / Scala",
    "Javascript / Typescript",
    "Matlab",
    "Python",
    "R", 
    "SAS",
    "SPSS", 
    "SQL",
    "Stata",
    "VBA"
  )
  
  frequencies <- data.frame("Programming language" = languages, t(frequencies), check.names = FALSE)
  
  rownames(frequencies) <- NULL
  
  return(frequencies)
}

#' @title Summarise where respondents learned to code 
#'
#' @description calculate frequency table of where respondents learned to code
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
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
  
  # 
  data$where_learned_to_code[(is.na(data$learn_before_current_role) | (data$learn_before_current_role == "No")) & 
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
#' @return frequency table (data.frame)
#'
#' @importFrom rlang .data

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
#' @return frequency table (data.frame)
#' 

summarise_rap_basic <- function(data){
  
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
#' @return frequency table (data.frame)
#' 

summarise_rap_advanced <- function(data){
  
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
#' @return frequency table (data.frame)

summarise_rap_knowledge <- function(data){
  
  # Validation checks
  if (!"heard_of_RAP" %in% colnames(data)) {
    stop("unexpected_input: no column called 'heard_of_RAP")
  }
  
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
#' @return frequency table (data.frame)

summarise_rap_opinions <- function(data) {
  
  # Validation checks
  if (!"heard_of_RAP" %in% colnames(data)) {
    stop("unexpected_input: no column called 'heard_of_RAP")
  }
  
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

#' @title Frequency of documentation use
#' 
#' @description Create frequency table of documentation use
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table (data.frame)

summarise_doc <- function(data) {
  
  # Validation checks
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq")
  }
  
  documentation_data <- data[data$code_freq != "Never", ]
  documentation_data <- dplyr::select(documentation_data, "desk_notes":"flow_charts")
  
  levels = c("I don't understand this question",
             "Never",
             "Rarely",
             "Sometimes",
             "Regularly",
             "All the time")
  
  labels = c("Desk notes",
             "Documentation for each function or class",
             "README files",
             "Analytical Quality Assurance (AQA) logs",
             "Data or assumptions registers",
             "Code comments",
             "Flow charts")
  
  
  freq_documentation_data <- calc_multi_col_freqs(documentation_data, levels = levels, labels = labels, calc_props = TRUE)
  
  colnames(freq_documentation_data) <- c("Question",
                                         levels)
  
  return(freq_documentation_data)
  
}

#' @title RAP score components
#' 
#' @description Create frequency table of basic and advanced RAP score components
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table (data.frame)

summarise_rap_comp <- function(data){
  
  labels <- c("use_open_source_score" = "Use open source software",
              "open_code_score" = "Team open source code",
              "version_control_score" = "Version control",
              "peer_review_score" = "Peer review",
              "doc_score" = "Documentation",
              "function_score" = "Functions",
              "unit_test_score" = "Unit testing",
              "function_doc_score" = "Function documentation",
              "package_score" = "Code packages",
              "code_style_score" = "Follow code style guidelines",
              "cont_integreation_score" = "Continuous integration",
              "dep_management_score" = "Dependency management")
  
  rap_score <- data[grepl("_score", colnames(data))]
  
  components <- rap_score[!colnames(rap_score) %in% c("basic_rap_score", "advanced_rap_score", "AQUA_book_score")]
  components[is.na(components)] <- 0
  components <- data.frame(Component = labels,
                           Type = c(rep("Basic", 5), rep("Advanced", 7)),
                           Count = unname(colSums(components))
  )
  
  rownames(components) <- NULL
  components <- components[order(-rank(components$Type),components$Component),]
  components$Component <- factor(components$Component, levels = components$Component)
  
  return(components)
  
}

#' @title Summarise continuous integration frequency
#'
#' @description calculate frequency table for continuous integration
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table (data.frame)

summarise_ci <- function(data) {
  
  # Validation checks
  if (!"CI" %in% colnames(data)) {
    stop("unexpected_input: no column called 'CI")
  }
  
  data$CI <- factor(data$CI, levels = c("Yes",
                                        "No",
                                        "I don't know what continuous integration is"))

freqs <- data.frame(table(data$CI))

colnames(freqs) <- c("Continuous Integration Frequency", "Count")
return(freqs)
}

#' @title Summarise dependency management frequency
#'
#' @description calculate frequency table for dependency management. 
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table (data.frame)

summarise_dep_man <- function(data) {
  
  # Validation checks
  if (!"dependency_management" %in% colnames(data)) {
    stop("unexpected_input: no column called 'dependency_management")
  }
  
  data$dependency_management <- factor(data$dependency_management, levels = c("Yes",
                                                                              "No",
                                                                              "I don't know what dependency management is"))

freqs <- data.frame(table(data$dependency_management))

colnames(freqs) <- c("Dependency Management Frequency", "Count")
return(freqs)
}

#' @title Summarise dependency_management frequency
#'
#' @description calculate frequency table for dependency_management. 
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table (data.frame)

summarise_rep_workflow <- function(data) {
  
  # Validation checks
  if (!"reproducible_workflow" %in% colnames(data)) {
    stop("unexpected_input: no column called 'reproducible_workflow")
  }
  
  data$reproducible_workflow <- factor(data$reproducible_workflow, levels = c("Yes",
                                                                              "No",
                                                                              "I don't know what reproducible workflows are"))
  
  freqs <- data.frame(table(data$reproducible_workflow))
  
  colnames(freqs) <- c("Reproducible Workflow Frequency", "Count")
  
  return(freqs)
}

#' @title Summarise ability change frequency
#'
#' @description calculate frequency table for ability change 
#'
#' @param data full CARS wave 3 data.frame after preprocessing 
#'
#' @return frequency table (data.frame)
#' 
#' @export

summarise_ability_change <- function(data) {
  
  # Validation checks
  if (!"ability_change" %in% colnames(data)) {
    stop("unexpected_input: no column called 'ability_change")
  }
  
  data$ability_change <- factor(data$ability_change,levels = c("Significantly worse",
                                                               "Slightly worse",
                                                               "No change",
                                                               "Slightly better",
                                                               "Significantly better"))
  
  freqs <- data.frame(table(data$ability_change))
  
  colnames(freqs) <- c("Ability Change Frequency", "Count")
  return(freqs)
}

#' @title Summarise programming language status
#' 
#' @description calculate counts of responents reporting access to, knowledge of, or both for each programming language.
#' 
#' @param data full CARS wave 3 data.frame after preprocessing 
#' 
#' @return frequency table (data.frame)
#' 
#' @importFrom rlang .data

summarise_language_status <- function(data) {
   selected_data <- dplyr::select(data, .data$status_R:.data$status_matlab)
   
   select_data <- selected_data[order(colnames(selected_data))]
   
   frequencies <- data.frame(apply(selected_data, 2, function(x) {
     x <- factor(x, levels = c("access", "both", "knowledge"))
     
     table(x)
   }))
   
   frequencies <- frequencies[order(colnames(frequencies))]
   
   languages <- c(
     "C++ / C#",
     "Java / Scala",
     "Javascript / Typescript",
     "Matlab",
     "Python",
     "R", 
     "SAS",
     "SPSS", 
     "SQL",
     "Stata",
     "VBA"
   )
   
   frequencies <- data.frame(languages, t(frequencies), check.names = FALSE)
   
   colnames(frequencies) <- c("Programming language", "Access only", "Access and knowledge", "Knowledge only")
   
   frequencies
}