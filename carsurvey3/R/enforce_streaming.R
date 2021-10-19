#'@title API to enforce streaming rules
#'
#'@description Enforce all streaming rules across all questions
#'
#'@param data pre-processed data
#'
#'@return data
#'
#'@export

enforce_streaming <- function(data){
  
  if (class(data) != "data.frame") {
    stop("Unexpected input: data is not a data.frame.")
  
  data <- enforce_degree_streaming(data)
  data <- enforce_code_freq_streaming(data)
  data <- enforce_outside_role_streaming(data)
  data <- enforce_prev_exp_streaming(data)
  data <- enforce_heard_of_rap_streaming(data)
  data <- enforce_rap_champ_streaming(data)
  
  return(data)
}


#'@title Enforce degree streaming
#'
#'@description Enforce the streaming rules that if highest qualification isn't degree then no degree subject has "Yes" returned 
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_degree_streaming <- function(data){
  
  degree_data <- data[grepl("degree_", colnames(data))]
  degree_data[!is.na(data$highest_qualification) & !(data$highest_qualification %in% c("Bachelor's degree (or equivalent)","Master's degree (or equivalent)","Doctoral degree (or equivalent)")),] <- "No"
  data[colnames(degree_data)] <- degree_data
  
  return(data)
}

#'@title Enforce code frequency streaming
#'
#'@description Enforce the streaming rules that if code frequency is never then multiple questions are skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_code_freq_streaming <- function(data){
  
  data <- enforce_codefreq_coding_prac(data)
  data <- enforce_codefreq_doc(data)
  data <- enforce_codefreq_ci(data)
  data <- enforce_codefreq_dep_man(data)
  data <- enforce_codefreq_rep_wf(data)
  data <- enforce_codefreq_comment(data)
  
  return(data)
  
}

#'@title Enforce code frequency streaming on coding practices
#'
#'@description Enforce the streaming rules that if code frequency is never then coding practices is skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_codefreq_coding_prac <- function(data){
  
  coding_prac_data <- dplyr::select(data, use_open_source:AQUA_book)
  coding_prac_data[!is.na(data$code_freq) & (data$code_freq %in% c("Never")),] <- NA
  data[colnames(coding_prac_data)] <- coding_prac_data
  
  return(data)
}

#'@title Enforce code frequency streaming on documentation questions
#'
#'@description Enforce the streaming rules that if code frequency is never then documentation practices is skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_codefreq_doc <- function(data){
  
  doc_data <- dplyr::select(data, desk_notes:other_docs)
  doc_data[!is.na(data$code_freq) & (data$code_freq %in% c("Never")),] <- NA
  data[colnames(doc_data)] <- doc_data
  
  return(data)
}

#'@title Enforce code frequency streaming on continuous integration questions
#'
#'@description Enforce the streaming rules that if code frequency is never then continuous integration questions is skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_codefreq_ci <- function(data){
  
  data$CI[!is.na(data$code_freq) & (data$code_freq %in% c("Never"))] <- NA
  
  return(data)
}

#'@title Enforce code frequency streaming on dependency management questions
#'
#'@description Enforce the streaming rules that if code frequency is never then dependency management questions is skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_codefreq_dep_man <- function(data){
  
  data$dependency_management[!is.na(data$code_freq) & (data$code_freq %in% c("Never"))] <- NA
  
  return(data)
}

#'@title Enforce code frequency streaming on reproducible workflow questions
#'
#'@description Enforce the streaming rules that if code frequency is never then reproducible workflow questions is skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_codefreq_rep_wf <- function(data){
  
  data$reproducible_workflow[!is.na(data$code_freq) & (data$code_freq %in% c("Never"))] <- NA
  
  return(data)
}

#'@title Enforce code frequency streaming on extra comment question
#'
#'@description Enforce the streaming rules that if code frequency is never then extra comment question is skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_codefreq_comment <- function(data){
  
  data$coding_practices_comments[!is.na(data$code_freq) & (data$code_freq %in% c("Never"))] <- NA
  
  return(data)
}

#'@title Enforce coding experience outside current role streaming
#'
#'@description Enforce the streaming rules that if coding experience outside current role is no then follow up questions are skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_outside_role_streaming <- function(data){
  
  prev_exp_data <- dplyr::select(data, "ability_change":"where_learned_to_code")
  prev_exp_data[!is.na(data$experience_outside_role) & (data$experience_outside_role %in% c("No")),] <- NA
  data[colnames(prev_exp_data)] <- prev_exp_data
  
  return(data)
}

#'@title Enforce previous coding experience streaming
#'
#'@description Enforce the streaming rules that if previous coding experience is no then where learned to code is skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_prev_exp_streaming <- function(data){
  
  data$where_learned_to_code[!is.na(data$learn_before_current_role) & (data$learn_before_current_role %in% c("No"))] <- NA
  
  return(data)
}

#'@title Enforce heard of RAP streaming
#'
#'@description Enforce the streaming rules that if heard of RAP is "No" then skip various RAP based questions
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_heard_of_rap_streaming <- function(data){

  data <- enforce_rap_champ_knowl_streaming(data)
  data <- enforce_outside_role_streaming(data)

  return(data)
}

#'@title Enforce heard of RAP streaming on RAP_champion and RAP_knowledge
#'
#'@description Enforce the streaming rules that if heard of RAP is "No" then are you RAP_champion and RAP_knowledge are skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_rap_champ_knowl_streaming <- function(data){
  
  rap_data <- data[,c("RAP_champion","RAP_knowledge")]
  rap_data[!is.na(data$heard_of_RAP) & (data$heard_of_RAP %in% c("No")),] <- NA
  data[colnames(rap_data)] <- rap_data
  
  return(data)
}

#'@title Enforce heard of RAP streaming on RAP statements
#'
#'@description Enforce the streaming rules that if heard of RAP is "No" then statements on RAP are skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_rap_state_streaming <- function(data){
  
  statement_data <- dplyr::select(data, "RAP_confident":"RAP_comments")
  statement_data[!is.na(data$heard_of_RAP) & (data$heard_of_RAP %in% c("No")),] <- NA
  data[colnames(statement_data)] <- statement_data
  
  return(data)
}

#'@title Enforce RAP champion streaming
#'
#'@description Enforce the streaming rules that if RAP champion is yes then Do you know your RAP champion is skipped
#'
#'@param data pre-processed data
#'
#'@return data frame

enforce_rap_champ_streaming <- function(data){
  
  data$RAP_knowledge[!is.na(data$RAP_champion) & (data$RAP_champion %in% c("Yes"))] <- NA
  
  return(data)
}

