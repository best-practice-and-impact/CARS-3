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
  
  ci_data <- data.frame(data$CI)
  ci_data[!is.na(data$code_freq) & (data$code_freq %in% c("Never")),] <- NA
  data[colnames(ci_data)] <- ci_data
  
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
  
  dep_man_data <- data.frame(data$dependency_management)
  dep_man_data[!is.na(data$code_freq) & (data$code_freq %in% c("Never")),] <- NA
  data[colnames(dep_man_data)] <- dep_man_data
  
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
  
  rep_wf_data <- data.frame(data$reproducible_workflow)
  rep_wf_data[!is.na(data$code_freq) & (data$code_freq %in% c("Never")),] <- NA
  data[colnames(rep_wf_data)] <- rep_wf_data
  
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
  
  comment_data <- data.frame(data$coding_practices_comments)
  comment_data[!is.na(data$code_freq) & (data$code_freq %in% c("Never")),] <- NA
  data[colnames(comment_data)] <- comment_data
  
  return(data)
}