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