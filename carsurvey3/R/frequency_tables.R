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
#' @param data  full CARS wave 3 data.frame after preprocessing 
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