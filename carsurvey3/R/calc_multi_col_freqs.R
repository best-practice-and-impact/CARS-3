#'@title Calculate multiple column frequency table
#'
#'@description Take multiple columns and produce a frequency or proportion of each column
#'
#'@param data the selected columns from the CARS-3 wave dataset
#'@param levels a list of factor levels for the cell values of all columns
#'@param labels a list of labels for each of the variables
#'@param calc_props whether or not to calculate proportions (logical, FALSE by default)
#'
#'@return frequencies data frame with the frequencies of response options by column
#'
#'@export

calc_multi_col_freqs <- function(data, levels, labels, calc_props = FALSE){
  # Validate inputs
  if (class(data) != "data.frame") {
    stop("Unexpected input - cols is not a data.frame")
  } else if (typeof(levels) != "character") {
    stop("Unexpected input - levels is not a character vector")
  } else if (typeof(labels) != "character") {
    stop("Unexpected input - labels is not a character vector")
  } else if (typeof(calc_props) != "logical" | length(calc_props) > 1) {
    stop("Unexpected input - calc_percentages is not a single logical value")
  }
  
  frequencies <- apply(data, 2, function(x) {
    x <- factor(x, levels = levels)
    
    table(x)
  })
  
  if (calc_props) {
    frequencies <- apply(frequencies, 2, function(x) x/sum(x))}
  
  frequencies <- data.frame(labels = labels, t(frequencies))
  
  colnames(frequencies) <- gsub("[.]", " ", colnames(frequencies))
  
  rownames(frequencies) <- NULL
  
  return(frequencies)
  
}
