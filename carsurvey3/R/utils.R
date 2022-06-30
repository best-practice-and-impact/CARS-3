#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' second Pipe operator
#'
#' @name %<>%
#' @rdname second_pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
NULL

#' compute spearmans rank test
#' 
#' @description compute spearmans rank correlation on two variables 
#' 
#' @param data dataset
#' @param first_variable first variable to compare
#' @param second_variable second variable to compare
#' 
#' @return list of correlation test outputs

compute_correlation_test <- function(data, first_variable, second_variable){
  
  corr <- cor.test(as.numeric(data[[first_variable]]), as.numeric(data[[second_variable]]), method = "spearman", exact = FALSE)
  return(corr)
  
}
