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
#' @param first_variable first variable to compare
#' @param second_variable second variable to compare
#' @param method optional: correlation method. Defaults to "spearman" 
#' 
#' @return list of correlation test outputs
#' 
#' @export

compute_correlation_test <- function(first_variable, second_variable, method = "spearman"){
  
  corr <- cor.test(as.numeric(first_variable), as.numeric(second_variable), method = method, exact = FALSE)
  return(corr)
  
}
