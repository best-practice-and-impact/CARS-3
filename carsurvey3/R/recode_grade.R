#'@title Recode grade
#'
#'@description Recode non-set responses to grade question
#'
#'@param data a data frame containing cleaned CARS wave 2 data
#'@param grade_col the name of the column containing the grade (set to "grade" by default)
#'@param dep_col the name of the column containing the department (set to "dept" by default)
#'
#'@return cARS wave 3 data with the recoded grade column
#'
#'@export


recode_grade <- function(data, grade_col = "grade", dep_col = "department") {
  
  if (class(data) != "data.frame") {
    stop("Unexpected input - data is not a data.frame")
  } else if (!grade_col %in% colnames(data)) {
    stop("Unexpected input - data does not contain the grade column")
  } else if (!dep_col %in% colnames(data)) {
    stop("Unexpected input - data does not contain the department column")
  }
  
  final_grades <- c(
    "Administrative Officer (or equivalent)",
    "Executive Officer (or equivalent)",
    "Higher Executive Officer (or equivalent)",                             
    "Senior Executive Officer (or equivalent)",
    "Grade 7 (or equivalent)",                 
    "Grade 6 (or equivalent)",
    "Fast Stream",
    "Other - NHS", 
    "Other - DSTL"
  )
  
  # Merge NHS into single category
  data[[grade_col]][grepl("nhs", data[[dep_col]], ignore.case=TRUE)] <- "Other - NHS"
  data[[grade_col]][grepl("nhs", data[[grade_col]], ignore.case=TRUE)] <- "Other - NHS"
  
  data[[grade_col]][!data[[grade_col]] %in% final_grades] <- "Other"
  
  return(data)
  
}

