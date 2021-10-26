#' Derive code status
#'
#' @description Dervice the staus of each programmming language as "access" (access only), "knowledge" (knowledge only), "both" or "neither".  
#'
#' @param data tidied CARS wave 3 data (data.frame).
#'
#' @return data.frame
#' @export

derive_language_status <- function(data) {
  lang_list <- colnames(data)[grepl("access_", colnames(data))]
  
  lang_list <- gsub("access_", "", lang_list)
  
  new_vars <- sapply(lang_list, function(lang) {
    access_col <- data[paste0("access_", lang)]
    
    knowledge_col <- data[paste0("knowledge_", lang)]
    
    dplyr::case_when(access_col == "Yes" & knowledge_col == "Yes" ~ "both",
                     access_col == "Yes" & knowledge_col != "Yes" ~ "access",
                     access_col != "Yes" & knowledge_col == "Yes" ~ "knowledge",
                     access_col != "Yes" & knowledge_col!= "Yes" ~ "neither")
  })
  
  colnames(new_vars) <- paste0("status_", lang_list)
  
  return(data.frame(data, new_vars))
}