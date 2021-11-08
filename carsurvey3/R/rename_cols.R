#' Rename columns
#'
#' @description add meaningful column names to dataset ingested from smartsurvey API. 
#'
#' @param data CARS wave 3 (2021) survey data (data.frame).
#'
#' @return data.frame
#' 
#' @export

rename_cols <- function(data) {
  
  if (class(data) != "data.frame") {
    stop("Unexpected input: data is not a data.frame.")
  } else if (length(data) != 120) {
    stop("unexpected value: expecting data frame with 120 ")
  }
  
  data <- data[8:length(data)]
  
  colnames(data) <- c(
    "tracking_link",
    "grade", 
    "department",
    "department_other",
    "prof_non_CS",
    "prof_DS_GSG_GORS",
    "prof_DS_other",
    "prof_DDAT",
    "prof_GAD",
    "prof_GES",
    "prof_finance",
    "prof_geography",
    "prof_GORS",
    "prof_GSR",
    "prof_GSG",
    "prof_none_CS",
    "prof_other_CS",
    "region",
    "highest_qualification", 
    "degree_maths",
    "degree_stats",
    "degree_systems",
    "degree_comp_sci",
    "degree_econ",
    "degree_psych",
    "degree_geography",
    "degree_other_social_science",
    "degree_life_sci",
    "degree_physical_sci",
    "degree_earth_sci",
    "degree_engineering",
    "degree_business",
    "degree_med_health",
    "degree_law",
    "degree_history",
    "degree_lang_lit",
    "degree_other",
    "code_freq", 
    "code_manage",
    "operations_analysis",
    "operations_data_cleaning",
    "operations_data_linking",
    "operations_data_transfer",
    "operations_data_vis",
    "operations_machine_learning",
    "operations_modelling",
    "operations_QA",
    "operations_other",
    "operations_other_comments",
    "knowledge_R",
    "access_R",
    "knowledge_SQL",
    "access_SQL",
    "knowledge_SAS",
    "access_SAS",
    "knowledge_VBA",
    "access_VBA",
    "knowledge_python",
    "access_python",
    "knowledge_SPSS",
    "access_SPSS",
    "knowledge_stata",
    "access_stata",
    "knowledge_JS",
    "access_JS",
    "knowledge_java_scala",
    "access_java_scala",
    "knowledge_C",
    "access_C",
    "knowledge_matlab",
    "access_matlab",
    "other_tools",
    "experience_outside_role",
    "ability_change",
    "learn_before_current_role",
    "where_learned_to_code",
    "heard_of_RAP",
    "RAP_champion",
    "RAP_knowledge",
    "RAP_confident",
    "RAP_supported",
    "RAP_resources",
    "RAP_understand_key_components",
    "RAP_important",
    "RAP_implementing",
    "RAP_planning_to_implement",
    "RAP_comments",
    "use_open_source",
    "open_source_code",
    "version_control", 
    "code_reviews",
    "functions",
    "unit_tests",
    "packages",
    "standard_dir_structure",
    "follow_code_guidelines",
    "automated_QA",
    "AQUA_book",
    "code_comments",
    "function_docs",
    "readme",
    "desk_notes",
    "registers",
    "AQA_logs",
    "flow_charts",
    "other_docs",
    "CI",
    "dependency_management",
    "reproducible_workflow",
    "coding_practices_comments",
    "support_comments",
    "future_survey_comments",
    "survey_comments"
  )
  
  return(data)
  
}