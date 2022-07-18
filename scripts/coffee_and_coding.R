library(magrittr)

# Ingest data

raw_data <- carsurvey2::ingest(survey = "961613", export = "2028104", check_hash = "e95fc0c3dc2514a0ef1966902221cc13bdbd9224")

tidied_data <- carsurvey2::convert_raw(raw_data) %>% carsurvey2::tidy_ingest()

data <- tidied_data %>% carsurvey3::rename_cols() %>% carsurvey3::enforce_streaming() %>% carsurvey3::derive_vars()

data$prof_DS <- ifelse(data$prof_DS_GSG_GORS == "Yes" | data$prof_DS_other == "Yes", "Yes", "No")

# Coding tools by profession

profs <- grep("prof_", colnames(data), value = TRUE)[c(4:6, 8:11, 14)]
langs <- grep("knowledge_", colnames(data), value = TRUE)
lang_names <- c("R", "SQL", "SAS", "VBA", "Python", "SPSS", "Stata", "JavaScript", "Java/Scala", "C++/C#", "Matlab")
  
prof_counts <- sapply(profs, function(prof) {
  sum(data[prof] == "Yes")
})

prof_langs <- sapply(profs, function(prof) {
  filtered_data <- data[data[prof] == "Yes", ]

  freqs <- as.vector(colSums(filtered_data[langs] == "Yes"))
  return(freqs)
}) %>% data.frame

prof_langs <- data.frame(mapply(function(x, y) round(x / y * 100), prof_langs, prof_counts))

prof_langs <- cbind(data.frame(lang = lang_names), prof_langs)
colnames(prof_langs) <- c("Language", "DDAT", "GAD", "GES", "Geography", "GORS", "GSR", "GSG", "Data scientists")

top_langs <- sapply(prof_langs[2:9], function(x) {
  lang_names[order(x, decreasing = TRUE)][1:3]
}) %>% data.frame(check.names = FALSE)

top_langs <- cbind(data.frame(Rank = 1:3), top_langs)

kableExtra::kable(top_langs, format = "html") %>% kableExtra::kable_styling() 


# Coding capability by frequency

freq_order <- c(
  "Never",
  "Rarely",
  "Sometimes",
  "Regularly",
  "All the time"
)

change_order <- c(
  "Significantly worse",
  "Slightly worse",
  "No change",
  "Slightly better",
  "Significantly better"
)

data$code_freq <- factor(data$code_freq, levels = freq_order)
data$ability_change <- factor(data$ability_change, levels = change_order)

capability_change <- data.frame(table(data$code_freq, data$ability_change)) %>% tidyr::pivot_wider(names_from = Var2, values_from = Freq) 
capability_change <- data.frame(capability_change, check.names = FALSE)
capability_change[2:6] <- t(apply(capability_change[2:6], 1, function(x) x / sum(x)))
carsurvey2::plot_likert(capability_change, 3, "Capability change", "", 912, font_size = 14, width = 800, height = 800)

# Correlation

corr <- cor.test(as.numeric(data$code_freq), as.numeric(data$ability_change), method = "spearman", exact = FALSE)
corr$estimate
corr$p.value


# Where learned to code by profession 

where_learned_freqs <- table(data$where_learned_to_code)
where_learned_top <- where_learned_freqs[where_learned_freqs > 4]
where_learned_levels <- names(where_learned_top)

data$where_learned_cleaned <- data$where_learned_to_code
data$where_learned_cleaned <- factor(data$where_learned_cleaned, levels = c("In current role", where_learned_levels, "Other"))
data$where_learned_cleaned[is.na(data$where_learned_cleaned)] <- "Other"
data$where_learned_cleaned[data$where_learned_cleaned == "Other" & data$code_freq != "Never"] <- "In current role"

prof_where_learned <- data.frame(sapply(profs, function(x) table(data[data[x] == "Yes", ]$where_learned_cleaned)))
prof_where_learned <- data.frame(apply(prof_where_learned, 2, function(x) round(x / sum(x) * 100)))
prof_where_learned <- apply(prof_where_learned, 2, function(x) round(x / sum(x) * 100))
rbind(prof_where_learned, "All public sector employment" = prof_where_learned["In current role", ]+  prof_where_learned["In public sector employment", ])

# ONS comparisons


#'@title Plot factorial frequency graph
#'
#'@description Produce bar chart (plotly) for frequency data with grouping variable. 
#'
#'@param table Frequency table (data frame). 3 columns - cateogry names, groups and frequencies. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default.
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_factorial <- function(table, xlab, ylab, n, font_size = 12, orientation = "v", ...) {
  
  # Set default bar colours
  n_groups <- length(unique(table[[2]]))
  c <- carsurvey2::get_2colour_scale(n_groups)
  colours <- unlist(lapply(c, function(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))) 
  colours <- unlist(colours)
  colours <- rep(colours, c(unlist(table(table[[2]]))))
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) != 3) {
    stop("Unexpected input - table does not contain 3 columns.")
  } else if (!is.numeric(table[[3]])) {
    stop("Unexpected input - table column 3 is not numeric.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate n
  if ((!is.numeric(n) & !is.character(n)) | length(n) > 1) {
    stop("Unexpected input - n is not a single number or string")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }
  
  # Validate orientation
  if (!(orientation %in% c("h", "v"))) {
    stop("Unexpected input - orientation should be set to 'h' or 'v'")
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  y <- list(
    title = ylab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  if (orientation == "v") {
    table <- dplyr::arrange(table, table[,1])
    table[,1] <- factor(table[,1], levels = unique(table[,1]))
    x_vals <- table[[1]]
    y_vals <- table[[3]]
    x_axis <- x
    y_axis <- y
  } else if (orientation == "h") {
    table <- dplyr::arrange(table, dplyr::desc(table[,1]))
    table[,1] <- factor(table[,1], levels = unique(table[,1]))
    x_vals <- table[[3]]
    y_vals <- table[[1]]
    x_axis <- y
    y_axis <- x
  }
  
  ylab <- y_axis$title
  y_axis$title <- ""
  
  fig <- plotly::plot_ly(
    x = x_vals,
    y = y_vals,
    color = table[[2]],
    marker = list(color = colours),
    type = "bar",
    ...
  )
  
  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,  
                        xaxis = x_axis, 
                        yaxis = y_axis, 
                        margin = list(b = 100),
                        legend = list(traceorder = "reversed", font = list(font_size = font_size)),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size))
  )
  
  fig <- plotly::layout(fig, annotations = carsurvey2::create_y_lab(ylab, font_size))
  
  return(fig)
  
}


ons_data <- data[data$department == "Office for National Statistics", ]
other_deps_data <- data[data$department != "Office for National Statistics", ]

ons_tables <- carsurvey3::summarise_all(ons_data)
other_deps_tables <- carsurvey3::summarise_all(other_deps_data)

## Coding frequency

wilcox.test(as.numeric(ons_data$code_freq), as.numeric(other_deps_data$code_freq))

ons_tables$code_freq[2] <- ons_tables$code_freq[2] / nrow(ons_data) * 100
other_deps_tables$code_freq[2] <- other_deps_tables$code_freq[2] / nrow(other_deps_data) * 100


freq_combined <- rbind(data.frame(other_deps_tables$code_freq, department = "Other departments", check.names = FALSE), 
                  data.frame(ons_tables$code_freq, department = "ONS", check.names = FALSE))
freq_combined <- freq_combined[c(1,3,2)]
plot_factorial(freq_combined, "Coding frequency", "Percent", n = nrow(data), font_size = 14, width = 700)


## Coding tools

langs <- grep("knowledge_", colnames(data), value = TRUE)
lang_names <- c("R", "SQL", "SAS", "VBA", "Python", "SPSS", "Stata", "JavaScript", "Java/Scala", "C++/C#", "Matlab")

ons_tables$knowledge[2] <- ons_tables$knowledge[2] / nrow(ons_data) * 100
other_deps_tables$knowledge[2] <- other_deps_tables$knowledge[2] / nrow(other_deps_data) * 100

langs_combined <- rbind(data.frame(ons_tables$knowledge[1:2], department = "ONS", check.names = FALSE), 
                        data.frame(other_deps_tables$knowledge[1:2], department = "Other departments",  check.names = FALSE))

langs_combined <- langs_combined[c(1,3,2)]

plot_factorial(langs_combined, "Programming language", "Percent", n = nrow(data), font_size = 14, height = 600, width = 600, orientation = "h")

# Ability change

wilcox.test(as.numeric(ons_data$ability_change), as.numeric(other_deps_data$ability_change))

ons_tables$ability_change[2] <- ons_tables$ability_change[2] / sum(ons_tables$ability_change[2]) * 100
other_deps_tables$ability_change[2] <- other_deps_tables$ability_change[2] / sum(ons_tables$ability_change[2]) * 100

freq_combined <- rbind(data.frame(other_deps_tables$ability_change, department = "Other departments", check.names = FALSE), 
                       data.frame(ons_tables$ability_change, department = "ONS", check.names = FALSE))