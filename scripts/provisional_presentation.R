library(magrittr)

# Ingest data

raw_data <- carsurvey2::ingest(survey = "961613", export = "2028104", check_hash = "e95fc0c3dc2514a0ef1966902221cc13bdbd9224")

tidied_data <- carsurvey2::convert_raw(raw_data) %>% carsurvey2::tidy_ingest()

data <- tidied_data %>% carsurvey3::rename_cols() %>% carsurvey3::enforce_streaming() %>% carsurvey3::derive_vars()


# Tracking links

tracking_link_freqs <- data.frame(table(data$tracking_link))
tracking_link_freqs <- tracking_link_freqs[order(tracking_link_freqs[[2]], decreasing = TRUE), ]
tracking_link_freqs$Var1 <- factor(tracking_link_freqs$Var1, levels = tracking_link_freqs$Var1)

carsurvey2::plot_freqs(tracking_link_freqs, "Link", "Count", n = sum(tracking_link_freqs[[2]]), orientation = "h", font_size = 14)

# Top departments

dep_freqs <- data.frame(table(data$department))
dep_freqs <- dep_freqs[order(dep_freqs[[2]], decreasing = TRUE), ]
top_deps <- head(dep_freqs, n = 10)
top_deps$Var1 <- factor(top_deps$Var1, levels = top_deps$Var1)

carsurvey2::plot_freqs(top_deps, "Department", "Count", n = nrow(data), orientation = "h", font_size = 14)

n_deps <- nrow(top_deps)

# Managers

manage_coders <- data.frame(table(data$code_manage))
manage_coders$Var1 <- factor(manage_coders$Var1, levels = c("Yes", "No", "I don't line manage anyone"))
carsurvey2::plot_freqs(manage_coders, "Line manage anyone who writes code", "Count", n = nrow(data), orientation = "h", font_size = 14)
