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
    trace_order <- "normal"
    colours <- rev(colours)
  } else if (orientation == "h") {
    table <- dplyr::arrange(table, dplyr::desc(table[,1]))
    table[,1] <- factor(table[,1], levels = unique(table[,1]))
    x_vals <- table[[3]]
    y_vals <- table[[1]]
    x_axis <- y
    y_axis <- x
    trace_order <- "reversed"
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
                        legend = list(traceorder = trace_order, font = list(font_size = font_size)),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size))
  )
  
  fig <- plotly::layout(fig, annotations = carsurvey2::create_y_lab(ylab, font_size))
  
  return(fig)
  
}

#'@title Plot stacked bar graph
#'
#'@description Produce stacked bar chart (plotly). 
#'
#'@param table Frequency table for stacked bar chart (data frame). 3+ columns - sub-question names in column 1 with answer options in subsequent columns.. 
#'@param colour_scale type of colour scale ("gradient", "scale" or "2gradients"). See get_gradient(), get_2colour_scale() and get_2colour_gradients(). 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param neutral_mid whether the midpoint of the colour scale should be neutral ("2gradients" scale only). TRUE by default
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_stacked <- function(table, xlab, ylab, n, colour_scale = "2gradients", font_size = 12, neutral_mid = TRUE, ...) {
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) < 3) {
    stop("Unexpected input - table should have at least three columns")
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
  
  # Validate colour_scale
  if (length(colour_scale) > 1 | !colour_scale %in% c("gradient", "scale", "2gradients", "3scale")) {
    stop("Unexpected input - colour_scale should be set to 'gradient', 'scale', '2gradients' or '3scale'.")
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  y <- list(
    title = "",
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  #reorder table
  table <- dplyr::arrange(table, dplyr::desc(table[,1]))
  table[,1] <- factor(table[,1], levels = table[,1])
  
  # Reshape data
  suppressMessages(
    longdata <- reshape2::melt(table)
  )
  
  
  # Get bar colours
  ncolours <- ncol(table) - 1
  if (colour_scale == "gradient") {
    colours <- get_gradient(ncolours)
  } else if (colour_scale == "scale") {
    colours <- get_2colour_scale(ncolours)
  } else if (colour_scale == "2gradients") {
    mid <- ceiling(ncolours/2)
    colours <- carsurvey2::get_2colour_gradients(ncol(table)-1, mid = mid, neutral_mid = neutral_mid)
  } else if (colour_scale == "3scale") {
    colours <- get_3colour_scale(ncolours)
  }
  
  colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255))
  colours <- lapply(colours, function(x) rep(x, nrow(table)))
  colours <- unlist(colours)
  
  hovertext <- paste0(longdata[[2]], ": ", longdata[[3]], " <extra></extra>")
  
  fig <- plotly::plot_ly(y = longdata[[1]], 
                         x=longdata[[3]], 
                         type="bar", 
                         color = longdata[[2]], 
                         orientation = "h", 
                         hovertemplate = hovertext,
                         marker = list(color = colours),
                         ...)
  
  fig <- plotly::config(fig, displayModeBar = F)
  
  fig <- plotly::layout(fig,  
                        barmode = "stack", 
                        clickmode = "none",
                        legend = list(orientation = "v",   
                                      title=list(text='RAP Score'),
                                      traceorder = "normal",
                                      font = list(size = font_size)),
                        margin = list(b = 100),
                        xaxis = x, 
                        yaxis = y,
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)
                        )
  )
  
  fig <- plotly::layout(fig, annotations = carsurvey2::create_y_lab(ylab, font_size))
  
  return(fig)
  
}