
#'@title Plot frequency subplots
#'
#'@description Produce multiple bar chart (plotly) for single factor frequency data. 
#'Intended for use with frequencies represented as percentages.
#'Incomplete validation checks: some combinations of number of plots and number of rows/columns will fail:
#'Each column should contain at least two charts. 
#'
#'@param data Frequency table (data frame). Expects a tidy dataset with 3 columns: factor 1 (categorical axes), factor 2 (one plot per factor level) and values. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param height plot height in pixels
#'@param width plot width in pixels
#'@param nrows number of rows in plot (min 2, 3 by default)
#'@param y_margin margin between rows (chart proportion), 0.1 by default.
#'@param x_margin margin between columns (chart proportion). 0.1 by default.
#'@param bar_colour Colour name. Defaults to blue (see carsurvey2::get_gradient())
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#'
#'@return subplots
#'
#'@export

freq_subplots <- function(data, xlab, ylab, height, width, bar_colour, nrows = 3, y_margin = .1, x_margin = .1, font_size = 12, orientation = "h") {
  
  if (nrows == 1) {
    stop("Unexpected input: n_rows should be 2 or greater.")
  }
  
  n_plots <- length(unique(data[[2]]))
  ncols <- ceiling(n_plots / nrows)
  
  # The R plotly implementation handles margins poorly. Custom margins are created here by filling
  # spaces with blank plotly objects. This is done by the loops below, as well as the height and width settings.
  plot_positions <- sapply(1:n_plots, function(i) {
    row <- ceiling(i / ncols)
    col <- i - (row - 1) * ncols
    
    # Position = chart index + space occupied by previous blank rows + 1 space per previous spaces between charts 
    pos <- i + (row - 1) * ncols + 2 * (ncols - 1) * (row - 1) + col - 1
    
    return(pos)
  })
  
  n_cells <- plot_positions[length(plot_positions)]
  
  plots <- list()
  plot_index <- 1
  
  for (i in 1:n_cells) {
    
    if (i %in% plot_positions) {
      filter <- unique(data[[2]])[plot_index]
      
      plot_data <- data[data[[2]] == filter, c(1, 3)]
      
      plot <- plot_sub_freqs(plot_data, 
                             ylab = ylab, 
                             xlab = xlab, 
                             title = filter,
                             font_size = font_size, 
                             orientation = orientation, 
                             height = height,
                             width = width)
      
      plots[[i]] <- plotly::plotly_build(plot)
      
      plot_index <- plot_index + 1
    } else {
      plots[[i]] <- plotly::plotly_build(plotly::plotly_empty())
    }
  }   
  
  # Available non-margin space / the number of non-empty rows 
  plot_height <- (1 - y_margin * (nrows - 1)) / nrows
  heights <- c(plot_height, rep(c(y_margin, plot_height), nrows - 1))
  
  plot_width <- (1 - x_margin * (ncols - 1)) / ncols
  widths <- c(plot_width, rep(c(x_margin, plot_width), ncols - 1))
  
  # Annotation settings
  all_positions <- expand.grid(cumsum(widths), 1 - cumsum(heights) + plot_height / 2)
  annotation_locations <- all_positions[plot_positions, ]
  
  annotations <- lapply(1:n_plots, function(i) {
    return(
      list( 
        x = annotation_locations[i, 1] - plot_width / 2,  
        y = annotation_locations[i, 2] + plot_height / 2,  
        text = unique(data[[2]])[i],  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE, 
        font = list(size = font_size * 1.2)
      )
    )
  })
  
  return(subplot <- plotly::subplot(plots, heights = heights, widths = widths, nrows = nrows * 2 - 1, 
                                    titleX = TRUE, titleY = TRUE, margin = 0) %>%
           plotly::layout(autosize = TRUE, x = list(), showlegend = FALSE, annotations = annotations) %>% 
           plotly::config(displayModeBar = FALSE))
  
}


#'@title Plot frequency graph for subplot
#'
#'@description Produce bar chart (plotly) for single factor frequency data. 
#'
#'@param table Frequency table (data frame). 2 columns - cateogry names and frequencies. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param height plot height in pixels
#'@param width plot width in pixels
#'@param title optional. Subplot title
#'@param bar_colour Colour name. Defaults to blue (see carsurvey2::get_gradient())
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#'
#'@return bar chart
#'
#'@export

plot_sub_freqs <- function(table, xlab, ylab, height, width, title = "", bar_colour, font_size = 12, orientation = "v") {
  
  # Set default bar colour
  if (missing(bar_colour)) {
    bar_colour <- grDevices::rgb(0, 69, 86, max = 255)
  } else if (!is.character(bar_colour) | length(bar_colour) != 1) {
    stop("Unexpected input - bar_colour should be a single colour name.")
  }
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) != 2) {
    stop("Unexpected input - table does not contain two columns.")
  } else if (!is.numeric(table[[2]])) {
    stop("Unexpected input - table column 2 is not numeric.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
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
    title = list(text = xlab, standoff = 0),
    tickfont = list(size = font_size),
    titlefont = list(size = font_size)
  )
  
  y <- list(
    title = list(text = ylab, standoff = 0),
    tickfont = list(size = font_size),
    titlefont = list(size = font_size),
    range = c(0, 100)
  )
  
  if (orientation == "v") {
    table <- dplyr::arrange(table, table[,1])
    table[,1] <- factor(table[,1], levels = table[,1])
    x_vals <- table[[1]]
    y_vals <- table[[2]]
    x_axis <- x
    y_axis <- y
  } else if (orientation == "h") {
    table <- dplyr::arrange(table, dplyr::desc(table[,1]))
    table[,1] <- factor(table[,1], levels = table[,1])
    x_vals <- table[[2]]
    y_vals <- table[[1]]
    x_axis <- y
    y_axis <- x
  }
  
  ylab <- y_axis$title
  y_axis$standoff = 40
  
  hovertext <- paste0(table[[1]], ": ", round(abs(table[[2]]), 1), "%", " <extra></extra>")
  
  fig <- plotly::plot_ly(
    x = x_vals,
    y = y_vals,
    hovertemplate = hovertext,
    marker = list(color = bar_colour),
    type = "bar",
    orientation = orientation, 
    height = height,
    width = width
  )
  
  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,
                        xaxis = x_axis, 
                        yaxis = y_axis,
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size))) %>%
    
    return(fig)
}
