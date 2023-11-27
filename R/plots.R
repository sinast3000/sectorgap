
#' Time series plots
#' 
#' @description Creates a set of time series plots of the results.
#'
#' @param x object of class \code{ss_fit}
#' @param plot_type type of plots, options are \code{"timeseries", "density"}
#' @param n_col number of columns for grid plots
#' @param n_sep increments of x axis ticks in years
#' @param file_path file path for plots
#' @param title boolean indicating if plots should contain titles
#' @param save boolean indicating if plots should be saved, if \code{FALSE}, the 
#'   plots will be printed instead, default is \code{save = FALSE} (ignored if 
#'   \code{file_path} is provided)
#' @param device character string with format used in \code{ggsave}
#' @param width plot width in \code{units}, for grid plots adjusted for the 
#'   number of plot columns \code{n_col}
#' @param height plot height in \code{units}, for grid plots adjusted for the 
#'   number of plot rows implied by \code{n_col}
#' @param units units for plot size (\code{"in", "cm", "mm", or "px"})
#' @param highlighted_area data frame with two columns called \code{start} and 
#'   \code{end} containing start and end date, e.g. \code{1990.25} and 
#'   \code{1992.75} for 1990 Q2 until 1992 Q4 
#'   (only used if \code{plot_type = "timeseries"})
#' @param plot_start start of x axis in years, e.g., \code{1990.5} (only used if 
#'   \code{plot_type = "timeseries"})
#' @param plot_end end of x axis in years, e.g., \code{2010.25} (only used if 
#'   \code{plot_type = "timeseries"})
#' @param alpha cut off value for posterior (only used if 
#'   \code{plot_type = "density"})
#' @param ... ignored
#' @inheritParams define_ssmodel
#' @inheritParams transform_results
#' 
#' @return nothing
#' 
#' @export
plot.ss_fit <- function(
  x,
  plot_type = "timeseries",
  estimate = "median",
  data = data,
  n_col = 3,
  n_sep = 5,
  file_path = NULL,
  title = TRUE,
  save = FALSE,
  device = "jpg",
  width = 10,
  height = 3,
  units = "in",
  highlighted_area = NULL,
  plot_start = NULL,
  plot_end = NULL,
  alpha = 0.05,
  ...
) {
  
  # check file path
  if (is.null(file_path) && save) {
    warnings("No file path provided, figures will not be saved.")
  } else if (!is.null(file_path)) {
    dir.create(file_path, showWarnings = FALSE)
    save <- TRUE
  }
  
  if (plot_type == "timeseries") {
    
    df <- transform_results(
      fit = x, 
      data = data,
      estimate = estimate
    )
    
    plot_time_series(
      df = df,
      settings = x$settings,
      n_col = n_col,
      n_sep = n_sep,
      highlighted_area = highlighted_area,
      plot_start = plot_start,
      plot_end = plot_end,
      file_path = file_path,
      title = title,
      save = save,
      device = device,
      width = width,
      height = height,
      units = units
    )
    
  } else if (plot_type == "density") {
    
    plot_densities(
      fit = x, 
      file_path = file_path,
      n_col = n_col,
      alpha = alpha,
      save = save,
      title = title,
      device = device,
      width = width,
      height = height,
      units = units
    )
    
    
  } else {
    stop("No valid 'plot_type' provided.")
  }
  
}
