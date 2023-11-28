# ------------------------------------------------------------------------------

#' Settings object validity check
#' 
#' @description Checks if settings are a valid object of class \code{settings}.
#' 
#' @param x settings object
#' @param dfl list of data frames, returned by function \code{settings_to_df}
#' @param return.logical If \code{return.logical = FALSE} (default), an error 
#'   message is printed if the object is not of class \code{settings}, if 
#'   \code{return.logical = TRUE}, a logical value is returned
#'   
#' @importFrom dplyr %>%
#'  
#' @return A logical value or nothing, depending on the value of \code{return.logical}.
#'
#' @export
is.settings <- function(x, dfl = NULL, return.logical = FALSE) {
  
  opts <- list(
    lnames = c("agg", "group1", "subgroup1", "group2", "agggroup", "misc", 
               "fun_transform", "fun_transform_inv"),
    trend = 0:4,
    cycle = 0:2,
    corr = c(NA, 0, 2, 4)
  )
  
  # df settings if not provided
  if (is.null(dfl)) {
    dfl <- settings_to_df(x = x)
  }
  
  if (return.logical) {
    y <- inherits(x, "settings") &&
      any(names(x) %in% opts$lnames) &&
      any(dfl$obs$trend %in% opts$trend) &&
      any(dfl$obs$cycle %in% opts$cycle) &&
      any(dfl$obs$corr %in% opts$corr) &&
      !any(dfl$obs$variable %>% duplicated) &&
      !any(dfl$obs$variable_label %>% duplicated) &&
      !any(grepl("_", dfl$obs$variable)) &&
      !any(dfl$obs %>% select(group, group_label) %>% unique %>% .$group_label %>% duplicated)
    y
  } else {
  
    # check settings object
    idx <- !(names(x) %in% opts$lnames)
    if (any(idx)) {
      name_invalid <- names(x)[idx]
      stop(
        paste0(
          "Invalid list item for object 'settings': '", 
          paste0(name_invalid, collapse = ", "), 
          "', please respecify. \n Valid list items are ",
          paste0(opts$lnames, collapse = ", "),), 
        call. = FALSE
      )
    }
    
    # check trends 
    opts_names <- c("trend", "cycle", "corr")
    for (io in opts_names) {
      idx <- !(dfl$obs[[io]] %in% opts[[io]])
      if (any(idx)) {
        name_invalid <- dfl$obs$variable[idx]
        stop(
          paste0(
            "Invalid ", io ," for variable '", 
            paste0(name_invalid, collapse = ", "), 
            "', please respecify. Valid input for ", io ,"  are ",
            paste0(opts[[io]], collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
    
    # check variable names and labels
    cols_check <- c("variable", "variable_label")
    for (ic in cols_check) {
      # check duplicates
      idx <- dfl$obs[[ic]] %>% duplicated
      if (any(idx)) {
        name_duplicated <- dfl$obs[[ic]][idx]
        stop(
          paste0(
            "Duplicated ", gsub("_", " ", ic), " '", 
            paste0(name_duplicated, collapse = ", "), 
            "', please respecify"), 
          call. = FALSE
        )
      }
    }
    cols_check <- c("variable")
    for (ic in cols_check) {
      # check special character
      idx <- grepl("_", dfl$obs[[ic]])
      if (any(idx)) {
        name_duplicated <- dfl$obs[[ic]][idx]
        stop(
          paste0(
            "Invalid ", gsub("_", " ", ic), " '", 
            paste0(name_duplicated, collapse = ", "), 
            "', please do not use special characters"), 
          call. = FALSE
        )
      }
    }
    
    # check group labels
    labels <- dfl$obs %>% select(group, group_label) %>% unique %>% .$group_label
    idx <- labels %>% duplicated
    if (any(idx)) {
      name_duplicated <- labels[idx]
      stop(
        paste0(
          "Duplicated group label",  
          paste0(name_duplicated, collapse = ", "), 
          "', please respecify"), 
        call. = FALSE
      )
    }
  }
  
} 