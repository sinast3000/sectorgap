
# ------------------------------------------------------------------------------

#' Input data
#' 
#' @description Prepares the required input data, it performs the 
#' transformations to the raw data and computes the necessary weights for the 
#' constraints.
#'
#' @param tsl_n time series list with nominal level series for aggregate output 
#' \code{agg} and its subcomponents in \code{group1, group2}
#' @param tsl_p time series list with price series for aggregate output 
#' \code{agg} and its subcomponents in \code{group1, group2}
#' @param tsl time series list with all untransformed endogenous series
#' @param ts_start start date, e.g. \code{c(2000, 2)} or \code{2000.25}
#' @param ts_end end date, e.g. \code{c(2000, 2)} or \code{2000.25}
#' @param extend_weights logical indicating if missing weights at beginning/end 
#'   of sample should be filled with the last/first available value
#' @inheritParams define_ssmodel
#' 
#' @details Either \code{tsl_n} or \code{tsl_p} must be supplied.
#' 
#' @details Weights are forward/backward extended with the first/last value if 
#'   the supplied time series do not cover the entire period.
#' 
#' @return A list with five components:
#' \item{tsm}{multiple time series object with all (transformed) endogneous 
#'   variables}
#' \item{real}{multiple time series object with real series of 
#'   \code{agg, group1, group2}}
#' \item{nominal}{multiple time series object with nominal series of 
#'   \code{agg, group1, group2}}
#' \item{prices}{multiple time series object with price series of 
#'   \code{agg, group1, group2}}
#' \item{weights}{list of multiple time series objects with weights for the 
#'   constraints, i.e., for series \code{agg, group1, group2} if applicable}
#'   
#' @importFrom stats window start end
#' @importFrom dplyr %>%
#' 
#' @export
#' @examples
#' data("data_ch")
#' settings <- initialize_settings()
#' data <- prepate_data(
#'   settings = settings,
#'   tsl = data_ch$tsl,
#'   tsl_n = data_ch$tsl_n
#' )
prepate_data <- function(
  settings,
  tsl,
  tsl_n = NULL, 
  tsl_p = NULL,
  ts_start = NULL,
  ts_end = NULL,
  extend_weights = FALSE
) {

  . <- NULL
  
  if (is.null(tsl_p) & is.null(tsl_n)) stop("Either 'tsl_p' or 'tsl_n' must be supplied.")
  
  # initialize
  if (is.null(tsl_p)) {
    tsl_p <- list() 
    prices <- FALSE
    tsl_r <- tsl[names(tsl_n)]
  } else {
    tsl_n <- list()
    prices <- TRUE
    tsl_r <- tsl[names(tsl_p)]
  }
  tsl_w <- list()
  
  # settings to data frames
  df_set <- settings_to_df(x = settings)
  endo <- df_set$obs$variable
  
  # aggregate
  idx <- settings$agg$variable
  if (!prices) {
    tsl_p[[idx]] <- tsl_n[[idx]] / tsl_r[[idx]] * 100
  } else {
    tsl_n[[idx]] <- tsl_p[[idx]] / tsl_n[[idx]] * 100
  }
  
  # group1, group2
  for (ig in c("group1", "group2")) {
    if (!is.null(settings[[ig]]$variable)) {
      idx <- settings[[ig]]$variable
      if (!prices) {
        tsl_p[idx] <- lapply(idx, function(x) {
          tsl_n[[x]] / tsl_r[[x]] * 100
        })
      } else {
        tsl_n[idx] <- lapply(idx, function(x) {
          tsl_p[[x]] * tsl_r[[x]] / 100
        })
      }
      tsl_w[[ig]] <- compute_gr_weights(
        tsl_r = c(tsl_r[c(settings$agg$variable, settings[[ig]]$variable)]),
        tsl_n = c(tsl_n[c(settings$agg$variable, settings[[ig]]$variable)]),
        idx = 1,
        pos = settings[[ig]]$variable[!(settings[[ig]]$variable %in% settings[[ig]]$variable_neg)],
        neg = settings[[ig]]$variable_neg
      ) %>%
        do.call(cbind, .)
    }
  }
  
  # subgroup1
  if (!is.null(settings$subgroup1)) {
    idx <- settings$subgroup1$variable
    tsl_w[["subgroup1"]] <- lapply(idx, function(x) {
      stats::lag(tsl[[x]] / tsl[[settings$subgroup1$load_name]], -1)
    }) %>%
      do.call(cbind, .)
    colnames(tsl_w[["subgroup1"]]) <- idx
  }
  
  # construct multiple time series object with transformed series
  tsm <- lapply(1:NROW(df_set$obs), function(x) {
    trans <- df_set$obs$transform[x]
    vx <- df_set$obs$variable[x]
    if (trans) {
      settings$fun_transform(tsl[[vx]])
    } else {
      tsl[[vx]]
    }
  }) %>%
    do.call(cbind, .)
  if (NROW(df_set$obs) > 1)  colnames(tsm) <- df_set$obs$variable
  
  # add residual for conatraints if necessary
  for (ig in c("group1", "group2")) {
    if (!is.null(settings[[ig]]$name_residual)) {
      ts_residual <- diff(tsm[,settings$agg$variable]) - Reduce("+", as.list(diff(tsm[, settings[[ig]]$variable]) * tsl_w[[ig]]))
      ts_residual <- hpfilter(ts_residual %>% na.trim, lambda = 1600)
      tsl_w[[ig]] <- cbind(tsl_w[[ig]], ts_residual)
      colnames( tsl_w[[ig]]) <- c(settings[[ig]]$variable, settings[[ig]]$name_residual)
    }
  }
  for (ig in c("subgroup1")) {
    if (!is.null(settings[[ig]]$name_residual)) {
      ts_residual <- diff(tsm[,settings[[ig]]$load_name]) - Reduce("+", as.list(diff(tsm[, settings[[ig]]$variable]) * tsl_w[[ig]]))
      ts_residual <- hpfilter(ts_residual %>% na.trim, lambda = 1600)
      tsl_w[[ig]] <- cbind(tsl_w[[ig]], ts_residual)
      colnames( tsl_w[[ig]]) <- c(settings[[ig]]$variable, settings[[ig]]$name_residual)
    }
  }
  
  # cut data window
  if (is.null(ts_start)) ts_start <- start(tsm)
  if (is.null(ts_end)) ts_end <- end(tsm)
  tsl_r <- lapply(tsl_r, window, start = ts_start, end = ts_end, extend = TRUE)
  tsl_p <- lapply(tsl_p, window, start = ts_start, end = ts_end, extend = TRUE)
  tsl_n <- lapply(tsl_n, window, start = ts_start, end = ts_end, extend = TRUE)
  tsl_w <- lapply(tsl_w, window, start = ts_start, end = ts_end, extend = TRUE)
  
  # extend weights if necessary
  if (extend_weights) {
    ts_freq <- frequency(tsl[[settings$agg$variable]])
    if (length(ts_start) > 1) ts_start <- ts_start  %*% c(1, 1 / ts_freq) - 1 / ts_freq
    if (length(ts_end) > 1) ts_end <- ts_end  %*% c(1, 1 / ts_freq) - 1 / ts_freq
    tsm <- window(tsm, start = ts_start, end = ts_end, extend = TRUE)
    tsl_w <- lapply(tsl_w, function(x) {
      x_trim <- na.trim(x)
      x_start <- first(time(x_trim))
      x_end <- last(time(x_trim))
      if (x_end <  ts_end) {
        window(x, start = x_end + 1 / ts_freq) <- window(x_trim, start = x_end) %>%
          matrix(., ncol = (ts_end - x_end) * ts_freq, nrow = ncol(x)) %>%
          t
      }
      if (x_start >  ts_start) {
        window(x, end = x_start - 1 / ts_freq) <- window(x_trim, end = x_start) %>%
          matrix(., ncol = (x_start - ts_start) * ts_freq, nrow = ncol(x)) %>%
          t
      }
      x
    })
  }
  
  resl <- list(
    tsm = tsm,
    real = do.call(cbind, tsl_r),
    prices = do.call(cbind, tsl_p),
    nominal = do.call(cbind, tsl_n),
    weights = tsl_w
  )
  class(resl) <- "data"
  
  return(resl)
}

# ------------------------------------------------------------------------------

#' Computes nominal weights from sub sector data
#'
#' @param tsl_r time series list with real level series
#' @param tsl_n time series list with nominal level series
#' @param idx index for aggregate
#' @param pos sectors to be added
#' @param neg sectors to be substracted 
#' 
#' @return A time series list with weights.
#' 
#' @importFrom stats window<-
#' @keywords internal
compute_gr_weights <- function(tsl_r, tsl_n, idx, pos, neg) {
  
  # compute prices  
  tsl_p <- lapply(names(tsl_r), function(x) {
    tsl_n[[x]] / tsl_r[[x]] * 100
  })
  names(tsl_p) <- names(tsl_r)
  # # prolong price series if necessary
  tsl_p <- lapply(names(tsl_p), function(x) {
    end_date_r <- last(time(zoo::na.trim(tsl_r[[x]])))
    end_date_p <- last(time(zoo::na.trim(tsl_p[[x]])))
    y <- tsl_p[[x]]
    if (end_date_r > end_date_p) {
      window(y, start = end_date_p, end = end_date_r, extend = TRUE) <- as.numeric(last(zoo::na.trim(y)))
    }
    y
  })

  # previous year prices
  tsl_p <- lapply(tsl_p, function(x) {
    y <- stats::lag(ta(x, to = "annual"), -1)
    td(y ~ -1, method = "uniform")$values
  })
  names(tsl_p) <- names(tsl_r)
  # previous period real level series
  tsl_r <- lapply(tsl_r, stats::lag, -1)
  
  # compute weights for prices and real level series
  w_p <- do.call(cbind, tsl_p[-idx]) / tsl_p[[idx]]
  w_r <- do.call(cbind, tsl_r[-idx]) / tsl_r[[idx]]
  tsl_w <- as.list(w_p * w_r)
  names(tsl_w) <- names(tsl_r[-idx])
  
  # change sign of negative contributions
  tsl_w[neg] <- lapply(tsl_w[neg], function(x) -x)

  return(tsl_w)
}

