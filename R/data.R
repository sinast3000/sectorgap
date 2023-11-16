
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
