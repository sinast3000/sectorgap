

# ---------------------------------------------------------------------------#' 

#' Computes the period on period percentage change
#'
#' @param x (multiple) time series object
#'
#' @keywords internal
pct <- function(x) {
  y <- 100 * (x / stats::lag(x, -1) - 1)
  colnames(y) <- colnames(x)
  return(y)
}

# ---------------------------------------------------------------------------

#' Creates a constant time series with same dates and frequency as the one 
#' given.
#' 
#' @param c constant scalar
#' @param tsm (multiple) time series object
#'
#' @keywords internal
ts_c <- function(c = 0, tsm) {
  ts(
    c, 
    start = start(tsm), 
    end = end(tsm), 
    frequency = frequency(tsm)
  )
}

# ---------------------------------------------------------------------------

#' HP filter
#'
#' @description Applies the Hodrick Prescott Filter.
#'
#' @param x A univariate time series object.
#' @param lambda The smoothing parameter.
#'
#' @return A univariate time series object containing the trend of the original time series.
#'
#' @importFrom stats start end window ts lag frequency time
#' @keywords internal
hpfilter <- function(x, lambda) {
  n <- length(x[is.na(x) == FALSE])
  A <- 6 * diag(n)
  A[row(A) == (col(A) - 1)] <- -4
  A[row(A) == (col(A) + 1)] <- -4
  A[row(A) == (col(A) - 2)] <- 1
  A[row(A) == (col(A) + 2)] <- 1
  A[1:2, 1:2] <- matrix(c(1, -2, -2, 5), 2, 2)
  A[(n - 1):n, (n - 1):n] <- matrix(c(5, -2, -2, 1), 2, 2)
  
  trend <- ts(NA, start = start(x), end = end(x), frequency = frequency(x))
  trend[is.na(x) == FALSE] <- (solve(diag(n) + lambda * A)) %*% x[is.na(x) == FALSE]
  
  trend
}

# ---------------------------------------------------------------------------

#' Extracts last letter in string
#'
#' @param x character string
#' @param n number of letters to extract
#'
#' @keywords internal
substr_r <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}

# ---------------------------------------------------------------------------

#' array multiplication
#'
#' @param a A multiple time series object.
#' @param a A matrix.
#' @keywords internal
matmult3d <- function(a,b) {
  n <- NROW(a)
  m <- NCOL(a)
  k <- dim(b)[3]
  if (k > 1) {
    y <- do.call(rbind, lapply(1:n, function(x) a[x, ] %*% t(b[, , x])))
    y <- ts(y, start = start(a), frequency = frequency(a))
  } else {
    y <- a %*% t(b[,,1])
  }
  y <- ts(y, start = start(a), frequency = frequency(a))
  return(y)
}
