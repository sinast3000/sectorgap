
# ------------------------------------------------------------------------------

get_recessions <- function(country) {
  
  if(country == "us") {
    
    recessions <- as.data.frame(matrix(
      c(1960+4/12, 1961+2/12,
        1969+12/12, 1970+11/12,
        1973+11/12, 1975+03/12,
        1980+01/12, 1980+07/12,
        1981+07/12, 1982+11/12,
        1990+07/12, 1991+03/12,
        2001+03/12, 2001+11/12,
        2007+12/12, 2009+06/12,
        2020+2/12,  2020+4/12),
      ncol = 2, byrow = T))
    
  } else if (country == "ch") {
    
    recessions <- as.data.frame(matrix(
      c(1982.25, 1983.25,
        1991.00, 1993.25,
        2002.25, 2003.50,
        2008.75, 2009.50,
        2020.00, 2021.00),
      ncol = 2, byrow = T))
    
  }
  
  colnames(recessions) = c("start", "end") 
  return(recessions)
  
}


# ---------------------------------------------------------------------------#' 

#' Computes the period on period percentage change
#'
#' @param x (multiple) time series object
#'
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
substr_r <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}

