
#' Results for sampled parameters and states
#' 
#' @description Computes estimation results for the MCMC sampling output for a
#' specific HPDI and evaluation function (e.g. mean or median).
#'
#' @inheritParams estimate_ssmodel
#' @inheritParams define_ssmodel
#' @param HPDIprob probability of highest posterior density interval, optional 
#'  if \code{fit} is supplied 
#' @param mcmc list with draws of parameters and states (including burnin phase)
#' @param fit (optional) an object of class \code{fit} (returned by the function 
#'   \code{estimate_ssmodel} and this function).
#' @param ... additional arguments (in case \code{fit} is supplied)
#' 
#' @details If \code{fit} is supplied, the arguments 
#'   \code{model, settings, mcmc} will be taken from this object.
#'  
#' @return An object of class \code{ss_fit}.  
#'  
#' @export
#'
compute_mcmc_results <- function(
  model,
  settings, 
  mcmc,
  HPDIprob = NULL, 
  fit = NULL,
  ...
){
  
  # to avoid RMD check note
  burnin <- R <- thin <- . <- prior <- NULL
  
  # unload object fit if supplied
  if (!is.null(fit))  {
    HPDIprob_new <- NULL
    if (!is.null(HPDIprob)) HPDIprob_new <- HPDIprob
    list2env(x = fit, envir = environment())
    if (is.null(HPDIprob)) HPDIprob <- attr(fit, "HPDIprob")
    if (!is.null(HPDIprob_new)) HPDIprob <- HPDIprob_new
  }
  if (is.null(HPDIprob)) {
    stop("HPDIprob not supplied")
  }
  
  # settings to data frames (for ssm parameter assignment)
  df_set <- settings_to_df(x = settings)
  endo <- df_set$obs$variable
  n <- length(endo)
  
  # exclude burn in phase
  list2env(x = mcmc, envir = environment())
  R_burnin <- floor(burnin * R)
  state <- state[, , (R_burnin / thin + 1):(R / thin)]
  state_gap <- state_gap[, , (R_burnin / thin + 1):(R / thin), drop = FALSE]
  parameters <- parameters[(R_burnin / thin + 1):(R / thin), ]
  colnames(state) <-  colnames(mcmc$state)
  colnames(state_gap) <-  colnames(mcmc$state_gap)

  # ----- parameters
  
  # parameters estimates
  df_param <- mcmc_summary(x = parameters, HPDIprob = HPDIprob)
  df_param <- df_param[order(rownames(df_param)), ]
  
  # update final model
  ssmodel_final_mean <- update_ssmodel(
    pars = apply(parameters, 2, mean), 
    model = model,
    settings = settings,
    df_set = df_set
  )  
  ssmodel_final_median <- update_ssmodel(
    pars = apply(parameters, 2, median), 
    model = model,
    settings = settings,
    df_set = df_set
  )
  
  # ----- states

  # drop lagged series
  idx_no_lag <- !grepl(paste0("_L", 1:max(df_set$lags$max_lag), collapse = "|"),  colnames(state))
  state <- state[, idx_no_lag, ]
  
  # combine state and state gaps
  state <-  lapply(1:dim(state)[3], function(x) cbind(state[,, x], state_gap[,, x])) %>% 
    do.call(cbind, .) %>% 
    array(., dim = c(dim(state)[1], dim(state)[2] + dim(state_gap)[2], dim(state)[3]), 
          dimnames = list(NULL, c(colnames(state), colnames(state_gap)), NULL))
  
  # sample statistics for  model series
  tsl <- results_state(
    model = model, 
    HPDIprob = HPDIprob, 
    state = state
  )

  # sample statistics for transformed model series
  # initialize
  state_trans <- state
  tsl$state_trans_summary <- tsl$state_summary
  tsl$state_trans_mean <- tsl$state_mean
  tsl$state_trans_median <- tsl$state_median
  
  # transform states back (inverse input transformations, only trends)
  idx_trend_trans <- endo[paste0("trend_", endo) %in% colnames(state_trans) &
                            df_set$obs$transform] %>%
    paste0("trend_", .)
  state_trans[, idx_trend_trans, ] <-  settings$fun_transform_inv(state_trans[, idx_trend_trans, ])

  # sample statistics for transformed trends
  idx_trend <- colnames(state_trans)[colnames(state_trans) %in% idx_trend_trans]
  tsl_trans <- results_state(
    model = model, 
    HPDIprob = HPDIprob, 
    state = state_trans[, idx_trend, , drop = FALSE]
  )
  idx_trend_summary <- colnames(tsl$state_summary)[gsub("\\..*", "", colnames(tsl$state_summary)) %in% idx_trend_trans]
  tsl$state_trans_summary[, idx_trend_summary] <- tsl_trans$state_summary
  tsl$state_trans_mean[, idx_trend] <- tsl_trans$state_mean
  tsl$state_trans_median[, idx_trend] <- tsl_trans$state_median

  # return object of class fit
  fit <- list(
    tsl = tsl,
    ssm_fit_mean = ssmodel_final_median,
    ssm_fit_median = ssmodel_final_median,
    parameters = df_param,    
    model = model,
    settings = settings,
    mcmc = mcmc,
    prior = prior
  )
  class(fit) <- c("ss_fit")
  attr(fit, "R") <- R
  attr(fit, "burnin") <- burnin
  attr(fit, "thin") <- thin
  attr(fit, "HPDIprob") <- HPDIprob

  invisible(fit)
}


# ------------------------------------------------------------------------------

#' Highest posterior density interval (HPDI)
#' 
#' @description Computes the approximate highest posterior density interval 
#' (HPDI).
#'
#' @param x A \code{R x n} matrix with \code{R} draws of \code{n} variables
#' @param prob The probability mass of the interval, a scalar between zero and one
#' 
#' @return \code{n x 2} matrix with lower and upper boundary of the HPDI.
#' 
#' @keywords internal
hpd_interval <- function(x, prob = 0.95) {
  x <- as.matrix(x)
  # order values
  xOrder <- apply(x, 2, sort)
  if (!is.matrix(xOrder)) stop("x must have nsamp > 1")
  # number of samples and parameters
  R <- nrow(xOrder)
  n <- ncol(xOrder)
  # number or sampled values included in the HPD interval
  Rprob <- max(1, min(R - 1, round(R * prob)))
  # compute ranges of possible intervals
  inds <- apply(
    xOrder[(Rprob + 1):R, , drop = FALSE] - xOrder[1:(R - Rprob), , drop = FALSE],
    2, which.min
  )
  # choose intervals with shortest range
  result <- cbind(
    xOrder[cbind(inds, 1:n)],
    xOrder[cbind(inds + Rprob, 1:n)]
  )
  # rename
  dimnames(result) <- list(colnames(x), c("lower", "upper"))
  attr(result, "Probability") <- Rprob / R
  # return
  return(result)
}


# ------------------------------------------------------------------------------

#' Geweke test for convergence
#' 
#' @description Conducts a Geweke test for convergence of the draws.
#'
#' @param x \code{R x n} matrix with \code{R} draws of \code{n} variables
#' @param frac1 probability mass of the first interval, a scalar between zero and one
#' @param frac2 probability mass of the second interval, a scalar between zero and one
#' @param alpha significance level used to compute the test decision, a scalar between
#'   zero and one
#'
#' @details Under the H0 of convergence, the test statistic is standard normally distributed.
#' @details Naturally, \code{frac1 + frac2} is between zero and one.
#'
#' @return A list with the following items
#' \describe{
#'   \item{h}{Test decision.}
#'   \item{CD}{Convergence Diagnostic (test statistic).}
#'   \item{pvalue}{The p-value.}
#'   \item{alpha}{The applied signifcicance level.}
#'   \item{frac1}{The fraction of data contained in the first interval.}
#'   \item{frac2}{The fraction of data contained in the second interval.}
#' }
#' @importFrom stats window start end spec.ar pnorm var
#' @keywords internal
geweke_test <- function(x, frac1 = 0.1, frac2 = 0.5, alpha = 0.05) {
  if (frac1 + frac2 > 1 | any(c(frac1, frac2) < 0) | any(c(frac1, frac2) > 1)) {
    stop("The input parameters 'frac1' and/or 'frac2' are invalid.")
  }
  if (alpha > 1 | alpha < 0) {
    stop("The input parameters 'alpha' is invalid.")
  }
  
  # number of variables
  x <- as.matrix(x)
  n <- ncol(x)
  
  # initialize
  CD <- pvalue <- h <- rep(NA, n)
  
  for (j in 1:n) {
    if (var(x[, j]) != 0) {
      # start and end dates
      x1start <- start(x[, j])
      x2start <- floor(end(x[, j]) - frac2 * (end(x[, j]) - start(x[, j])))
      x1end <- ceiling(start(x[, j]) + frac1 * (end(x[, j]) - start(x[, j])))
      x2end <- end(x[, j])
      
      # two series
      x1 <- window(x[, j], start = x1start, end = x1end)
      x2 <- window(x[, j], start = x2start, end = x2end)
      
      
      if ((var(x1) != 0) & (var(x2) != 0)) {
        
        # means
        m1 <- mean(x1)
        m2 <- mean(x2)
        
        # spectral densities
        sd1 <- tryCatch({spec.ar(x = x1, plot = FALSE)$spec[1]}, error = function(e) return(NA))
        sd2 <- tryCatch({
          spec.ar(x = x2, plot = FALSE)$spec[1]
          }, 
          error = function(e) {
            message("yo")
            return(NA)
          })
        
        # convergence diagnostic
        CD[j] <- (m1 - m2) / sqrt(sd1 / length(x1) + sd2 / length(x2))
        
        # p-value and test decision
        pvalue[j] <- 2 * (1 - pnorm(abs(CD[j])))
        h[j] <- 0 + (pvalue[j] < alpha)
      }
      
    }
  }
  
  # prepare results
  names(h) <- names(CD) <- names(pvalue) <- colnames(x)
  result <- list(
    h = h,
    CD = CD,
    pvalue = pvalue,
    alpha = alpha,
    frac1 = frac1,
    frac2 = frac2
  )
  return(result)
}

# ------------------------------------------------------------------------------

#' MCMC summary statistics
#' 
#' @description Computes MCMC summary statistics.
#'
#' @param x \code{R x n} matrix with \code{R} draws of \code{n} variables
#' @param HPDIprob Tprobability mass of the HPDI, a scalar between zero and one
#' @param frac1 probability mass of the first interval used for the Geweke test, a scalar
#'   between zero and one
#' @param frac2 probability mass of the second interval used for the Geweke test, a scalar
#'   between zero and one
#'
#' @details Naturally, \code{frac1 + frac2} is between zero and one.
#'
#' @return A data frame with the following columns
#' \describe{
#'   \item{Mean}{The posterior mean.}
#'   \item{Median}{The posterior median.}
#'   \item{SD}{Standard deviation.}
#'   \item{HPDI-LB}{Highest posterior density credible interval lower bound}
#'   \item{HPDI-UB}{Highest posterior density credible interval upper bound}
#'   \item{Naive SE}{Naive Standard error of the mean (ignoring chain autocorrelation.}
#'   \item{Time-series SE}{Time-series standard error (based on spectral density at 0).}
#'   \item{Geweke statistic}{The Geweke test statistic.}
#'   \item{frac1}{The fraction of data contained in the first interval.}
#'   \item{frac2}{The fraction of data contained in the second interval.}
#' }
#' 
#' @importFrom stats median spec.ar var
#' @keywords internal
mcmc_summary <- function(x, HPDIprob, frac1 = 0.1, frac2 = 0.5) {
  
  # number of variables
  x <- as.matrix(x)
  x <- x[, !apply(x, 2, function(a) all(is.na(a)))]
  n <- ncol(x)
  
  # initialize
  m <- md <- sd <- seNaive <- seTs <- tGeweke <- rep(NA, n)
  hpd <- matrix(NA, n, 2)
  
  for (j in 1:n) {
    # mean
    m[j] <- mean(x[, j])
    # median
    md[j] <- median(x[, j])
    # if (var(x[, j]) != 0) {
    try({
      # standard deviation
      sd[j] <- sqrt(var(x[, j]))
      # naive standard errors
      seNaive[j] <- sqrt(var(x[, j]) / length(x[, j]))
      # spectral densities standard errors
      seTs[j] <- sqrt(spec.ar(x = x[, j], plot = FALSE)$spec[1] / length(x[, j]))
      # HPDI
      hpd[j, ] <- hpd_interval(x[, j], prob = HPDIprob)
      # Geweke test
      tGeweke[j] <- geweke_test(x[, j], frac1 = frac1, frac2 = frac2)$CD
    }, silent = TRUE)
  }
  
  # prepare results
  result <- data.frame(m, md, sd, hpd, seNaive, 
                       seTs,
                       tGeweke, row.names = colnames(x))
  names(result) <- c(
    "Mean", "Median", "SD",
    paste0(HPDIprob * 100, "% HPDI-", c("LB", "UB")),
    "Naive SE",
    "Time-series SE",
    "Geweke statistic"
  )
  # attributes
  attr(result, "HPD probability") <- HPDIprob
  attr(result, "frac1") <- frac1
  attr(result, "frac2") <- frac2
  return(result)
}


# ------------------------------------------------------------------------------

#' MCMC summary statistics for states
#' 
#' @description Computes MCMC summary statistics for each state.
#'
#' @param model The return object of the function \code{fitSSM} from the package \code{KFAS}
#' @param state An array with the smoothed state
#' @inheritParams estimate_ssmodel
#' 
#' @return List of time series.
#' 
#' @importFrom stats ts start frequency
#' @keywords internal
results_state <- function(model, HPDIprob, state) {
  ts_start <- start(model$y)
  ts_freq <- frequency(model$y)
  tsl <- list()
  
  # smoothed states
  tsl$state_summary <- lapply(setdiff(colnames(state), "const"), function(x) {
    ts(
      mcmc_summary(x = t(state[, x, ]), HPDIprob = HPDIprob),
      start = ts_start, 
      frequency = ts_freq
    )
  })
  names(tsl$state_summary) <- setdiff(colnames(state), "const")
  tsl$state_summary <- do.call(cbind, tsl$state_summary)
  if (length(setdiff(colnames(state), "const")) == 1) {
    colnames(tsl$state_summary) <- paste0(setdiff(colnames(state), "const"), ".", colnames(tsl$state_summary) )
  }
  
  # retrieve mean and median
  tsl$state_mean <- tsl$state_summary[, grepl(".Mean", colnames(tsl$state_summary)), drop = FALSE]
  tsl$state_median <- tsl$state_summary[, grepl(".Median", colnames(tsl$state_summary)), drop = FALSE]
  colnames(tsl$state_mean) <- gsub(".Mean.*", "",   colnames(tsl$state_mean))
  colnames(tsl$state_median) <- gsub(".Median.*", "",   colnames(tsl$state_median))
  
  return(tsl)
}
