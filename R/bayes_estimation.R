
# ------------------------------------------------------------------------------

#' Bayesian estimation via Gibbs sampling
#' 
#' @description Estimates the parameters and states of a multi-dimensional 
#' state space model by Bayesian methods using a Gibbs sampling procedure.
#'
#' @inheritParams define_ssmodel
#' @param model state space model object, returned by the function 
#'   \code{define_ssmodel}
#' @param prior list of matrices, each list item corresponds to one endogenous 
#'   variable. See \code{initialize_prior}
#' @param R number of draws, the default is \code{10000}
#' @param burnin share of draws as burnin period, the default is \code{0.5}
#' @param thin thinning parameter defining how many draws are discarded. 
#'   \code{1} means no draw is discarded, \code{2} means each second draw is 
#'   kept, and so on
#' @param HPDIprob probability of highest posterior density interval, the 
#'  default is \code{HPDIprob = 0.68}
#' @param fit already fitted object of class \code{ss_fit}, to continue drawing, 
#'   see details
#'
#' @details If \code{fit} is supplied, the function will continue drawing 
#'   \code{R} additional repetitions. In this case, all input variables except 
#'   for \code{fit} and \code{R} are ignored.
#'
#' @return An object of class \code{ss_fit}.
#'
#' @importFrom KFAS simulateSSM
#' @importFrom stats start ts frequency
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom dplyr %>% select
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
#' model <- define_ssmodel(
#'   settings = settings, 
#'   data = data
#' )
#' prior <- initialize_prior(
#'   model = model, 
#'   settings = settings
#' ) 
#' \donttest{
#' fit <- estimate_ssmodel(
#'   model = model, 
#'   settings = settings, 
#'   data = data,
#'   prior = prior,
#'   R = 100
#' )
#' }
estimate_ssmodel <- function(
  model, 
  settings, 
  data,
  prior = initialize_prior(model), 
  R = 10000, 
  burnin = 0.5, 
  thin = 1, 
  HPDIprob = 0.68,
  fit = NULL
){
  
  # save call
  mc <- match.call(expand.dots = FALSE)
  
  # to avoid RMD check note
  variable <- parameter_name <- distribution <- NULL
  
  # fitted object present (continue drawing)
  if (!is.null(fit)) {
    message("Continue drawing for fitted object.")
    model <- fit$model
    settings <- fit$settings
    data <- fit$data
    prior <- fit$prior
    burnin <- attr(fit, "burnin")
    thin <- attr(fit, "thin")
    HPDIprob <- attr(fit, "HPDIprob")
  }
  
  # settings to data frames (for ssm parameter assignment)
  df_set <- settings_to_df(x = settings)
  endo <- df_set$obs$variable
  
  # helper list with settings (for posterior assignment)
  hlp <- helper_posterior_assignment(
    model = model,
    df_set = df_set, 
    prior = prior,
    endo = endo
  )
  
  # ----- prior
  
  # prior distributions
  # df_prior <- do.call(cbind, prior)
  df_prior <- prior %>% select(-variable, -parameter_name, -distribution) %>% t
  colnames(df_prior) <- prior$parameter_name
  nPar <- ncol(df_prior)
  
  # initial values
  pars <- df_prior["ini", ]
  
  # covariances
  pars_covar <- sapply(df_set$covariance$parameter_name, function(x) 0)
  if (length(pars_covar)>0) {
    pars <- c(pars, pars_covar)
    nPar <- nPar + length(pars_covar)
  }
  
  # ----- model
  
  # dimensions
  N <- dim(model$y)[2]
  Tt <- dim(model$y)[1]
  k <- dim(model$T)[1]
  
  # initialize
  if (!is.null(fit)) pars <- fit$mcmc$parameters[dim(fit$mcmc$parameters)[1], ]
  tryCatch({
    ssmodel_k <- update_ssmodel(
      pars = pars, 
      model = model,
      settings = settings,
      df_set = df_set
    )},
    error = function(cont) {
      stop("State space update problem, check model and prior.")
    }
  )
  
  # ----- Gibbs procedure
  
  # initialization
  state <- state_f <- array(0, dim = c(Tt, k, R / thin), 
                            dimnames = list(NULL, colnames(model$Z), NULL))
  state_gap <- array(0, dim = c(Tt, length(df_set$obs$variable), R / thin), 
                     dimnames = list(NULL, paste0("gap_", df_set$obs$variable), NULL))
  param <- array(0, c(R / thin, nPar),
                 dimnames = list(NULL, names(pars)))
  count <- 0

  # print details and progress
  message(
    paste0(
      "Bayesian Estimation\n",
      "\tNumber of draws \t", R, "\n",
      "\tSkipped draws\t\t", thin - 1, "/", thin
    )
  )
  
  # loop
  message("Obtaining draws ...")
  pb <- utils::txtProgressBar(min = 0, max = R, style = 3)
  # for (r in 1:R) {
  r <- 0
  while(count < R / thin) {
    r <- r + 1
    
    # update progress bar
    if (r %% ceiling(R/100) == 0) {
      utils::setTxtProgressBar(pb, r)
    }
    
    # step 1: states -----------------------------------------------------------

    # apply simulation smoothing, conditional on parameters from step r-1
    state_smoothed <- tryCatch({
      x <- ts(
        simulateSSM(ssmodel_k, type = "states", nsim = 1)[,,1],
        start = hlp$start, 
        frequency = hlp$frequency
      )
      if (any(is.nan(x))) {
        stop("NaN in state")
      }
      x
      },
      error = function(cont) {
        warning("Simulation smoother problem, skipping draw.")
        r <<- r - 1
        return(state_smoothed)
      }
    )
    # ssmodel_k$y[is.na(model$y)] <- matmult3d(a = state_smoothed, b = ssmodel_k$Z)[is.na(model$y)]
    
    # update non-linear constraints if present
    ssmodel_k <- update_nonlinear_constraints(
      state = state_smoothed,
      model = ssmodel_k, 
      settings = settings,
      df_constr = hlp$linear_constraints,
      data = data
    )
    
    # step 2: trends -----------------------------------------------------------
    pars[hlp$step2$names] <- draw_trend_innovations(
      model = ssmodel_k,
      state = state_smoothed,
      df_var = hlp$step2$variance_ncycle,
      df_cov = hlp$step2$covariance,
      df_prior = df_prior
    )[hlp$step2$names]
    
    # AR drift
    for (x in seq_len(length(hlp$step2AR))) {
      lx <- hlp$step2AR[[x]]
      
      pars[c(lx$const, lx$phi)] <- postARp_phi(
        Y = state_smoothed[, lx$state], 
        phi = pars[lx$phi], 
        phiDistr = df_prior[, lx$phi, drop = FALSE], 
        sigma = pars[lx$var_cycle], 
        const = pars[lx$const], 
        constDistr = df_prior[, lx$const, drop = FALSE]
      )[c(lx$const, lx$phi)]
    }
    
    # step 3: observation equations without loadings ---------------------------
    # e.g. the output gap
    
    for (x in names(hlp$step3)) {
      
      lx <- hlp$step3[[x]]
      
      pars[lx$pars_regression] <- draw_output_gap(
        Y = state_smoothed[, paste0("cycle_", lx$endo)], 
        phi = pars[lx$phi],
        phiDistr =  df_prior[, lx$phi, drop = FALSE],
        sigma = pars[lx$var_cycle],
        sigmaDistr =  df_prior[, lx$var_cycle, drop = FALSE]
      )[lx$pars_regression]
    }
      
    # step 4: all remaining observation equations ------------------------------
    # assumption: observation equations have no constant to estimate
    
    if (length(hlp$step4) > 0) {
      # compute Y for all variables simultaneously
      # substract constant and trend from observation
      Y <- model$y[, endo] - 
        matmult3d(b = ssmodel_k$Z[endo, hlp$idx$T_ncycle, , drop = FALSE], a = state_smoothed[, hlp$idx$T_ncycle])
      colnames(Y) <- endo
  
      
      for (x in names(hlp$step4)) {
  
        lx <- hlp$step4[[x]]
  
        # draw parameters
        pars[lx$pars_regression] <- post_regression(
          Y = Y[, x], 
          X = state_smoothed[, lx$Xnames], 
          beta = pars[lx$load], 
          betaDistr = df_prior[, lx$load, drop = FALSE], 
          sigma = pars[lx$var_cycle],
          sigmaDistr = df_prior[, lx$var_cycle, drop = FALSE], 
          phi = pars[lx$phi],
          phiDistr = df_prior[, lx$phi, drop = FALSE]
        )[lx$pars_regression]
  
      }
    }
    
    # -------------------------------------------------------------------------- 
    
    # save draws and thin
    if (r %% thin == 0) {
      count <- count + 1
      state[, , count] <- state_smoothed
      param[count, ] <- pars

      # compute actual gap per obs      
      state_smoothed_gap <- compute_gaps(
        model = ssmodel_k, 
        state = state_smoothed, 
        idx_state = hlp$idx$T_cycle,
        idx_obs = hlp$idx$Z_endo
      )
      state_gap[, , count] <- state_smoothed_gap 
    }
    
    
    # update model
    ssmodel_k <- tryCatch({
      update_ssmodel(
        pars = pars, 
        model = model,
        model_last = ssmodel_k, 
        settings = settings,
        df_set = df_set
      )},
      error = function(cont) {
        warning("State space update problem, skipping draw.")
        r <<- r - 1
        return(ssmodel_k)
      }
    )

  }
  close(pb)
  message("Done.")
  
  # save data including burnin phase
  colnames(state) <- colnames(state_smoothed)
  if (!is.null(fit)) {
    mcmc <- list(
      state = array(
        c(fit$mcmc$state, state), 
        dim = c(dim(state) + c(0, 0, dim(fit$mcmc$state)[3])),
        dimnames = list(NULL, colnames(state))
      ),
      parameters = rbind(fit$mcmc$parameters, param),
      state_gap = array(
        c(fit$mcmc$state_gap, state_gap), 
        dim = c(dim(state_gap) + c(0, 0, dim(fit$mcmc$state_gap)[3])),
        dimnames = list(NULL, colnames(state_gap))
      ),
      prior = prior,
      R = R + attr(fit, "R"), 
      burnin = burnin, 
      thin = thin
    )
  } else {
    mcmc <- list(
      state = state,
      parameters = param,
      state_gap = state_gap,
      prior = prior,
      R = R, 
      burnin = burnin, 
      thin = thin
    )
  }
  
  resl <- list(
    model = model,
    settings = settings, 
    HPDIprob = HPDIprob,
    mcmc = mcmc,
    prior = prior,
    data = data
  )
  # obtain results
  fit <- tryCatch({
    do.call(
      compute_mcmc_results, 
      resl
    )},
    error = function(cont) {
      message("MCMC evalutaion problem, returning object without MCMC evaluation.")
      return(resl)
    }
  )
  attr(fit, "call") <- mc
  
  
  return(fit)
}

# ------------------------------------------------------------------------------

#' Gaps of observation equations
#' 
#' @description Computes the gap of each observable, i.e., it sums up the 
#' respective cycle and all cycles the observable additionally loads on.
#'
#' @inheritParams estimate_ssmodel
#' @param state state vector
#' @param idx_state names of all cycle states
#' @param idx_obs names of all observations (excluding constraints)
#' 
#' @return A multiple time series object with gaps.
#' @keywords internal
compute_gaps <- function(
  model, 
  state, 
  idx_state,
  idx_obs
) {
  
  # exclude all constant, trend and drift states
  X <- state[, idx_state]
  
  if (dim(model$Z)[3] > 1) {
    # multiply Z and state vector piecewise (for each point in time)
    gaps <- ts(
      t(sapply(1:dim(X)[1], function(x) {
        model$Z[idx_obs, idx_state, x] %*% X[x, ]
      })),
      start = start(X),
      frequency = frequency(X)
    )
  } else {
    gaps <- ts(
      t(
        model$Z[idx_obs, idx_state, 1] %*% t(X)
      ),
      start = start(X),
      frequency = frequency(X)
    )  
  }
  colnames(gaps) <- paste0("gap_", idx_obs)
  
  return(gaps)
  
}

# ------------------------------------------------------------------------------

#' Settings for draws from posterior
#' 
#' @description Creates a list of settings used during the Gibbs sampling 
#' algorithm.
#'
#' @inheritParams update_ssmodel
#' @param endo character vector of endogenous variable names
#' 
#' @importFrom dplyr %>% filter
#' 
#' @return A list of items necessary to run each each step of the 
#' Gibbs sampler.
#' @keywords internal
helper_posterior_assignment <- function(
  model,
  df_set, 
  prior,
  endo
) {
  
  # to avoid RMD check note
  type <- variable <- . <- linear <- NULL
  
  # helper list for posterior draws
  hlp <- list(
  
    # time series properties
    start = start(model$y),
    end = end(model$y),
    frequency = frequency(model$y),
    
    # 
    idx = list(
      Z = colnames(model$y),
      Z_endo = colnames(model$y)[colnames(model$y) %in% endo],
      T = colnames(model$T) ,
      T_cycle = colnames(model$T)[grepl("cycle", colnames(model$T))],
      T_ncycle = colnames(model$T)[!grepl("cycle", colnames(model$T))]
    ),
    
    # step 2: trend and drift variances
    step2 = within(list(), {
      variance_ncycle <- df_set$variance %>% filter(type == "trend" | type == "drift")
      covariance <- df_set$covariance
      names <- variance_ncycle$parameter_name
    }),
    
    # step 2 AR: AR drifts
    step2AR = sapply(seq_len(NROW(df_set$AR_drift)), function(x) list(df_set$AR_drift[x, ])),
    
    # setp 4: observation equations (excluding trends)
    step4 = lapply(endo, function(x) {
      within(list(), {
        endo <- x
        pars <- (prior %>% filter(variable == x) %>% .$parameter_name)
        load <- pars[grepl("_load_", pars)]
        load_type <- gsub("_load_.*", "", load)
        load_y <- gsub("_.*", "",gsub(paste0(".*", x, "_"),"", load))
        load_lag <- as.numeric(substr_r(load, 1))
        load_series <- paste0(load_type, "_", load_y)
        phi <- pars[grepl(paste0("cycle_", x, "_AR"), pars)]
        var_cycle <- pars[grepl("var_cycle", pars)]
        pars_regression <- c(phi, var_cycle, load)
        Xnames <- gsub("_L0", "", paste0(load_series, "_L", load_lag))
      })
    })
  )
  names(hlp$step4) <- endo
  hlp$step3 <- hlp$step4[!(endo %in% df_set$loadings$variable)]
  hlp$step4 <- hlp$step4[endo %in% df_set$loadings$variable]
  
  # for one dimensional model
  if (is.null(hlp$idx$Z_endo)) hlp$idx$Z_endo <- endo
  
  # linear trend constraints
  if (!is.null(df_set$constr)) hlp$linear_constraints <- df_set$constr %>% filter(type == "trend", !linear)

  return(hlp)
}
