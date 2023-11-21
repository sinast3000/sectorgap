
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
#'  \code{1} means no draw is discarded, \code{2} means each second draw is 
#'  kept, and so on
#' @param HPDIprob probability of highest posterior density interval, the 
#'  default is \code{HPDIprob = 0.68}
#'
#' @return An object of class \code{fit}.
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
#'   prior = prior,
#'   R = 100
#' )
#' }
estimate_ssmodel <- function(
  model, 
  settings, 
  prior = initialize_prior(model), 
  R = 10000, 
  burnin = 0.5, 
  thin = 1, 
  HPDIprob = 0.68
){
  
  # save call
  mc <- match.call(expand.dots = FALSE)
  
  # to avoid RMD check note
  variable <- parameter_name <- distribution <- NULL
  
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
  ssmodel_k <- update_ssmodel(
    pars = pars, 
    model = model, 
    settings = settings,
    df_set = df_set
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
      "\tNumber of draws \t\t", R, "\n",
      "\tSkipped draws\t", thin - 1, "/", thin
    )
  )
  
  timec <- rep(0, 6)
  
  # loop
  message("Obtaining draws ...")
  pb <- utils::txtProgressBar(min = 0, max = R, style = 3)
  for (r in 1:R) {
    
    # update progress bar
    if (r %% ceiling(R/100) == 0) {
      utils::setTxtProgressBar(pb, r)
    }
    
    end.time1 <- Sys.time()
    
    # step 1: states -----------------------------------------------------------
    
    # apply simulation smoothing, conditional on parameters from step r-1
    state_smoothed <- ts(
      simulateSSM(ssmodel_k, type = "states", nsim = 1)[,,1],
      start = hlp$start, 
      frequency = hlp$frequency
    )
    
    end.time2 <- Sys.time()
    
    # step 2: trends -----------------------------------------------------------
    pars[hlp$step2$names] <- draw_trend_innovations(
      model = ssmodel_k,
      state = state_smoothed,
      df_var = hlp$step2$variance_ncycle,
      df_cov = hlp$step2$covariance,
      df_prior = df_prior
    )[hlp$step2$names]
    
    end.time3 <- Sys.time()
    
    # AR drift
    for (x in seq_len(length(hlp$step2AR))) {
      lx <- hlp$step2AR[[x]]
      
      pars[c(lx$const, lx$phi)] <- .postARp_phi(
        Y = state_smoothed[, lx$state], 
        phi = pars[lx$phi], 
        phiDistr = df_prior[, lx$phi, drop = FALSE], 
        sigma = pars[lx$var_cycle], 
        const = pars[lx$const], 
        constDistr = df_prior[, lx$const, drop = FALSE]
      )[c(lx$const, lx$phi)]
    }
    
    # step 3: output gap -------------------------------------------------------
    
    pars[hlp$step3$pars_regression] <- draw_output_gap(
      Y = state_smoothed[, paste0("cycle_", hlp$step3$endo)], 
      phi = pars[hlp$step3$phi],
      phiDistr =  df_prior[, hlp$step3$phi, drop = FALSE],
      sigma = pars[hlp$step3$var_cycle],
      sigmaDistr =  df_prior[, hlp$step3$var_cycle, drop = FALSE]
    )[hlp$step3$pars_regression]
      
    end.time4 <- Sys.time()
    
    # step 4: all remaining observation equations ------------------------------
    # assumption: observation equations have no constant to estimate
    
    # compute Y for all variables simultaneously
    # substract constant and trend from observation
    Y <- model$y[, endo] - ts(sapply(1:dim(ssmodel_k$Z)[3], function(x) {
      ssmodel_k$Z[endo, hlp$idx$T_ncycle, x] %*% 
        state_smoothed[x, hlp$idx$T_ncycle]
    }) %>% t,
      start = hlp$start, 
      frequency = hlp$frequency
    )
    colnames(Y) <- endo

    end.time5 <- Sys.time()
    
    for (x in endo[-1]) {
      
      lx <- hlp$step4[[x]]

      # draw parameters
      pars[lx$pars_regression] <- .postRegression(
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
    
    # -------------------------------------------------------------------------- 
    
    end.time6 <- Sys.time()
    
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
    ssmodel_k <- update_ssmodel(
      pars = pars, 
      model = model, 
      settings = settings,
      df_set = df_set
    )
    
    end.time7 <- Sys.time()
    # 
    # 
    timec <- timec + c(
      end.time2 - end.time1,
      end.time3 - end.time2, 
      end.time4 - end.time3,
      end.time5 - end.time4, 
      end.time6 - end.time5, 
      end.time7 - end.time6
    )
    
  }
  close(pb)
  message("Done.")

  print(paste0(round(as.numeric(timec) / sum(as.numeric(timec)) * 100, 1), sep = ", "))
  
  # save data including burnin phase
  colnames(state) <- colnames(state_smoothed)
  mcmc <- list(
    state = state,
    parameters = param,
    state_gap = state_gap,
    prior = prior,
    R = R, 
    burnin = burnin, 
    thin = thin
  )
  
  # obtain results
  fit <- compute_mcmc_results(
    model = model,
    settings = settings, 
    HPDIprob = HPDIprob, 
    mcmc = mcmc
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
  
  # multiply Z and state vector piecewise (for each point in time)
  gaps <- ts(
    t(sapply(1:dim(X)[1], function(x) {
      model$Z[idx_obs, idx_state, x] %*% X[x, ]
    })),
    start = start(X),
    frequency = frequency(X)
  )
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
  type <- variable <- . <- NULL
  
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
  hlp$step3 <- hlp$step4[[endo[[1]]]]

  return(hlp)
}
