
# ------------------------------------------------------------------------------

#' Creates the system matrices for a state space model
#'
#' @param settings list with model setting (on trend, cycle, etc)
#' @param tsm time series with all observation series (as matrix)
#' @param weightl named list of time series with weights
#' 
#' @return A state space model object returned by the function \code{SSModel} of 
#'   the package \code{KFAS} and in addition a list item called \code{names} 
#'   which contains information on the parameters to be estimated. 
#'
#' @importFrom stats window start end
#' @importFrom KFAS SSModel SSMcustom
define_ssmodel <- function(
  settings, 
  tsm, 
  weightl = NULL
){

  # convert to time series list
  tsl <- as.list(tsm)
  
  # settings to data frames
  df_set <- settings_to_df(x = settings)
  endo <- df_set$obs$variable
  
  restr <- constr <- FALSE
  
  # select variables
  tsl <- tsl[endo]

  # set constraints
  for (ix in rownames(df_set$constr)) {
    df <- df_set$constr[ix, , drop = FALSE]
    endo <- c(endo, paste0("constr_", df$type, "_", df$group))
    weightl[[df$group]] <- window(weightl[[df$group]], start = start(tsm), end = end(tsm), extend = TRUE)
    constr <- TRUE 
    tsl <- c(tsl, list(ts_c(c = 0, ts = tsm)))
    names(tsl)[length(tsl)] <- paste0("constr_", df$type, "_", df$group)
  }

  # set restrictions
  if (!is.null(settings$restr)) {
    restr <- TRUE
    endo <- c(endo, paste0("restr_", 1:length(settings$restr)))
  }

  # convert time series list back to matrix
  tsm <- do.call(cbind, tsl)
  
  # initialize system
  sys <- initialize_ss(endo, df_settings = df_set)
  
  # add trends
  for (ix in 1:NROW(df_set$obs)) {
    df <- df_set$obs[ix, ]
    if (df$trend > 0) {
        sys <- add_trend(sys = sys, name = df$variable, type = df$trend)
        sys$Zt[df$variable, paste0("trend_", df$variable)] <- 1
    }
  }

  # add cycles
  for (ix in df_set$obs$variable) {
    lag <- df_set$lag %>% 
      filter(variable == ix, type == "cycle") %>% 
      dplyr::select(max_lag) %>% as.numeric
    df <- df_set$obs %>%
      filter(variable == ix)
    sys <- add_cycle(sys = sys, name = ix, p = df$cycle, lags = lag)
    sys$Zt[ix, paste0("cycle_", ix)] <- 1
  }
  
  # add loadings
  for (ix in unique(df_set$loadings$loading_state)) {
    df_tmp <- df_set$loadings %>%
      filter(loading_state == ix) %>%
      dplyr::select(variable, loading_state, parameter_name)
      
    sys$Zt[df_tmp$variable, df_tmp$loading_state] <- NA
  }
  sys$names$par <- c(sys$names$par, df_set$loadings$parameter_name)
  
  # constraint (add third dimension)
  if (constr) {

    names_expand <- NULL
    for (ix in rownames(df_set$constr)) {
      df <- df_set$constr[ix, , drop = FALSE]
      
      if (df$type == "trend") {
        names_expand <- c(names_expand, df$name_agg, settings[[df$group]]$variable)
      }
    }
    
    # expand system
    for (iz in unique(names_expand)) {
      sys <- add_lag(sys = sys, name = iz, type = df$type, lags = 1)
    }
    
    n_t <- NROW(tsm) 
    tmp <- array(rep(sys$Zt,n_t), c(dim(sys$Zt), n_t))
    colnames(tmp) <- colnames(sys$Zt)
    rownames(tmp) <- rownames(sys$Zt)
    sys$Zt <- tmp
    
  }
  # constraint 
  for (ix in rownames(df_set$constr)) {
    df <- df_set$constr[ix, , drop = FALSE]

    idx_residual <- rep(FALSE, NCOL(weightl[[df$group]]))
    if (df$residual) {
      idx_residual <- colnames(weightl[[df$group]]) == settings[[df$group]]$name_residual
    }
    
    sys$Zt[paste0("constr_", df$type, "_", df$group), paste0(df$type, "_", df$name_agg), ] <- -1
    sys$Zt[paste0("constr_", df$type, "_", df$group), paste0(df$type, "_", settings[[df$group]]$variable), ] <- t(weightl[[df$group]][, !idx_residual])

    if (df$type == "trend") {
      sys$Zt[paste0("constr_", df$type, "_", df$group), paste0(df$type, "_", df$name_agg, "_L1"), ] <- 1
      sys$Zt[paste0("constr_", df$type, "_", df$group), paste0(df$type, "_", settings[[df$group]]$variable, "_L1"), ] <- -t(weightl[[df$group]][, !idx_residual])
      
    }
        
    if (df$residual) {
      ts_residual <- diff(tsl[[df$name_agg]]) - Reduce("+", as.list(diff(tsm[, settings[[df$group]]$variable]) * weightl[[df$group]][, !idx_residual]))
      ts_residual <- hpfilter(ts_residual, lambda = 1600)
      # TODO: consider transformation functions
      sys$Zt[paste0("constr_", df$type, "_", df$group), "const", ] <- c(NA, ts_residual)
      
    }
    
  }

  # other restrictions  
  if (restr) {
    if (is.na(dim(sys$Zt)[3])) {
      n_t <- NROW(tsm) 
      tmp <- array(rep(sys$Zt,n_t), c(dim(sys$Zt), n_t))
      colnames(tmp) <- colnames(sys$Zt)
      rownames(tmp) <- rownames(sys$Zt)
      sys$Zt <- tmp
    }
    for (ii in 1:length(settings$restr)) {
      sys$Zt[paste0("restr_",ii) ,settings$restr[[ii]]$col, settings$restr[[ii]]$time] <- 1 #settings$restr[[ii]]$value
    }
  }
  
  # add A1, etc
  sys <- add_init_mat(sys = sys)
  
  # add empty covariance matrix for observation equation
  sys$Ht <- matrix(0, NROW(sys$Zt), NROW(sys$Zt))

  # number of parameters
  sys$npar <- length(sys$names$par)
  
  # KFAS model object
  model <- SSModel(
    tsm ~ -1 + SSMcustom(
      Z = sys$Zt, T = sys$Tt, R = sys$Rt, Q = sys$Qt,
      a1 = sys$a1, P1 = sys$P1, P1inf = sys$P1inf, 
      state_names = colnames(sys$Tt)
    ),
    H = sys$Ht,
    data = tsm
  )
  
  model$names <- sys$names
  # return(list(sys = sys, tsm = tsm, model = model))
  return(model)
  
}

# ------------------------------------------------------------------------------

#' Updates the system matrices for a state space model
#'
#' @param pars named vector with all parameters
#' @param model state space model object
#' @param df_set data frame with model settings
#' @inherit define_ssmodel
#' 
#' @return The state space model object \code{model} with updates matrices.
#'
update_ssmodel <- function(
  pars, 
  model, 
  settings,
  df_set
) {
  
  pars_info <- model$names
  
  # parameter names
  names_par <- names(pars)
  
  # 1a) variances 
  target_order <- rownames(model$R[,,1])[rownames(model$R[,,1]) %in% df_set$variance$variable]
  df <- df_set$variance[match(target_order, df_set$variance$variable), ]
  diag(model$Q[, , 1]) <- pars[df$parameter_name]
  
  # 1b) covariances
  if (NROW(df_set$covariance)>1) {
    tmp <- model$Q[, , 1]
    colnames(tmp) <- rownames(tmp) <- target_order

    for(ix in seq_len(NROW(df_set$covariance))) {
      tmp[df_set$covariance$variable[ix], df_set$covariance$variable2[ix]] <- pars[df_set$covariance$parameter_name[ix]]
    }
    model$Q[, , 1] <- tmp
  }

  # 2) constants
  loc <- list()
  for (ix in seq_len(NROW(df_set$const))) {
    loc$T1 <- df_set$const$variable[ix]
    loc$T2 <- df_set$const$variable_lag[ix]
    model$T[loc$T1, loc$T2, ] <- pars[df_set$const$parameter_name[ix]]
  }

  # 3) loadings in Z
  df_tmp <- df_set$loadings
  # direct loadings
  for (ix in seq_len(NROW(df_tmp))) {
    
    Z1 <- df_tmp$variable[ix]
    Z2 <- df_tmp$loading_state[ix]
    
    load <- pars[df_tmp$parameter_name[ix]]
    
    model$Z[Z1, Z2, ][is.na(model$Z[Z1, Z2, ])] <- 0
    model$Z[Z1, Z2, ] <- model$Z[Z1, Z2, ] + load
  }
  # indirect loadings
  df_tmp <- df_set$loadings_extra
  for (ix in seq_len(NROW(df_tmp))) {
    
    Z1 <- df_tmp$variable[ix]
    Z2 <- df_tmp$loading_state[ix]
    
    load <- pars[df_tmp$parameter_name_direct[ix]] * pars[df_tmp$parameter_name_indirect[ix]]
    
    model$Z[Z1, Z2, ][is.na(model$Z[Z1, Z2, ])] <- 0
    model$Z[Z1, Z2, ] <- model$Z[Z1, Z2, ] + load
    
  }

  # 4) cycle autoregressive parameters in T
  for (ix in seq_len(NROW(df_set$AR))) {
    loc$T1 <- df_set$AR$variable[ix]
    loc$T2 <- df_set$AR$variable_lag[ix]
    model$T[loc$T1, loc$T2, ] <- pars[df_set$AR$parameter_name[ix]]
  }

  # 5) update a1, P1, P1inf
  indexStat <- pars_info$stationary["const" != pars_info$stationary]
  indexRoot <- pars_info$root
  nStat <- length(indexStat)
  nRoot <- length(indexRoot)

  # as long as all stationary components and all root components are independent, i can do the following:
  RQR <- model$R[,,1] %*% model$Q[,,1] %*% t(model$R[, , 1])
  RQR <- RQR[indexStat,indexStat]

  # computational burden increases exponentially, so im cutting into smaller chunks
  # valid as long as all cycles are independent:
  state_stat <- gsub("_L.*","", pars_info$stationary[pars_info$stationary != "const"]) %>% unique
  for (name in state_stat) {

    idx <- pars_info$stationary[grepl(name, pars_info$stationary)]
    n_idx <- length(idx)
    tryCatch({
      model$P1[idx, idx] <- matrix(
        solve(
          diag(n_idx^2) - as.matrix(kronecker(
            model$T[idx, idx, 1], model$T[idx, idx, 1])
          )) %*% 
          c(RQR[idx, idx]), 
        n_idx, 
        n_idx
      )},
      error = function(cont) {
        stop("The stationary part of the model is close to being non-stationary, please respecify.")
      }
    )
  }
  model$P1["const", "const"] <- 0
  model$P1inf[] <- 0
  model$P1inf[indexRoot, indexRoot] <- diag(nRoot)
  
  # update unconditional mean
  model$a1[indexStat, ] <- solve(diag(nStat) - model$T[indexStat, indexStat, 1]) %*% model$T[indexStat, "const", 1]

  return(model)
  
}

