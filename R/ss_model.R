
# ------------------------------------------------------------------------------

#' State space model
#' 
#' @description Defines a state space model for the provided settings and data.
#'
#' @param settings list with model setting, in the format returned by the 
#'   function \code{initialize_settings}
#' @param data list with at least two named components: \code{tsm} is a multiple 
#'   time series object that contains all observation series, \code{weights} 
#'   is a named list of time series with (nominal) weights, the list names 
#'   correspond to the different groups, i.e., \code{group1, group2, subgroup1}, 
#'   if present in the model
#' 
#' @details \code{data} is preferably the output of funtion \code{prepare_data}.
#' 
#' @return A state space model object of class \code{ss_model}, which consists 
#'   of an object returned by the function \code{SSModel} of 
#'   the package \code{KFAS} and in addition a list item called \code{names} 
#'   which contains information on the parameters to be estimated. 
#'
#' @importFrom stats window start end
#' @importFrom KFAS SSModel SSMcustom
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
define_ssmodel <- function(
  settings, 
  data
){
  
  # save call
  mc <- match.call(expand.dots = FALSE)
  
  # to avoid RMD check note
  variable <- type <- max_lag <- loading_state <- parameter_name <- NULL
  
  # convert to time series list
  tsm <- data$tsm
  weightl <- data$weights
  tsl <- as.list(tsm)
  
  # settings to data frames
  df_set <- settings_to_df(x = settings)
  endo <- df_set$obs$variable
  
  restr <- constr <- FALSE
  
  # select variables
  if (length(endo) > 1) tsl <- tsl[endo]

  # set constraints
  for (ix in rownames(df_set$constr)) {
    df <- df_set$constr[ix, , drop = FALSE]
    endo <- c(endo, paste0("constr_", df$type, "_", df$group))
    weightl[[df$group]] <- window(weightl[[df$group]], start = start(tsm), end = end(tsm), extend = TRUE)
    constr <- TRUE 
    tsl <- c(tsl, list(ts_c(c = 0, tsm = tsm)))
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
        names_expand <- c(names_expand, df$load_name, settings[[df$group]]$variable)
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
    
    sys$Zt[paste0("constr_", df$type, "_", df$group), paste0(df$type, "_", df$load_name), ] <- -1
    sys$Zt[paste0("constr_", df$type, "_", df$group), paste0(df$type, "_", settings[[df$group]]$variable), ] <- t(weightl[[df$group]][, !idx_residual])

    if (df$type == "trend") {
      sys$Zt[paste0("constr_", df$type, "_", df$group), paste0(df$type, "_", df$load_name, "_L1"), ] <- 1
      sys$Zt[paste0("constr_", df$type, "_", df$group), paste0(df$type, "_", settings[[df$group]]$variable, "_L1"), ] <- -t(weightl[[df$group]][, !idx_residual])
      
    }
        
    if (df$residual) {
      if (df$type == "trend") {
        sys$Zt[paste0("constr_", df$type, "_", df$group), "const", 2:dim(sys$Zt)[3]] <- diff(weightl[[df$group]][, idx_residual])
      } else {
        sys$Zt[paste0("constr_", df$type, "_", df$group), "const", ] <- weightl[[df$group]][, idx_residual]
      }
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
  if (dim(model$Z)[1] == 1) rownames(model$Z) <- rownames(sys$Zt)
  
  model$names <- sys$names
  class(model) <- c("ss_model", class(model))
  attr(model, "call") <- mc
  

  return(model)
  
}

# ------------------------------------------------------------------------------

#' State space model update
#'
#' @description Updates the system matrices of a state space model.
#'
#' @param pars named vector with all parameters
#' @param model state space model object (unassigned)
#' @param model_last state space model object (previous draw)
#' @param df_set data frame with model settings
#' @inherit define_ssmodel
#' 
#' @return The state space model object \code{model} with updates matrices.
#' @keywords internal
update_ssmodel <- function(
  pars, 
  model, 
  model_last,
  settings,
  df_set
) {
  
  pars_info <- model$names
  
  # parameter names
  names_par <- names(pars)
  
  # 1a) variances 
  target_order <- rownames(model$R[,,1])[rownames(model$R[,,1]) %in% df_set$variance$state]
  df <- df_set$variance[match(target_order, df_set$variance$state), ]
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
    loc$T1 <- df_set$const$state[ix]
    loc$T2 <- df_set$const$state_lag[ix]
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
    loc$T1 <- df_set$AR$state[ix]
    loc$T2 <- df_set$AR$state_lag[ix]
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
        # stop("The stationary part of the model is close to being non-stationary, please respecify.")
        # warning(paste0("Stationarity problem when updating P1 for variable'", name , "', reusing values of last draw of P1."))
        # return(model_last$P1[idx, idx])
        stop(paste0("Problem when updating P1 for variable'", name , "', reusing last model draw."))
        return(NA)
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
