
# ------------------------------------------------------------------------------

#' Initializes a state space model
#'
#' @param name names of observations
#' @param df_settings list of data frames with settings
#' 
initialize_ss <- function(name, df_settings) {
  
  k <- length(name)
  n_const <- 1 
  const_names <- c("const")
  
  sys <- list()
  sys$Zt <- matrix(0, k, n_const)
  sys$Ht <- matrix(0, k, k)
  sys$Tt <- diag(n_const)
  sys$Tt[-1,-1] <-0
  sys$Qt <- matrix(0, 0, 0)
  sys$Rt <- matrix(0, n_const, 0)
  
  sys$names <- list()
  sys$names$stationary <- const_names
  colnames(sys$Zt) <- colnames(sys$Tt) <- rownames(sys$Tt) <- rownames(sys$Rt) <- const_names
  rownames(sys$Zt) <- name
  
  sys
  
}

# ------------------------------------------------------------------------------

#' Add a trend to a state space model
#'
#' @param sys list with system matrices
#' @param type type of trend, see details
#' @param name name ob observation equation
#' @param const logical indicating if there is a constant
#' 
#' @details \code{type = 1} denotes a random walk, \code{type = 2} an integrated
#'  random walk, \code{type = 3} and random walk with AR drift, and
#'  \code{type = 4} a local linear trend. \code{type = 3} is currently not 
#'  implemented.
#' 
#' @return The input list \code{sys} with updated matrices.
#'
add_trend <- function(sys, type, name, const = FALSE) {
  
  k <- nrow(sys$Zt)
  m <- ncol(sys$Tt)
  n_var <- ncol(sys$Rt)
  
  if (is.null(m)) {
    m <- 0
    n_var <- 0
  }
  
  obs_names <- rownames(sys$Zt)
  state_names <- colnames(sys$Tt)
  var_names <- colnames(sys$Qt)
  
  # expand matrices
  const_names_new <- AR_names_new <- NULL
  if (type == 1) { 
    # random walk with constant drift
    sys$Zt <- cbind(sys$Zt, rep(0, k))
    sys$Tt <- rbind(cbind(sys$Tt, rep(0, m)),
                    c(ifelse(const, NA, 0), rep(0, m - 1), 1))
    sys$Rt <- rbind(cbind(sys$Rt, rep(0, m)),
                    c(rep(0, n_var), 1))
    sys$Qt <- rbind(cbind(sys$Qt, rep(0, n_var)),
                    c(rep(0, n_var), NA))
    
    state_names_new <- paste0("trend_", name)
    var_names_new <- paste0("var_trend_", name)
    if (const) {
      const_names_new <- paste0("const_trend_", name)
    }
    
  } else if (type == 2 | type == 4 | type == 3) { 
    # random walk with random drift
    sys$Zt <- cbind(sys$Zt, matrix(0, k, 2))
    sys$Tt <- rbind(cbind(sys$Tt, matrix(0, m, 2)),
                    cbind(matrix(0, 2, m), matrix(c(1, 1, 0, 1), 2, 2, byrow = TRUE)))
    sys$Qt <- rbind(cbind(sys$Qt, rep(0, n_var)),
                    c(rep(0, n_var), NA))
    state_names_new <- c(paste0("trend_", name), paste0("drift_", name))
    var_names_new <- paste0("var_drift_", name)
    
    if (type == 4 | type == 3) {
      sys$Rt <- rbind(cbind(sys$Rt, rep(0, m), rep(0, m)),
                      cbind(matrix(0, 2, n_var),diag(1,2)))
      sys$Qt <- rbind(cbind(sys$Qt, rep(0, n_var+1)),
                      c(rep(0, n_var+1), NA))
      var_names_new <- c(paste0("var_trend_", name), paste0("var_drift_", name))
    } else {
      sys$Rt <- rbind(cbind(sys$Rt, rep(0, m)),
                      cbind(matrix(0, 2, n_var), c(0, 1)))
    } 
    if (type == 3) {
      sys$Tt[NROW(sys$Tt), NCOL(sys$Tt)] <- NA
      sys$Tt[NROW(sys$Tt), "const"] <- NA
      AR_names_new <- paste0("drift_", name,"_AR")
      const_names_new <- paste0("const_drift_", name)
    }
    
  } 
  
  # update column, row and parameter names
  colnames(sys$Zt) <- colnames(sys$Tt) <- rownames(sys$Tt) <- rownames(sys$Rt) <- c(state_names, state_names_new)
  colnames(sys$Qt) <- rownames(sys$Qt) <- colnames(sys$Rt) <- c(var_names, var_names_new)
  sys$names$par <- c(sys$names$par, var_names_new, const_names_new, AR_names_new)
  
  # update stationary and root components
  if (type != 3) {
    sys$names$root <- c(sys$names$root, state_names_new)
  } else {
    sys$names$root <- c(sys$names$root, state_names_new[1])
    sys$names$stationary <- c(sys$names$stationary, state_names_new[2])
    
  }
  
  # update empty observation covariance matrix
  sys$Ht <- matrix(0, nrow(sys$Zt), nrow(sys$Zt))
  colnames(sys$Ht) <- rownames(sys$Ht) <- rownames(sys$Zt)
  
  return(sys)
}

# ------------------------------------------------------------------------------

#' Add a cycle to a state space model
#'
#' @param p integer with autoregressive order, \code{p <= 2}
#' @param lags (optional) number of lags added to state equation, e.g. since 
#'  other equations load on them
#' @inheritParams add_trend
#' 
#' @return The input list \code{sys} with updated matrices.
#'
add_cycle <- function(sys, p, name, lags = NULL) {
  
  k <- nrow(sys$Zt)
  m <- nrow(sys$Tt)
  n_var <- ncol(sys$Rt)
  
  obs_names <- rownames(sys$Zt)
  state_names <- colnames(sys$Tt)
  var_names <- colnames(sys$Qt)
  
  # expand matrices
  if (p == 0) { 
    # white noise
    sys$Zt <- cbind(sys$Zt, rep(0, k))
    sys$Tt <- rbind(cbind(sys$Tt, rep(0, m)),
                    c(rep(0, m), 0))
    sys$Rt <- rbind(cbind(sys$Rt, rep(0, m)),
                    c(rep(0, n_var), 1))
    sys$Qt <- rbind(cbind(sys$Qt, rep(0, n_var)),
                    c(rep(0, n_var), NA))
    
    state_names_new <- paste0("cycle_", name)
    var_names_new <- paste0("var_cycle_", name)
    
    sys$names$par <- c(sys$names$par, paste0("var_cycle_", name))
    
  } else if (p == 1) {
    # AR(1)
    sys$Zt <- cbind(sys$Zt, rep(0, k))
    sys$Tt <- rbind(cbind(sys$Tt, rep(0, m)),
                    c(rep(0, m), NA))
    sys$Rt <- rbind(cbind(sys$Rt, rep(0, m)),
                    c(rep(0, n_var), 1))
    sys$Qt <- rbind(cbind(sys$Qt, rep(0, n_var)),
                    c(rep(0, n_var), NA))
    
    state_names_new <- paste0("cycle_", name)
    var_names_new <- paste0("var_cycle_", name)
    
    sys$names$par <- c(sys$names$par, paste0("cycle_", name, "_AR"), paste0("var_cycle_", name))
    
  } else if (p == 2) {
    # AR(2)
    sys$Zt <- cbind(sys$Zt, matrix(0, k, 2))
    sys$Tt <- rbind(cbind(sys$Tt, matrix(0, m, 2)),
                    cbind(matrix(0, 2, m), matrix(c(NA, NA, 1, 0), 2, 2, byrow = TRUE)))
    sys$Rt <- rbind(cbind(sys$Rt, rep(0, m)),
                    cbind(matrix(0, 2, n_var), c(1, 0)))
    sys$Qt <- rbind(cbind(sys$Qt, rep(0, n_var)),
                    c(rep(0, n_var), NA))
    
    state_names_new <- c(paste0("cycle_", name), paste0("cycle_", name, "_L1"))
    var_names_new <- paste0("var_cycle_", name)
    
    sys$names$par <- c(sys$names$par, paste0("cycle_", name, "_AR"), paste0("cycle_", name, "_AR_L1"), paste0("var_cycle_", name))
  }
  
  # add addtional lags
  if (lags > p - 1 & lags > 0) {
    p_add <- lags - ifelse(p>0, p - 1,0)
    m <- nrow(sys$Tt)
    sys$Zt <- cbind(sys$Zt, matrix(0, k, p_add))
    sys$Tt <- rbind(cbind(sys$Tt, matrix(0, m, p_add)),
                    cbind(matrix(0, p_add, m-1), cbind(diag(1, p_add), rep(0, p_add))))
    sys$Rt <- rbind(sys$Rt, 
                    matrix(0, p_add, ncol(sys$Rt)))
    state_names_new <- c(state_names_new, paste0("cycle_", name, "_L", ifelse(p>0,p,1):lags))
  }
  
  # update column, row and parameter names
  colnames(sys$Zt) <- colnames(sys$Tt) <- rownames(sys$Tt) <- rownames(sys$Rt) <- c(state_names, state_names_new)
  colnames(sys$Qt) <- rownames(sys$Qt) <- colnames(sys$Rt) <- c(var_names, var_names_new)
  
  # update empty observation covariance matrix
  sys$Ht <- matrix(0, nrow(sys$Zt), nrow(sys$Zt))
  colnames(sys$Ht) <- rownames(sys$Ht) <- rownames(sys$Zt)
  
  # update stationary and root components
  sys$names$stationary <- c(sys$names$stationary, state_names_new)
  
  return(sys)
  
}

# ------------------------------------------------------------------------------

#' Add initialization matrices to state space model
#'
#' @inheritParams add_trend
#' 
#' @return The input list \code{sys} with updated matrices.
#'
add_init_mat <- function(sys) {
  
  k <- nrow(sys$Zt)
  m <- nrow(sys$Tt)
  n_var <- ncol(sys$Rt)
  
  obs_names <- rownames(sys$Zt)
  state_names <- colnames(sys$Tt)
  var_names <- colnames(sys$Qt)
  
  # P1:     variance of stationary part should be assigned
  # P1inf:  diffuse parts should be set to 1
  # a1:     diffuse parts should be set to 0
  
  # initialize
  sys$a1 <- matrix(0, m, 1)
  sys$P1 <- sys$P1inf <- diag(0, m)
  
  # name matrices
  colnames(sys$P1) <- rownames(sys$P1) <- colnames(sys$P1inf) <- rownames(sys$P1inf) <- rownames(sys$a1) <- state_names
  
  indexStat <- sys$names$stationary
  indexRoot <- sys$names$root
  
  # assign
  sys$a1[grepl("const", rownames(sys$a1)), ] <- 1
  diag(sys$P1[indexStat, indexStat]) <- NA
  sys$P1[grepl("const", rownames(sys$a1)), grepl("const", rownames(sys$a1))] <- 0
  if (k > 1) {
    diag(sys$P1inf[indexRoot, indexRoot]) <- 1
  } else {
    sys$P1inf[indexRoot, indexRoot] <- 1
  }

  return(sys)
  
}

# ------------------------------------------------------------------------------

#' Add lags to state equation
#'
#' @inheritParams add_trend
#' @inheritParams add_cycle
#' 
#' @return The input list \code{sys} with updated matrices.
#'
add_lag <- function(sys, name, type, lags = NULL) {
  
  k <- nrow(sys$Zt)
  m <- nrow(sys$Tt)
  n_var <- ncol(sys$Rt)
  
  obs_names <- rownames(sys$Zt)
  state_names <- colnames(sys$Tt)
  var_names <- colnames(sys$Qt)
  
  p_add <- length(lags)
  m <- nrow(sys$Tt)
  sys$Zt <- cbind(sys$Zt, matrix(0, k, p_add))
  sys$Tt <- rbind(cbind(sys$Tt, matrix(0, m, p_add)),
                  cbind(matrix(0, p_add, m-1), cbind(diag(0, p_add), rep(0, p_add))))
  sys$Rt <- rbind(sys$Rt, 
                  matrix(0, p_add, ncol(sys$Rt)))
  state_names_new <- paste0(type, "_", name, "_L", lags)
  
  # update column, row and parameter names
  colnames(sys$Zt) <- colnames(sys$Tt) <- rownames(sys$Tt) <- rownames(sys$Rt) <- c(state_names, state_names_new)

  # update empty observation covariance matrix
  sys$Ht <- matrix(0, nrow(sys$Zt), nrow(sys$Zt))
  colnames(sys$Ht) <- rownames(sys$Ht) <- rownames(sys$Zt)
  
  # update stationary and root components
  sys$names$root <- c(sys$names$root, state_names_new)
  
  sys$Tt[state_names_new, gsub("_L0", "", paste0(type, "_", name, "_L", lags - 1))] <- diag(1, p_add)
  
  return(sys)
  
}

