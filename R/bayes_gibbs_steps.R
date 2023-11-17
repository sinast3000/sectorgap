# ------------------------------------------------------------------------------

#' Draws the parameters of the output gap..
#' 
#' This is a wrapper function that draws the parameters of the output gap for 
#' different cycle specifications
#'
#' @inheritParams .postRegression
#' 
#' @return A named vector of drawn parameters.
draw_output_gap <- function(
  Y, 
  phi, 
  phiDistr, 
  sigma, 
  sigmaDistr
) {
  
  if (length(phi)>0) {
    # AR cycle
    x <-.postARp(
      Y = Y,
      phi = phi,
      phiDistr = phiDistr,
      sigma =  sigma,
      sigmaDistr = sigmaDistr
    )
  } else {
    # white noise cycle
    x <- draw_variance_scalar(
      Y = Y, 
      nu = sigmaDistr[2], 
      s = sigmaDistr[1]
    )
    names(x) <- names(sigma)
  }
  return(x)
  
}

# ------------------------------------------------------------------------------

#' Draws (correlated) trend variances.
#'
#' @param model state space model object
#' @param state time series matrix with states
#' @param df_var data frame with trend and drift variance settings
#' @param df_cov data frame with trend and drift covariance settings
#' @param distrPar prior distribution matrix
#' 
#' @return A named vector of drawn parameters.
#' 
#' @importFrom stats rnorm
#' @keywords internal
draw_trend_innovations <- function(
  model,
  state,
  df_var,
  df_cov, 
  distrPar
) {
  
  # compute innovations
  eps <- state - ts(
    t(model$T[,,1] %*% t(stats::lag(state, k = -1))),
    start = start(state) + c(0, 1), 
    frequency = frequency(state)
  )
  eps <- eps[, paste0("state.", df_var$state)]
  
  # initialize    
  n_t <- ncol(eps)
  A <- matrix(0, n_t, n_t)
  nu <- n_t + ceiling(mean(distrPar[2, df_var$parameter_name])) + 1
  diag(A) <- distrPar[1, df_var$parameter_name] / (distrPar[2, df_var$parameter_name]-2) * (nu - n_t - 1)
  # draw
  tmp <- draw_variance_multi(
    Y = eps,
    nu = nu, 
    Phi = A
  )
  
  # variances
  draw <- c()
  draw[df_var$parameter_name] <- diag(tmp)
  # covariances
  colnames(tmp) <- rownames(tmp) <- df_var$variable
  covar_pars <- sapply(seq_len(NROW(df_cov)), function(x) { 
    tmp[df_cov$variable[x], df_cov$variable2[x]]
  }) %>% unlist
  draw[df_cov$parameter_name] <- covar_pars
  
  return(draw)
  
}


# ------------------------------------------------------------------------------

#' Draws the parameters in a regression equation with AR errors, if specified.
#' 
#' @param Y dependent variable
#' @param X explanatory variable(s)
#' @param beta coefficient vector
#' @param betaDistr prior distribution of coefficient vector
#' @param sigma innovation variance
#' @param sigmaDistr prior distribution of innovation variance
#' @param phi autoregressive coefficient vector
#' @param phiDistr prior distribution of autoregressive coefficient vector
#' @param indep logical, should beta and sigma be independent
#' @param const constant
#' @param constDistr prior distribution of constant
#'  
#' @details See "Chib, Siddhartha. "Bayes regression with autoregressive 
#'  errors: A Gibbs sampling approach." Journal of econometrics 58.3 (1993): 
#'  275-294."
#' 
#' @return A named vector of drawn parameters.
#' 
#' @importFrom zoo na.trim
#' @importFrom stats rgamma
#' 
#' @keywords internal
.postRegression <- function(
  Y, 
  X, 
  beta = NULL, 
  betaDistr, 
  sigma, 
  sigmaDistr, 
  phi = NULL, 
  phiDistr = NULL,
  indep = TRUE,
  const = NULL, 
  constDistr = NULL
) {

  # last draws
  betaLast <- beta
  sigmaLast <- sigma
  phiLast <- phi
  
  # align data
  data <- na.trim(cbind(Y,X))
  Y <- data[,1]
  X <- data[,-1]
  
  # dimensions
  p <- length(phi)
  k <- NCOL(X)
  n <- length(Y)
  Y <- matrix(Y, ncol = 1)
  X <- matrix(X, ncol = k)
  
  # 1) phi
  if (p > 0) {
    eps <- as.matrix(Y - X %*% betaLast, ncol = 1)
    
    phir <- .postARp_phi(
      Y = eps, 
      phi = phi, 
      phiDistr = phiDistr, 
      sigma = sigmaLast,
      const = const, 
      constDistr = constDistr
    )
    
  } else {
    phir <- NULL
  }
  
  # 2) beta
  Ystar <- Y[(p+1):n, , drop = FALSE]
  Xstar <- X[(p+1):n, , drop = FALSE]
  if (p > 0) {
    for (pp in 1:p) {
      Ystar <- Ystar - phir[pp] * Y[(p-pp+1):(n-pp), ]
      Xstar <- Xstar - phir[pp] * X[(p-pp+1):(n-pp), ]
    }
  }
  beta0 <- betaDistr[1, ]
  A0 <- solve(diag(betaDistr[2, ], nrow = length(beta)))
  
  if (!indep) {
    # dependent prior
    Astar <- A0 + t(Xstar) %*% Xstar
    betastar <- solve(Astar) %*% (A0 %*% beta0 + t(Xstar) %*% Ystar)
    betar <- .mvrnorm(mu = betastar, sigma = sigmaLast * solve(Astar))
  } else {
    # independent loading and variance prior
    Astar <- A0 + 1/sigmaLast * t(Xstar) %*% Xstar
    betastar <- solve(Astar) %*% (A0 %*% beta0 + 1/sigmaLast * t(Xstar) %*% Ystar)
    betar <- .mvrnorm(mu = betastar, sigma = solve(Astar))
  }
  
  # 3) sigma
  s0 <- sigmaDistr[1, ]
  nu0 <- sigmaDistr[2, ]
  if (!indep) {
    Qbeta <- t(betar - beta0) %*% A0 %*% (betar - beta0) 
  } else {
    Qbeta <- 0
    k <- 0
  }
  dbeta <- t(Ystar - Xstar %*% betar) %*% (Ystar - Xstar %*% betar)
  nustar <- n - p + nu0 + k
  sstar <- s0 + Qbeta + dbeta
  sigmar <- 1 / rgamma(1, shape = nustar / 2, rate = sstar / 2)
  
  # combine
  pars <- c(betar, sigmar, phir)
  names(pars) <- c(names(beta), names(sigma), names(phi))
  return(pars)
  
}

# ------------------------------------------------------------------------------

#' Draws the parameters of an AR process (AR parameters and variance).
#' .
#' @inheritParams .postRegression
#' 
#' @return A named vector of drawn parameters.
#' 
#' @details See "Chib, Siddhartha. "Bayes regression with autoregressive 
#'  errors: A Gibbs sampling approach." Journal of econometrics 58.3 (1993): 
#'  275-294."
#'  
#' @importFrom stats rgamma
.postARp <- function(Y, phi, phiDistr, sigma, sigmaDistr, const = NULL, constDistr = NULL) {

  # last draw
  sigmaLast <- sigma
  phiLast <- phi
  
  # dimensions
  p <- length(phi)
  n <- length(Y)
  Y <- matrix(Y, ncol = 1)
  
  # 1) phi
  eps <- Y
  phir <- .postARp_phi(
    Y = eps, 
    phi = phi, 
    phiDistr = phiDistr, 
    sigma = sigmaLast,
    const = const, 
    constDistr = constDistr
  )
  
  # create matrix with lags
  Ystar <- Y[(p+1):n, , drop = FALSE]
  if (p > 0) {
    for (pp in 1:p) {
      Ystar <- Ystar - phir[pp] * Y[(p-pp+1):(n-pp), ]
    }
  }
  
  # 2) sigma
  s0 <- sigmaDistr[1, ]
  nu0 <- sigmaDistr[2, ]
  
  dbeta <- t(Ystar) %*% (Ystar)
  nustar <- n - p + nu0
  sstar <- s0 + dbeta
  sigmar <- 1 / rgamma(1, shape = nustar / 2, rate = sstar / 2)
  
  # return
  pars <- c(sigmar, phir)
  names(pars) <- c(names(sigma), names(phi))
  
  return(pars)
}

# ------------------------------------------------------------------------------

#' Draws the autoregressive parameters of an AR process (AR parameters only).
#' .
#' @inheritParams .postRegression
#' 
#' @return A named vector of drawn parameters.
#' 
#' @details See "Chib, Siddhartha. "Bayes regression with autoregressive 
#'  errors: A Gibbs sampling approach." Journal of econometrics 58.3 (1993): 
#'  275-294."
.postARp_phi<- function(Y, phi, phiDistr, sigma, const = NULL, constDistr = NULL) {

  # last draw
  phiLast <- c(const, phi)
  phiDistr <- cbind(constDistr, phiDistr)
  
  # dimensions
  n <- length(Y)
  p <- length(phi)
  Y <- matrix(Y, ncol = 1)
  nc <- length(const)
  
  # create matrix with lags
  X <- matrix(NA, ncol = p, nrow = n - p)
  for (pp in 1:p) {
    X[, pp] <- Y[(p-pp+1):(n-pp), ]
  }
  if (!is.null(const)) X <- cbind(rep(1, NROW(X)), X)
  Y <- Y[(p+1):n, , drop = FALSE]
  
  # hyper parameters
  phi0 <- phiDistr[1, ]
  PHI0 <-  solve(diag(phiDistr[2, ], nrow = NCOL(phiDistr)))
  PHIstar <- PHI0 + 1/sigma * t(X) %*% X
  phistar <- solve(PHIstar) %*% (PHI0 %*% phi0 + 1 / sigma * t(X) %*% Y)
  
  alpha_ic <- 0
  count <- 0
  while (alpha_ic == 0) {
    phir <- .mvrnorm(mu = phistar, sigma = solve(PHIstar))
    # check stationarity
    alpha_ic <- all(abs(polyroot(z = c(1, -phir[(nc+1):(nc+p)]))) > 1)
    count <- count + 1
    if (count == 100) {
      alpha_ic <- 1
      phir <- phiLast
      warning("No stationary AR draw found, reusing last draw.", call. = FALSE)
    }
  }
  names(phir) <- c(names(const), names(phi))

  return(c(phir))
  
}

# ------------------------------------------------------------------------------

#' Draws a variance from an inverse Wishart distribution.
#' 
#' @inheritParams .postRegression
#' @param nu degrees of freedom, \code{nu>p-1}
#' @param s scale matrix, \code{p x p}
#' 
#' @details The mean is given by \code{s / (nu+p-1)} for \code{nu>p+1}.
#' 
#' @return A named vector of drawn parameters.
#' 
#' @importFrom MCMCpack riwish
draw_variance_scalar <- function(Y, nu, s) {
  
  # dimensions
  n <- NROW(Y)
  k <- NCOL(Y)
  
  # hyper parameters
  nustar <- nu + n
  sstar <- s + sum(Y^2)
  
  # draw
  sigmar <- c(riwish(v = nustar, S = sstar))
  
  return(sigmar)
  
}

# ------------------------------------------------------------------------------

#' Draws from the multivariate normal distribution.
#'
#' @param mu A \code{n x 1} vector, the mean vector.
#' @param sigma A \code{n x n} matrix, the covariance matrix.
#'
#' @return A \code{n x 1} named vector of drawn parameters.
#'
#' @importFrom stats rnorm
#' @keywords internal
.mvrnorm <- function(mu, sigma) {
  # tolerance regarding positive-definiteness of the covariance matrix
  tol <- 1e-08
  # check dimension
  n <- length(drop(mu))
  if (!all(dim(sigma) == c(n, n))) {
    stop("mean vector and covariance matrix not compatible")
  }
  # check covariance for positive definiteness
  eig <- eigen(sigma, symmetric = TRUE)
  if (!all(eig$values >= -tol * abs(eig$values[1L]))) {
    stop("The covariance matrix is not positive definite")
  }
  # cholesky
  L <- chol(sigma)
  # draw
  Y <- drop(mu) + t(L) %*% rnorm(n)
  # names
  name <- names(mu)
  dimnames(Y) <- list(name, NULL)
  # return
  return(Y)
}

# ------------------------------------------------------------------------------

#' Draws a variance from an inverse Wishart distribution.
#' .
#' @inheritParams .postRegression
#' @param nu degrees of freedom, \code{nu>p-1}
#' @param Phi scale matrix, \code{p x p}
#' 
#' @return A named vector of drawn parameters.
#' 
#' @details The mean is given by \code{Phi / (nu+p-1)} for \code{nu>p+1}.
draw_variance_multi <- function(
  Y, 
  nu, 
  Phi
) {
  
  # dimensions
  n <- NROW(Y)
  k <- NCOL(Y)
  
  # hyper parameters
  nustar <- nu + n
  Phistar <- Phi + apply(array(apply(as.matrix(Y), 1, function(x) {
    x %*% t(x)
  }), c(k, k, n)), c(1, 2) ,sum)

  # draw
  sigmar <- riwish(v = nustar, S = Phistar)
  
  return(sigmar)
  
}

