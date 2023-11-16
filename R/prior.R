
#' Initializes the prior distributions
#'
#' @param lambda_t trend smoothing constant (default: 100)
#' @param lambda_d drift smoothing constant (default: 100)
#' @param df degrees of freedom for inverse gamma distributions
#' @inheritParams estimate_ssmodel
#' @inheritParams define_ssmodel
#' 
#' @details All loadings and autoregressive parameters are assumed to be normal
#'  with mean zero and variance 1000.
#'  
#' @details All variance parameters are assumed to be inverse gamma distributed.
#'  The cycle variance has prior mean 1, and the trend variances have prior mean
#'  1/100.
#'  
#' @details The mean of the inverse gamma distribution is given by 
#'  \code{beta / (alpha - 1) = beta / 2 = s}, where 
#'  \code{s = 2 beta, nu = 2 alpha}
#'  
#' @return A list of matrices, each list item corresponds to one endogenous 
#'  variable. The matrix columns correspond to each parameter in the respective 
#'  equation. The first two rows in each matrix define the distribution, the 
#'  third and fourth parameter restrictions (not used), and the fifth row 
#'  contains the mean which can e.g. be used for initialization.
#'
initialize_prior <- function(model, settings, lambda_d = 100, lambda_t = 100, df = 6) {
  
  # vector with names of parameters
  par_names <- model$names$par

  # initialize
  prior <- list()
  row_names <- c("par1", "par2", "LB", "UB", "ini")

  # names of endo variables  
  df_set <- settings_to_df(x = settings)
  endo <- df_set$obs$variable

  # inverse gamma parameters
  Esigma_c <- 1
  Esigma_t <- 1 / lambda_t
  Esigma_d <- 1 / lambda_d
  alpha <- df / 2
  beta_c <- Esigma_c * (alpha - 1) 
  beta_t <- Esigma_t * (alpha - 1) 
  beta_d <- Esigma_d * (alpha - 1) 
  
  # assign list item with prior matrix for each endo variable
  for (name in endo) {
    
    # parameter names
    pars <- par_names[
      grepl(name, par_names) &
      !grepl(paste0(name, "_L"), par_names)
    ]
    
    # initialize matrix
    mat <- matrix(0, 5, length(pars), dimnames = list(row_names, pars))
    mat[3, ] <- -Inf
    mat[4, ] <- Inf
    
    # get index for parameters
    idx_var_t <- grepl("var_t", pars)
    idx_var_d <- grepl("var_d", pars)
    idx_var_c <- grepl("var_c", pars)
    idx_AR <- grepl("_AR", pars)
    idx_load <- grepl("_load_", pars)
    idx_const <- grepl("const_", pars)
    
    # assign distributions
    mat[c(1, 2, 5), idx_var_t] <- c(beta_t * 2, alpha * 2, Esigma_t)
    mat[c(1, 2, 5), idx_var_d] <- c(beta_d * 2, alpha * 2, Esigma_d)
    mat[c(1, 2, 5), idx_var_c] <- c(beta_c * 2, alpha * 2, Esigma_c)
    mat[c(1, 2, 5), idx_AR] <- c(0, 1000, 0)
    mat[c(1, 2, 5), idx_load] <- c(0, 1000, 0)
    mat[c(1, 2, 5), idx_const] <- c(0, 1000, 0)
    
    prior[[name]] <- mat
      
  }
  
  prior
  
}
