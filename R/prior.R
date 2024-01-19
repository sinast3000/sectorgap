
#' Prior distribution
#' 
#' @description Initializes the prior distributions.
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
#' @details The normal distribution is parametrized via mean and variance.
#' 
#' @details the inverse gamma distribution is parametrized degrees of freedom 
#' \code{nu} and scale \code{s}.
#'  
#' @details The mean of the inverse gamma distribution is given by 
#'  \code{beta / (alpha - 1) = beta / 2 = s}, where 
#'  \code{s = 2 beta, nu = 2 alpha}.
#'  
#' @return A data frame with one row per parameter and the following columns:
#'  \item{variable}{name of endogneous variable of equation}
#'  \item{parameter_name}{name of parameter}
#'  \item{par1}{first parameter of specified distribution, mean for normal 
#'    parameters and scale for inverse gamma parameters}
#'  \item{par2}{second parameter of specified distribution, variance for normal 
#'    parameters and degrees of freedom for inverse gamma parameters}
#'  \item{ini}{initial value for Gibbs sampler, i.e. mean of distribution given 
#'    \code{par1} and \code{par2}}
#'  \item{distribution}{name of prior distribution}
#'
#' @importFrom dplyr %>% mutate arrange
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
initialize_prior <- function(model, settings, lambda_d = 100, lambda_t = 100, df = 6) {
  
  # save call
  mc <- match.call(expand.dots = FALSE)
  
  # to avoid RMD check note
  . <- variable <- parameter_name <- par1 <- par2 <- ini <- type <- NULL
  
  # parameter settings
  df_set <- settings_to_df(x = settings)

  # inverse gamma parameters
  Esigma_c <- 1
  Esigma_t <- 1 / lambda_t
  Esigma_d <- 1 / lambda_d
  alpha <- df / 2
  beta_c <- Esigma_c * (alpha - 1) 
  beta_t <- Esigma_t * (alpha - 1) 
  beta_d <- Esigma_d * (alpha - 1) 
  
  # data frame with prior inverse gamma
  prior_c <- df_set$variance %>% 
    filter(type == "cycle") %>% 
    mutate(par1 = beta_c * 2, par2 = alpha * 2, ini = Esigma_c)
  prior_t <- df_set$variance %>% 
    filter(type == "trend") %>% 
    mutate(par1 = beta_t * 2, par2 = alpha * 2, ini = Esigma_t)
  prior_d <- df_set$variance %>% 
    filter(type == "drift") %>% 
    mutate(par1 = beta_d * 2, par2 = alpha * 2, ini = Esigma_d)
  prior_ig <- prior_c %>%
    rbind(., prior_t) %>%
    rbind(., prior_d) %>%
    select(variable, parameter_name, par1, par2, ini) %>%
    mutate(distribution = "inverse-gamma")
  
  # data frame with prior normals
  prior_load <- df_set$loadings %>% 
    mutate(par1 = 0, par2 = 1000, ini = 0) %>%
    select(variable, parameter_name, par1, par2, ini)
  prior_AR <- df_set$AR %>% 
    mutate(par1 = 0, par2 = 1000, ini = 0) %>%
    select(variable, parameter_name, par1, par2, ini)
  prior_const <- df_set$const %>% 
    mutate(par1 = 0, par2 = 1000, ini = 0) %>%
    select(variable, parameter_name, par1, par2, ini)
  prior_normal <- prior_load %>%
    rbind(., prior_AR) %>%
    rbind(., prior_const) %>%
    mutate(distribution = "normal")
  
  # combine and change order
  prior <- prior_ig %>%
    rbind(., prior_normal) %>% 
    arrange(factor(variable, levels = df_set$obs$variable), parameter_name)
 
  
  class(prior) <- c("prior", class(prior))
  attr(prior, "call") <- mc
  
  return(prior)
  
}
