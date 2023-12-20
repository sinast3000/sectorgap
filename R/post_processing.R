
# ------------------------------------------------------------------------------

#' Format results
#' 
#' @description Formats the output series into a tibble in long format and 
#' computes contribution series.
#'
#' @param fit fitted object
#' @param estimate character specifying the posterior estimate. Valid options 
#'   are \code{"mean"} and \code{"median"}, the default is 
#'   \code{estimate = "median"}.
#' @param transformed boolean indicating if the transformed series should be 
#'   used.
#' @param data list with at least two named components: \code{prices} is a 
#'   multiple time series object that contains price indices for all relevant 
#'   series, \code{weights}, is a named list of time series with (nominal) 
#'   weights, the list names  correspond to the different groups, i.e., 
#'   \code{group1, group2, subgroup1}, if present in the model
#' @inheritParams define_ssmodel
#' @inheritParams estimate_ssmodel
#' 
#' @details \code{data} is preferably the output of funtion \code{prepare_data}.
#' 
#' @return A data frame with results in long format.
#' 
#' @importFrom dplyr %>% left_join full_join
#' @importFrom tidyr pivot_longer
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
#' df <- transform_results(
#'   fit = fit, 
#'   data = data,
#'   estimate = "median"
#' )
#' }
transform_results <- function(
  fit, 
  data,
  settings, 
  estimate = "median", 
  HPDIprob = 0.68, 
  transformed = TRUE
) {
  
  # to avoid RMD check note
  . <- variable <- group <- group_label <- variable_label <- obs_name <- NULL
  
  HPDI <- HPDIprob * 100  
  
  # data
  tsl_w <- data$weights_growth
  tsm_p <- data$prices
  
  # settings to data frames
  settings <- fit$settings
  df_set <- settings_to_df(x = settings)
  
  # chose mean or median estimates
  state <- fit$tsl[[paste0("state", ifelse(transformed, "_trans_", "_"),  tolower(estimate))]]
  state_summary <- fit$tsl[[paste0("state", ifelse(transformed, "_trans_", "_"), "summary")]]
  state_ntrans <- fit$tsl[[paste0("state_", tolower(estimate))]]
  state_trans <- fit$tsl[[paste0("state_trans_", tolower(estimate))]]
  
  df <- state %>%
    data.frame("date" = time(.), .) %>%
    pivot_longer(-date, names_to = "series", values_to = "value") 
  df$type <- gsub("\\_.*", "", df$series)
  df$obs_name <- gsub(".*\\_", "", df$series)
  
  # add observations level and yoy rates
  if (NROW(df_set$obs) > 1) {
  df_obs <- 1:NROW(df_set$obs) %>%
      lapply(., function(x) {
        trans <- df_set$obs$transform[x]
        vx <- df_set$obs$variable[x]
        if (trans) {
          settings$fun_transform_inv(fit$model$y[, vx])
        } else {
          fit$model$y[, vx]
        }
    }) %>%
    do.call(cbind, .)
    colnames(df_obs) <- df_set$obs$variable
  } else {
    trans <- df_set$obs$transform[1]
    if (trans) {
      df_obs <- settings$fun_transform_inv(fit$model$y[, 1])
    } else {
      df_obs <- fit$model$y[, 1]
    }
  }
  
  df_obs_yoy <- df_obs %>%
    pct(.) %>%
    data.frame("date" = time(.), .) %>%
    pivot_longer(-date, names_to = "obs_name", values_to = "obs") 
  df_obs_yoy$type <- "drift"
  if (NROW(df_set$obs) == 1) df_obs_yoy$obs_name <- df_set$obs$variable
  df_obs_yoy$date <- as.numeric(df_obs_yoy$date)
  
  df_obs <- df_obs %>%
    data.frame("date" = time(.), .) %>%
    pivot_longer(-date, names_to = "obs_name", values_to = "obs") 
  df_obs$type <- "trend"
  if (NROW(df_set$obs) == 1) df_obs$obs_name <- df_set$obs$variable
  df_obs$date <- as.numeric(df_obs$date)
  df_obs <- rbind(df_obs, df_obs_yoy)
  
  df <- left_join(df, df_obs, by = c("date", "obs_name", "type"))
  df
  
  # add bounds
  vars_summary <- colnames(state)
  df_LB <- lapply(vars_summary, function(x) {
    state_summary[, paste0(x, ".", HPDI, "% HPDI-LB")]
  }) %>%
    do.call(cbind, .) 
  colnames(df_LB) <- gsub("_summary", "", vars_summary)
  df_LB <- df_LB %>%
    data.frame("date" = time(.), .) %>%
    pivot_longer(-date, names_to = "series", values_to = "lb") 
  df_UB <- lapply(vars_summary, function(x) {
    state_summary[, paste0(x, ".", HPDI, "% HPDI-UB")]
  }) %>%
    do.call(cbind, .) 
  colnames(df_UB) <- gsub("_summary", "", vars_summary)
  df_UB <- df_UB %>%
    data.frame("date" = time(.), .) %>%
    pivot_longer(-date, names_to = "series", values_to = "ub") 
  
  df <- full_join(df, df_LB, by = c("date", "series") ) 
  df <- full_join(df, df_UB, by = c("date", "series") ) 

  # gap, drift, trend contributions
  groups <- c("group1", "group2", "subgroup1")
  df_contr <- NULL
  for (ig in groups) {
    sig <- settings[[ig]]
    if (length(sig$variable) > 0 ) {
      
      series <- c(sig$load_name, sig$variable)
      
      # gap
      if (all(series %in% colnames(tsm_p))) {
        tsm_p_ig <- tsm_p[, series]
      } else {
        tsm_p_ig <- ts(
          matrix(100, nrow(tsm_p), length(series), dimnames = list(NULL, series)),
          start = start(tsm_p), frequency = frequency(tsm_p)
        )
      }
      tsm_gap_contr <- aggregate_gap(
        tsl_p = as.list(tsm_p_ig), 
        tsl_t = as.list(state_trans[, paste0("trend_", series)]), 
        tsl_g = as.list(state_trans[, paste0("gap_", series)]), 
        idx = 1,      
        error_name = paste0("gap_error", ig),
        idx_neg = which(sig$variable_neg == series) - 1
      )
      # drifts
      tsm_weights <- tsl_w[[ig]]
      tsm_drift_contr <- tsm_weights[, series[-1]] * state_ntrans[, paste0("drift_", series[-1])]
      tsm_drift_contr <- cbind(
        state_ntrans[, paste0("drift_", series[1])] - Reduce("+", as.list(tsm_drift_contr)),
        tsm_drift_contr
      )
      colnames(tsm_drift_contr) <- paste0("drift_", c(paste0("error", ig), series[-1]))
      # trend
      tsm_trend_contr <- tsm_weights[, series[-1]] * diff(state_ntrans[, paste0("trend_", series[-1] )])
      tsm_trend_contr <- cbind(
        diff(state_ntrans[, paste0("trend_", series[1])]) - Reduce("+", as.list(tsm_trend_contr)),
        tsm_trend_contr
      )
      colnames(tsm_trend_contr) <- paste0("trend_", c(paste0("error", ig), series[-1]))
      
      tsm_contr <- cbind(tsm_gap_contr, tsm_drift_contr, tsm_trend_contr)
      colnames(tsm_contr) <- gsub(".*\\.", "", colnames(tsm_contr))
      df_contr_ig <- tsm_contr %>%
        data.frame("date" = time(.), .) %>%
        pivot_longer(-date, names_to = "series", values_to = "contr") 
      df_contr <- rbind(df_contr_ig, df_contr)
      
    }
  }
  
  if (!is.null(df_contr)) {
    df <- full_join(df, df_contr, by = c("date", "series"))
  }
  
  # add display names
  df$type <- gsub("\\_.*", "", df$series)
  df$obs_name <- gsub(".*\\_", "", df$series)

  # add group
  df <- df_set$obs %>% 
    select(variable, group, group_label, variable_label) %>%
    rename(obs_name = variable, series_label = variable_label) %>%
    left_join(df, ., by = "obs_name")
  
  df <- df %>% filter(!(obs_name %in% df_set$obs$variable)) %>%  
    mutate(series_label = "Residual") %>%
    rbind(df %>% filter(obs_name %in% df_set$obs$variable), .) %>%
    as.data.frame
  
  return(df)
  
}


# ------------------------------------------------------------------------------

#' Output gap contributions
#' 
#' @description Computes chain aggregated output gap contributions.
#'
#' @param tsl_p time series list with prices
#' @param tsl_t time series list with trends
#' @param tsl_g time series list with gaps
#' @param idx index of aggregate
#' @param idx_neg index of negative sub sectors
#' @param error_name character string with name for aggregation error
#' @param previous_year boolean indicating if previous year prices should be 
#'   used instead of pervious period prices
#' 
#' @return A multiple time series object containing the contributions.
#' 
#' @importFrom tempdisagg ta td
#' 
#' @keywords internal
aggregate_gap <- function(
  tsl_p, 
  tsl_t, 
  tsl_g, 
  idx,
  error_name = "gap_error",
  idx_neg = NULL,
  previous_year = TRUE
) {
  
  # previous year/period prices
  if (previous_year) {
    tsl_p <- lapply(tsl_p, function(x) {
      y <- stats::lag(ta(x, to = "annual"), -1)
      td(y ~ -1, method = "uniform")$values
    })
  } else {
    tsl_p <- lapply(tsl_p, stats::lag, -1)
  }
  
  # compute weights for prices and trends
  w_p <- do.call(cbind, tsl_p[-idx]) / tsl_p[[idx]]
  w_t <- do.call(cbind, tsl_t[-idx]) / tsl_t[[idx]]  
  gaps <- do.call(cbind, tsl_g[-idx])
  
  # compute constributions
  gap_contr <- w_p * w_t * gaps
  if (!is.null(idx_neg)) gap_contr[, idx_neg] <- -gap_contr[, idx_neg]
  
  # compute error
  error <- Reduce("+", as.list(w_p * w_t)) - 1
  error <- tsl_g[[idx]] -  Reduce("+", as.list(gap_contr))
  
  # contributions
  gap_contr <- cbind(gap_contr, error)
  colnames(gap_contr) <- c(colnames(gaps), error_name)
  
  return(gap_contr)
  
}
