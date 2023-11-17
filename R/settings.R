

# corr = 1: trends ( only if trend exists (if trend!=2))
# corr = 2: drifts
# corr = 4: trends and drifts
# corr = NA or corr = 0: no correlation

# ------------------------------------------------------------------------------

#' Settings
#' 
#' This function initializes settings with a basic example.
#'
#' @param FUN_transform transformation function
#' @param FUN_transform_inv inverse transformation function
#' 
#' @return A nested list with settings for different groups, namely
#'   \item{agg}{List with settings for the aggregate variable.}
#'   \item{group1}{List with settings for the first subgroup, all variables 
#'         in this group load on the aggregate variable.}
#'   \item{group2}{List with settings for the second subgroup, all variables 
#'         this group load on the aggregate variable.}
#'   \item{subgroup1}{List with settings for the corresponding group to 
#'         \code{group1}, all variables in this group load on the respective 
#'         counterpart in \code{group1}.}
#'   \item{agggroup}{List with settings for a group of aggregate variables, all 
#'         variables in this group load on the aggregate variable.}
#'   \item{misc}{Nested list with settings for miscellaneous variables that do 
#'         not fit into any other list, settings for these variables are set 
#'         individually and are more flexible.}
#'         
initialize_settings <- function(
  FUN_transform = function(x) 100 * log(x),
  FUN_transform_inv = function(x) exp(x / 100)
) {
  
  set <- list()
  
  # aggregate variable
  set$agg <- list(
    cycle = 2, 
    trend = 3,
    variable = "output",
    transform = TRUE,
    label = "Aggregate output",
    variable_label = "GDP"
    
  )
  
  # group1
  set$group1 <- list(
    trend = 4,
    cycle = 2,
    corr = 0,
    agg_load = 0,
    constr_drift = TRUE,
    constr_trend = TRUE,
    variable = c(
      "vaA",
      "vaB",
      "vaC"
    ),
    prefix = "va",
    variable_neg = NULL,
    transform = TRUE,
    name_agg = set$agg$variable,
    name_residual = NULL,
    label = "Output",
    variable_label = c(
      "A: Goods-producing industries",
      "B: Service industries",
      "C: Government and adjustments"
    )
  )
  
  # employment
  set$subgroup1 <- list(
    trend = 4,
    cycle = 2,
    corr = 0,
    agg_load = 0:2,
    constr_trend = TRUE,
    constr_drift = TRUE,
    variable = c(
      "fteA",
      "fteB"
    ),    
    prefix = "fte",
    transform = TRUE,
    name_agg = "employment",
    name_residual = "resempl",
    label = "Employment",
    variable_label = c(
      "A: Goods-producing industries",
      "B: Service industries",
      "Residual"
    )
  )
  
  # group2
  set$group2 <- list(
    trend = 4,
    cycle = 2,
    corr = 0,
    agg_load = 0,
    constr_trend = TRUE,
    constr_drift = TRUE,
    variable = c(
      "exp1",
      "exp2",
      "exp3",
      "exp4"
    ), 
    variable_neg = "exp4",
    name_agg = set$agg$variable,
    name_residual = "resexp",
    transform = TRUE,
    label = "Expenditure",
    variable_label = c(
      "i: Total consumption",
      "ii: Investment",
      "iii: Exports",
      "iv: Imports",
      "Residual"
    )
  )
  
  # aggroup
  set$agggroup <- list(
    trend = 4,
    cycle = 2,
    corr = 0,
    agg_load = 0:2,
    variable = c(
      "employment",
      "urate"
    ),
    transform = c(TRUE, FALSE),
    label = "Aggregates",
    variable_label = c(
      "FTE employment",
      "Unemployment rate"
    )
  )
  
  # misc
  set$misc <- list(
    label = "Miscellaneous",
    inflation = list(
      trend = 1,
      cycle = 2,
      variable_label = "CPI inflation",
      loadings = list(
        list(
          variable = set$agg$variable,
          type = "cycle",
          lag = 0:2
        )
      ),
      transform = FALSE
    )
  )

  # transformation functions
  set$fun_transform <- FUN_transform
  set$fun_transform_inv <- FUN_transform_inv
  
  return(set)
  
}

# ------------------------------------------------------------------------------

#' Create data frame with all model settings
#' 
#' @param x list with model setting (on trend, cycle, etc)
#' 
#' @importFrom dplyr mutate select group_by transmute ungroup summarize filter
settings_to_df <- function(x) {
  
  # to avoid RMD check note
  type <- loads_on <- . <- variable <- variable_label <- group <- group_label <-
    parameter_name <- lag_direct <- lag_indirect <- max_lag <- max_lag_extra <-
    trend <- corr <- variable2 <- type2 <- cycle <- state <- state_lag <- 
    max_lag_AR <- value <- NULL
  
  # ----- observations
  count <- 0
  dfl <- list()
  for (jx in c("agg", "group1", "subgroup1", "group2", "agggroup")) {
    lx <- x[[jx]]
    count_ix <- 0
    for (ix in lx$variable)  {
      count <- count + 1
      count_ix <- count_ix + 1
      dfl[[count]] <- data.frame(
        variable = ix,
        variable_label = lx$variable_label[count_ix],
        variable_neg = ifelse(ix %in% lx$variable_neg, TRUE, FALSE),
        variable_pos = ifelse(ix %in% lx$variable_neg, FALSE, TRUE),
        trend = lx$trend,
        cycle = lx$cycle,
        corr = ifelse(is.null(lx$corr), NA, lx$corr),
        constr_drift = ifelse(is.null(lx$constr_drift), NA, lx$constr_drift),
        constr_cycle = ifelse(is.null(lx$constr_cycle), NA, lx$constr_cycle),
        group = jx,
        group_label = lx$label,
        transform = ifelse(length(lx$transform)==1, lx$transform, lx$transform[count_ix])
      )
    }
  }
  for (jx in c("misc")) {
    lx <- x[[jx]]
    for (ix in names(lx)[names(lx) != "label"])  {
      count <- count + 1
      dfl[[count]] <- data.frame(
        variable = ix,
        variable_label = lx[[ix]]$variable_label,
        variable_neg = NA,
        variable_pos = NA,
        trend = lx[[ix]]$trend,
        cycle = lx[[ix]]$cycle,
        corr = NA,
        constr_drift = NA,
        constr_cycle = NA,
        group = jx,
        group_label = lx$label,
        transform = lx[[ix]]$transform
      )
    }
  }  
  df_obs <- do.call(rbind, dfl)
  
  #  ------ loadings
  dfl_loadings <- list()
  for (lx in x[c("group1", "group2", "agggroup")]) {
    for (ix in lx$variable)  {
      for (jx in lx$agg_load) {
        count <- count + 1
        dfl_loadings[[count]] <- data.frame(
          variable = ix,
          loads_on = x$agg$variable,
          lag = jx,
          type = "cycle"
        ) %>%
          mutate(
            parameter_name = paste0(type, "_load_", ix, "_", loads_on, "_L", lag),
            loading_state = gsub("_L0", "", paste0(type, "_", loads_on, "_L", lag))
          )
      }
    }
  }
  for (lx in x[c("subgroup1")]) {
    for (ix in lx$variable)  {
      for (jx in lx$agg_load) {
        count <- count + 1
        dfl_loadings[[count]] <- data.frame(
          variable = ix,
          loads_on = x$group1$variable[
            grep(
              gsub(lx$prefix, "", ix), 
              gsub(x$group1$prefix, "", x$group1$variable))
          ],
          lag = jx,
          type = "cycle"
        ) %>%
          mutate(
            parameter_name = paste0(type, "_load_", ix, "_", loads_on, "_L", lag),
            loading_state = gsub("_L0", "", paste0(type, "_", loads_on, "_L", lag))
          )
      }
    }
  }
  for (lx in x[c("misc")]) {
    for (ix in names(lx)[names(lx) != "label"])  {
      for (jx in lx[[ix]]$loadings) {
        for (lag in jx$lag) {
          count <- count + 1
          dfl_loadings[[count]] <- data.frame(
            variable = ix,
            loads_on = jx$variable,
            lag = lag,
            type = jx$type
          ) %>%
            mutate(
              parameter_name = paste0(type, "_load_", ix, "_", loads_on, "_L", lag),
              loading_state = gsub("_L0", "", paste0(type, "_", loads_on, "_L", lag))
            )
        }
      }
    }
  }
  df_loadings <- do.call(rbind, dfl_loadings) %>%
    left_join(., df_obs %>% select(variable, variable_label, group, group_label), by = "variable")
  df_lags <- df_loadings %>% 
    group_by(variable = loads_on, type) %>%
    dplyr::summarize(max_lag = max(lag)) 
  
  # ----- indirect loadings
  df_tmp1 <- df_loadings %>%
    filter(type == "cycle") %>%
    filter(loads_on %in% variable) 
  
  dfl_loadings_extra <- list()
  df_loadings_extra <- data.frame()
  if (NROW(df_tmp1) > 0 ) {
    for (ix in 1:NROW(df_tmp1)) {
      
      dfl_loadings_extra[[ix]] <- df_loadings %>%
        filter(type == "cycle") %>%
        filter(df_tmp1$loads_on[ix] == variable) %>% 
        rename(
          parameter_name_indirect = parameter_name,
          lag_indirect = lag
        ) %>% 
        select(-variable) %>% 
        mutate(
          variable = df_tmp1$variable[ix],
          lag_direct = df_tmp1$lag[ix],
          lag = lag_direct + lag_indirect,
          parameter_name_direct = df_tmp1$parameter_name[ix],
          loading_state = gsub("_L0", "", paste0(type, "_", loads_on, "_L", lag))
        )
      
    }
    df_loadings_extra <- do.call(rbind, dfl_loadings_extra) %>% 
      select(-group) %>%
      left_join(., df_obs %>% select(variable, variable_label, group, group_label), by = "variable")
    df_lags_extra <- df_loadings_extra %>% 
      group_by(variable = loads_on, type) %>%
      dplyr::summarize(max_lag_extra = max(lag)) 
    
    df_lags <- df_lags %>%
      full_join(.,df_lags_extra, by = c("variable", "type")) %>%
      group_by(variable, type) %>%
      mutate(max_lag = max(max_lag, max_lag_extra, na.rm = TRUE)) %>%
      select(-max_lag_extra)
  }
  
  # ------ variance
  dfl_variance <- list()
  dfl_variance[[1]] <- df_obs %>% 
    filter(trend == 4 | trend == 3 | trend == 1) %>% 
    transmute(
      variable = variable,
      parameter_name = paste0("var_trend_", variable), 
      state = paste0("trend_", variable),
      type = "trend",
      corr = ifelse(!is.na(corr), corr, 0)
    ) 
  dfl_variance[[2]] <- df_obs %>% 
    filter(trend >= 2) %>% 
    transmute(
      variable = variable,
      parameter_name = paste0("var_drift_", variable), 
      state = paste0("drift_", variable),
      type = "drift",
      corr = ifelse(!is.na(corr), corr, 0)
    )
  dfl_variance[[3]] <- df_obs %>% 
    transmute(
      variable = variable,
      parameter_name = paste0("var_cycle_", variable), 
      state = paste0("cycle_", variable),
      type = "cycle",
      corr = NA
    ) 
  df_variance <- do.call(rbind, dfl_variance) %>%
    left_join(., df_obs %>% select(variable, variable_label, group, group_label), by = "variable")
  
  # ----- covariance
  idx <- df_variance$type != "cycle" & !is.na(df_variance$corr) & df_variance$corr != 0
  if (sum(idx)>0) {
    df_variance_idx <- df_variance[idx, ]
    covar_names <- sapply(1:length(df_variance_idx$parameter_name), function(x) {
      paste0(gsub(".*var_", "covar_", df_variance_idx$parameter_name[x]), 
             paste0(gsub(".*var_", "_", df_variance_idx$parameter_name) ))
    }) 
    covar_names <- c(covar_names[lower.tri(covar_names)])
    
    df_covariance <- data.frame(
      parameter_name = covar_names
    ) %>% mutate(
      variable = sapply(strsplit(gsub(".*covar_","",parameter_name), split="_"), function(x) paste0(x[1:2], collapse = "_")),
      variable2 = sapply(strsplit(gsub(".*covar_","",parameter_name), split="_"), function(x) paste0(x[3:4], collapse = "_")),
      type = gsub("_.*", "", variable),
      type2 = gsub("_.*", "", variable2)
    ) %>%
      left_join(., df_variance[, c("variable", "corr")], by = "variable") %>% 
      filter(!(corr!=4 & type != type2))
  } else {
    df_covariance <- data.frame()
  }
  
  # ----- AR
  dfl_AR <- list()
  dfl_AR[[1]] <- df_obs %>% 
    filter(cycle >= 1) %>% 
    transmute(
      variable = variable,
      parameter_name = paste0("cycle_", variable, "_AR"), 
      state = paste0("cycle_", variable),
      state_lag = state,
      lag = 1,
      type = "cycle"
    ) 
  dfl_AR[[2]] <- df_obs %>% 
    filter(cycle == 2) %>% 
    transmute(
      variable = variable,
      parameter_name = paste0("cycle_", variable, "_AR_L1"), 
      state = paste0("cycle_", variable),
      state_lag = paste0(state, "_L1"),
      lag = 2,
      type = "cycle"
    ) 
  dfl_AR[[3]] <- df_obs %>% 
    filter(trend == 3) %>% 
    transmute(
      variable = variable,
      parameter_name = paste0("drift_", variable, "_AR"), 
      state = paste0("drift_", variable),
      state_lag = state,
      lag = 1,
      type = "drift"
    ) 
  df_AR <- do.call(rbind, dfl_AR) %>%
    left_join(., df_obs %>% select(variable, variable_label, group, group_label), by = "variable")
  
  # ----- const
  dfl_const <- list()
  dfl_const[[1]] <- df_obs %>% 
    filter(trend == 3) %>% 
    transmute(
      variable = variable,
      parameter_name = paste0("const_drift_", variable), 
      state = paste0("drift_", variable),
      state_lag = "const"
    ) 
  df_const <- do.call(rbind, dfl_const)
  
  
  # ------ AR drift
  df_AR_drift <- left_join(
    df_AR %>% 
      filter(type == "drift") %>% 
      rename(phi=parameter_name) %>% 
      mutate(var_cycle = paste0("var_", state)) %>% 
      select(-state_lag, -variable), 
    df_const %>% 
      rename(const = parameter_name) %>% 
      select(-state_lag), 
    by = "state")
  
  # ------ lags
  df_lags <- full_join(
    df_lags, 
    df_obs %>% 
      transmute(variable = variable, max_lag_AR = cycle - 1, type = "cycle"),
    by = c("variable", "type")
  ) %>% dplyr::mutate(max_lag = max(max_lag, max_lag_AR, na.rm = TRUE)) %>%
    dplyr::select(!max_lag_AR) %>%
    ungroup
  
  # ------ constraints
  dfl_constr <- list()
  count <- 0
  for (gx in c("group1", "group2", "subgroup1")) {
    lx <- x[[gx]]
    if (length(lx$variable) > 0) {    
      count <- count + 1
      
      dfl_constr[[count]] <- data.frame(
        group = gx,
        drift = ifelse(is.null(lx$constr_drift), NA, lx$constr_drift),
        trend = ifelse(is.null(lx$constr_trend), NA, lx$constr_trend),
        cycle = ifelse(is.null(lx$constr_cycle), NA, lx$constr_cycle),
        name_agg = lx$name_agg,
        name_residual = ifelse(is.null(lx$name_residual), NA, lx$name_residual),
        residual = ifelse(is.null(lx$name_residual), FALSE, TRUE)
      )
    }
  }
  df_constr <- do.call(rbind, dfl_constr)
  if (!is.null(df_constr)) {
    df_constr <- df_constr %>% 
      pivot_longer(cols = c("drift", "cycle", "trend"), names_to = "type") %>% 
      filter(value) %>% 
      dplyr::select(!value) %>%
      as.data.frame()
  }
  
  # consolidate and return
  return(
    list(
      obs = df_obs, 
      loadings = df_loadings,
      loadings_extra = df_loadings_extra,
      variance = df_variance,
      covariance = df_covariance,
      AR = df_AR,
      AR_drift = df_AR_drift,
      const = df_const,
      lags = df_lags,
      constr = df_constr
    )
  )
  
}
