# -------------------------------------------------------------------------------------------

#' Print \code{settings} object
#'
#' @description Prints the model settings.
#'
#' @param x object of class \code{settings}
#' @param call logical, if \code{TRUE}, the call will be printed
#' @param check logical, if \code{TRUE}, the model class will be checked
#' @param ... Ignored.
#' @return No return value
#' @export
print.settings <- function(x, call = TRUE, check = TRUE, ...) {
  
  group <- . <- variable <- variable_label <- loads_on <- NULL
  
  # settings to data frames
  df_set <- settings_to_df(x = x)
  
  y <- df_set$obs
  
  if (call) {
    mc <- attr(x, "call")
    cat("Call:\n", paste(deparse(mc), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  }
  
  cat("Object of class settings\n")
  
  ig <- "agg"
  groups <- c("agg", "group1", "group2", "subgroup1", "agggroup")
  for (ig in groups) {
    df <- y %>% filter(group == ig)
    set <- y[[ig]]
    cycle <- switch(
      as.character(df$cycle[1]),
      "2" = "AR(2)",
      "1" = "AR(1)",
      "0" = "NWN")
    trend <- switch(
      as.character(df$trend[1]),
      "4" = "local linear",
      "3" = "local linear with AR(1) drift",
      "2" = "local linear, no trend variance",
      "1" = "random walk"
    )
    corr <- switch(
      as.character(df$corr[1]),
      "NA" = "no correlation",
      "0" = "no correlation",
      "4" = "correlation of trends and drifts",
        "2" = "correlation of drifts",
        "1" = "correlation of trends"
    )
    cat(
      paste0("\n--------------- ", ig, ": ", set$label[1], "\n"),
      paste0("\n cycle: \t", cycle), 
      paste0("\n trend: \t", trend)
    )
    if (!is.null(set$load_name) && !is.na(df$constr_trend[1]) && df$constr_trend[1]) {
      cat(paste0("\n \t\t", "trend constrained to ", set$load_name))
    }
    if (!is.null(set$load_name) && !is.na(df$constr_drift[1]) && df$constr_drift[1]) {
      cat(paste0("\n \t\t", "drift constrained to ", set$load_name))
    }
    if (NROW(df) > 1) cat(paste0("\n \t\t", corr))
    if (ig != "subgroup1") {
      if (NROW(df) > 1) cat(paste0("\n loads on:\t", set$load_name, ", lag(s)"), paste0(set$load_lag, collapse = ", "))
    } else {
      cat(paste0("\n loads on:\t", 
                 paste0(df_set$loadings %>% filter(group == "subgroup1") %>% .$loads_on %>% unique, collapse = ", "), ", lag(s)"), 
          paste0(set$load_lag, collapse = ", "))
    }
    cat(
      paste0("\n variables: \n")
    )
    df <- df %>% 
      mutate(variable = paste0("   ", variable)) %>%
      rename("   name" = variable, "label" = variable_label, "transform" = transform) %>%
      select("   name", "label", "transform")
    print(df, row.names = FALSE, right = FALSE)
  }
  
  # misc
  if (length(y$misc) > 1 ) {
    ig <- "misc"
    df <- df_set$obs %>% filter(group == ig)
    set <- y[[ig]]
    
    cat(paste0("\n--------------- ", toupper(ig), ": ", y[[ig]]$label[1]))
    
    
    for (ix in 2:length(y$misc)) {
      variable_i <- names(y[[ig]])[ix]
      df <- df_set$obs %>% filter(group == ig, variable == variable_i)
      set <- y[[ig]][[ix]]
      cycle <- switch(
        as.character(df$cycle[1]),
        "2" = "AR(2)",
        "1" = "AR(1)",
        "0" = "NWN")
      trend <- switch(
        as.character(df$trend[1]),
        "4" = "local linear",
        "3" = "local linear with AR(1) drift",
        "2" = "local linear, no trend variance",
        "1" = "random walk"
      )
      cat("\n",
        paste0("\n variable: \t", df$variable, "\n label: \t", df$variable_label), 
        paste0("\n cycle: \t", cycle), 
        paste0("\n trend: \t", trend),
        paste0("\n transform: \t", df$transform)
      )
  
      df_tmp <- df_set$loadings %>% 
        filter(variable == variable_i) %>% 
        group_by(loads_on) %>% 
        summarize(lags = paste0(lag, collapse = ", "))
      for (ik in 1:NROW(df_tmp)) {
        if (ik == 1) cat("\n loads on:")
        else cat("\n\t")
        cat(paste0("\t", df_tmp$loads_on[ik], ", lag(s)"), 
            paste0(df_tmp$lags[ik], collapse = ", "))
      }
    }
  }
  
  
}


# -------------------------------------------------------------------------------------------

#' Print \code{ss_model} object
#'
#' @description Prints the model specifications of an object of class 
#' \code{ss_model}.
#'
#' @param x object of class \code{ss_model}
#' @param call logical, if \code{TRUE}, the call will be printed
#' @param check logical, if \code{TRUE}, the model class will be checked
#' @param ... Ignored.
#' @return No return value
#' @export
print.ss_model <- function(x, call = TRUE, check = TRUE, ...) {

  if (call) {
    mc <- attr(x, "call")
    cat("Call:\n", paste(deparse(mc), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  }
  
  attrib <- attributes(x)
  
  cat("State space model object of class ss_model\n")
  cat(
    "\n--------------- Dimensions:\n",
    paste0("\n  Time points:\t", attrib$n),
    paste0("\n  Time series:\t", attrib$p),
    paste0("\n  Innovations:\t", attrib$k),
    paste0("\n  States:\t", attrib$m),
    "\n"
  )
  cat("\n--------------- Stationary states:\n\n")
  print(x$names$stationary)
  cat("\n--------------- Non-stationary states:\n\n")
  print(x$names$root)
  
}

# -------------------------------------------------------------------------------------------

#' Print \code{prior} object
#'
#' @description Prints the model specifications of an object of class 
#' \code{prior}.
#'
#' @param x object of class \code{prior}
#' @param call logical, if \code{TRUE}, the call will be printed
#' @param check logical, if \code{TRUE}, the model class will be checked
#' @param ... Ignored.
#' @return No return value
#' 
#' @importFrom dplyr %>% filter
#' @export
print.prior <- function(x, call = TRUE, check = TRUE, ...) {

  # to avoid RMD check note
  distribution <- NULL
  
  if (call) {
    mc <- attr(x, "call")
    cat("Call:\n", paste(deparse(mc), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  }
  
  class(x) <- class(x)[-1]
  df_normal <- x %>% filter(distribution == "normal") %>% select(-distribution)
  df_igamma <- x %>% filter(distribution == "inverse-gamma") %>% select(-distribution)
  
  colnames(df_normal) <- c("variable", "parameter name", "mean", "variance", "initial value")
  colnames(df_igamma) <- c("variable", "parameter name", "scale", "degrees of freedom", "initial value")

  cat("Data frame object of class prior\n")
  cat("\n--------------- Normally distributed parameters:\n\n")
  print(df_normal, row.names = FALSE)
  cat("\n--------------- Inverse-gamma distributed parameters:\n\n")
  print(df_igamma, row.names = FALSE)
  
}



# ------------------------------------------------------------------------------

#' Print \code{ss_fit} object.
#'
#' @description Prints the model specifications of an object of class \code{ss_fit}.
#'
#' @param x object of class \code{ss_fit}
#' @param call logical, if \code{TRUE}, the call will be printed
#' @param check logical, if \code{TRUE}, the model class will be checked
#' @param ... Ignored.
#' 
#' @return No return value
#' 
#' @importFrom dplyr %>%
#' @importFrom stats qnorm
#' 
#' @export
print.ss_fit<- function(x, call = TRUE, check = TRUE, ...) {

  # to avoid RMD check note
  . <- NULL
  
  if (call) {
    mc <- attr(x, "call")
    cat("Call:\n", paste(deparse(mc), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  }
  
  cat("Fitted state space model of class ss_fit\n")
  
  # Bayesian settings
  attrib <- attributes(x)
  cat(
    "\n--------------- Bayesian settings:\n",
    paste0("\n  Repetitions:\t\t", attrib$R),
    paste0("\n  Burnin period (%):\t", attrib$burnin),
    paste0("\n  Skipped draws:\t", attrib$thin - 1, "/", attrib$thin),
    paste0("\n  HPDI (%):\t\t", attrib$HPDIprob * 100)
  )
  
  # convergence
  alpha <- 0.1
  pars_nconverge_idx <-abs(x$parameters[, "Geweke statistic"]) > qnorm(p = 1 - alpha / 2)
  pars_nconverge <- rownames(x$parameters)[pars_nconverge_idx]
  states_geweke <- x$tsl$state_summary[, grepl("Geweke", colnames(x$tsl$state_summary)) &
                                         !grepl("gap_", colnames(x$tsl$state_summary))]
  # Bonfferoni correction per state
  states_nconverge_count <- apply(
    states_geweke, 
    2, 
    function(x) sum(x > qnorm(p = 1 - alpha/length(x) / 2))
  )
  states_nconverge_idx <- states_nconverge_count > 0
  states_nconverge <- states_geweke[, states_nconverge_idx] %>%
    colnames %>%
    gsub("\\..*", "", .) %>%
    data.frame(., states_nconverge_count[states_nconverge_idx])
  colnames(states_nconverge) <- c("state", "time points")
  cat(paste0("\n\n--------------- Convergence at ", alpha * 100, "% level:\n"))
  cat(paste0("\n  ", sum(!pars_nconverge_idx), "/", length(pars_nconverge_idx), 
             " parameters have converged"))
  cat(paste0("\n  ", sum(!states_nconverge_idx), "/", 
             length(states_nconverge_idx), " states have converged for all time points (Bonferroni corrected)"))
  if (sum(pars_nconverge_idx) > 0) {
    cat("\n\n  Non-converged parameters:\n" )
    print(pars_nconverge)
  }
  if (sum(pars_nconverge_idx) > 0) {
    cat("\n  Non-converged states:\n" )
    print(states_nconverge, row.names = FALSE)
  }
  
}
