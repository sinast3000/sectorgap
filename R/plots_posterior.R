
# -------------------------------------------------------------------------------------------

#' Prior and posterior plots
#' 
#' Plots the diagnostic plots of the posterior distribution.
#'
#' @param fit fitted object
#' @param alpha cut off value for posterior
#' @inheritParams create_plots
#'
#' @importFrom stats density qbeta dnorm dgamma
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom MCMCpack dinvgamma
#' 
#' @return nothing
plot_densities <- function(
  fit, 
  file_path, 
  n_col = 4,
  alpha = 0.05,
  print = TRUE, 
  save = TRUE,
  df_labels = NULL
) {
  
  # settings
  settings <- fit$settings
  distr <- do.call(cbind, prior <- fit$prior)
  par_names <- colnames(distr)
  burnin <- attr(fit, "burnin")
  n_draws <- NROW(fit$mcmc$parameters)
  
  # LOADINGS -------------------------------------------------------------------
    
  # identify different groups
  vars_agg <- c(settings$agggroup$variable, settings$misc$variable)
  par_loadings <- par_names[grepl("_load_", par_names)]
  lags <- sort(unique((as.numeric(substr_r(par_loadings, n = 1)))))
  idx_agg <- grepl(settings$agg$variable, par_loadings)
  par_loadings_list <- list(
    group1 = par_loadings[!is.null(settings$group1$variable) & 
                          grepl(paste0(settings$group1$variable, collapse = "|"), par_loadings) & 
                          idx_agg],
    group2 = par_loadings[!is.null(settings$group2$variable) & 
                         grepl(paste0(settings$group2$variable, collapse = "|"), par_loadings) & 
                         idx_agg],
    rest_agg = par_loadings[!is.null(vars_agg) & 
                          grepl(paste0(vars_agg, collapse = "|"), par_loadings) & 
                         idx_agg],
    misc = par_loadings[!is.null(vars_agg) & 
                          grepl(paste0(vars_agg, collapse = "|"), par_loadings) & 
                          !idx_agg],
    subgroup1 = par_loadings[!is.null(settings$subgroup1$variable) & 
                          grepl(paste0(settings$subgroup1$variable, collapse = "|"), par_loadings) & 
                         !idx_agg]
  )
  par_loadings_list <- par_loadings_list[unlist(lapply(par_loadings_list, function(x) length(x) > 0))]
  
  # variable names, loadings, lags and plot titles
  dfl <- lapply(par_loadings_list, function(x) {
    y <- data.frame(
      variable = x,
      loading_variable = gsub("_.*", "", gsub(".*_load_", "", x)),
      type_variable = gsub(".*_","",gsub("_L.*", "", gsub(".*_load_", "", x))),
      type = gsub("_.*", "", gsub("_load_.*", "", x)),
      lag = substr_r(x, n = 1)
    )
    
    if (!is.null(df_labels)) {
      df_labels_tmp <- df_labels %>% rename(loading_variable = variable)
      y <- left_join(y, df_labels_tmp, by = "loading_variable")
    } else {
      y$series_label <- y$loading_variable
    }
    # y$plot_title <- with(y, paste0(loading_variable, " on ", type_variable, " ", type, ", lag ", lag))
    y$plot_title <- gsub('(.{1,30})(\\s|$)', '\\1\n', with(y, paste0(series_label, ", lag ", lag)))
    y$plot_title <- substr(y$plot_title, 1, nchar(y$plot_title) - 1)
    y
  })
  
  # loading data
  tib <- fit$mcmc$parameters[, par_loadings] %>% 
    data.frame("draw" = 1:NROW(.), .) %>%
    pivot_longer(-draw, names_to = "variable", values_to = "posterior") %>%
    left_join(., do.call(rbind, dfl)[, c("variable", "plot_title")], by = "variable") %>%
    filter(draw > floor(n_draws * burnin))
  
  # loop through variable groups
  for (k1 in 1:length(dfl)) {

      try({
        
        # select variable group
        tib_posterior <- tib %>% filter(variable %in% dfl[[k1]]$variable)
      
        # get x limits
        xlim <- tib_posterior %>% 
          group_by(variable) %>% 
          summarize(lb = quantile(posterior, alpha, na.rm= TRUE), ub = quantile(posterior, 1 - alpha, na.rm= TRUE)) %>% 
          summarize(lb=min(lb), ub = max(ub)) %>% 
          as.numeric()

        # prior densities
        grid <- seq(xlim[1], xlim[2], by = diff(xlim) / floor(n_draws * burnin - 1))
        tib_prior <- lapply(unique(tib_posterior$variable), function(x) {
            data.frame( 
              draw = floor(n_draws * burnin+1):n_draws,
              variable = x,
              grid = grid,
              prior = dnorm(grid, distr[1, x], sqrt(distr[2, x]))
            )
          }) %>%
          do.call(rbind, .)
        tib_plot <- left_join(tib_posterior, tib_prior, by = c("draw", "variable"))
        
        p <- ggplot(tib_plot) +
          facet_wrap( ~ plot_title, ncol = n_col, scales = "fixed") +
          geom_density(aes(x = posterior, color = "posterior", linetype = "posterior"), 
                       linewidth = 0.75, show_guide = FALSE) +
          geom_line(aes(x = grid, y = prior, color = "prior", linetype = "prior"), 
                    linewidth = 0.75) +
          theme_minimal() +
          geom_vline(xintercept = 0, lwd = 0.2) +
          labs(x = NULL, y = "density") +
          xlim(xlim) +
          scale_color_manual(values = c("black", "grey60")) +
          scale_linetype_manual(values = 1:2, guide = guide_none()) +
          theme(legend.position="bottom",
                legend.title = element_blank(),
                panel.border = element_rect(fill = NA),
                text = element_text(size=10),
                axis.ticks.x = element_line(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
          ) +
          guides(color = guide_legend(override.aes = list(linetype = 1:2)))
        
        if (print) print(p)
        if (save) {
          n_linebreak_title <- max(sapply(tib_posterior$plot_title, function(x) lengths(regmatches(x, gregexpr("\n", x)))))
          ggsave(
            filename = file.path(file_path, paste0("loadings_", names(dfl)[k1], ".jpg")), 
            plot = p, 
            width = 0.5 + 9.5 / n_col * min(NROW(dfl[[k1]]), n_col),#10, 
            height = 1 + (1.5 + 1/6 * n_linebreak_title) * ceiling(NROW(dfl[[k1]]) / n_col)  
          )
          dev.off()
        }
      })
      
  }
  
  # VARIANCES -------------------------------------------------------------------
  
  # identify different groups
  vars_agg <- c(settings$agggroup$variable, settings$misc$variable)
  par_variances <- par_names[grepl(paste0("var_", c("trend", "drift", "cycle"), collapse = "|"), par_names)]
  lags <- sort(unique((as.numeric(substr_r(par_variances, n = 1)))))
  par_variances_list <- list(
    cycle = par_variances[grepl("_cycle_", par_variances)],
    trend = par_variances[grepl("_trend_", par_variances)],
    drift = par_variances[grepl("_drift_", par_variances)]
  )
  par_variances_list <- par_variances_list[unlist(lapply(par_variances_list, function(x) length(x) > 0))]
  
  # variable names, loadings, lags and plot titles
  dfl <- lapply(par_variances_list, function(x) {
    y <- data.frame(
      variable = x,
      variance = gsub("_.*", "", gsub("var_", "", x)),
      variance_variable = gsub(".*_","",gsub("_L.*", "", gsub("var_", "", x)))
    )
    # # y$plot_title <- with(y, paste0(variance_variable, " ", variance))
    # y$plot_title <- with(y, paste0(variance_variable))
    # y
    
    if (!is.null(df_labels)) {
      df_labels_tmp <- df_labels %>% rename(variance_variable = variable)
      y <- left_join(y, df_labels_tmp, by = "variance_variable")
      idx_duplicated <- duplicated(y$series_label, fromLast = TRUE) | duplicated(y$series_label)
      y$series_label[idx_duplicated] <- paste0(y$series_label[idx_duplicated], " (", y$group[idx_duplicated], ")")
    } else {
      y$series_label <- y$variance_variable
    }
    # y$plot_title <- with(y, paste0(variance_variable))
    y$plot_title <- gsub('(.{1,30})(\\s|$)', '\\1\n', with(y, series_label))
    y$plot_title <- substr(y$plot_title, 1, nchar(y$plot_title) - 1)
    y
    
    
  })
  
  # loading data
  tib <- fit$mcmc$parameters[, par_variances] %>% 
    data.frame("draw" = 1:NROW(.), .) %>%
    pivot_longer(-draw, names_to = "variable", values_to = "posterior") %>%
    left_join(., do.call(rbind, dfl)[, c("variable", "plot_title")], by = "variable") %>%
    filter(draw > floor(n_draws * burnin))
  
  # loop through variable groups
  for (k1 in 1:length(dfl)) {
    
    try({
      
      # select variable group
      tib_posterior <- tib %>% filter(variable %in% dfl[[k1]]$variable)
      
      # get x limits
      xlim <- tib_posterior %>% 
        group_by(variable) %>% 
        summarize(lb = quantile(posterior, alpha, na.rm= TRUE), ub = quantile(posterior, 1 - alpha, na.rm= TRUE)) %>% 
        summarize(lb=min(lb), ub = max(ub)) %>% 
        as.numeric()
      
      # prior densities
      grid <- seq(xlim[1], xlim[2], by = diff(xlim) / floor(n_draws * burnin - 1))
      tib_prior <- lapply(unique(tib_posterior$variable), function(x) {
        data.frame( 
          draw = floor(n_draws * burnin+1):n_draws,
          variable = x,
          grid = grid,
          prior = dinvgamma(grid, shape = distr[2, x]/2, scale = distr[1, x]/2)
        )
      }) %>%
        do.call(rbind, .)
      tib_plot <- left_join(tib_posterior, tib_prior, by = c("draw", "variable"))
      
      
      p <- ggplot(tib_plot) +
        facet_wrap( ~ plot_title, ncol = n_col, scales = "free_y") +
        geom_density(aes(x = posterior, color = "posterior", linetype = "posterior"), 
                     linewidth = 0.75, show_guide = FALSE) +
        geom_line(aes(x = grid, y = prior, color = "prior", linetype = "prior"), 
                  linewidth = 0.75) +
        theme_minimal() +
        labs(x = NULL, y = "density") +
        xlim(xlim) +
        scale_color_manual(values = c("black", "grey60")) +
        scale_linetype_manual(values = 1:2, guide = guide_none()) +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              panel.border = element_rect(fill = NA),
              text = element_text(size=10),
              axis.ticks.x = element_line(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank()
        ) +
        guides(color = guide_legend(override.aes = list(linetype = 1:2)))
      
      if (print) print(p)
      if (save) {
        n_linebreak_title <- max(sapply(tib_posterior$plot_title, function(x) lengths(regmatches(x, gregexpr("\n", x)))))
        ggsave(
          filename = file.path(file_path, paste0("variances_", names(dfl)[k1], ".jpg")), 
          plot = p, 
          width = 10, 
          height = 1 + (1.5 + 1/6 * n_linebreak_title) * ceiling(NROW(dfl[[k1]]) / n_col)  
        )
        dev.off()
      }
    })
    
  }
  
  # AR -------------------------------------------------------------------------
  
  # identify different groups
  vars_agg <- c(settings$agggroup$variable, settings$misc$variable)
  par_AR <- par_names[!(par_names %in% c(par_loadings, par_variances))]
  lags <- suppressWarnings(sort(c(0, unique((as.numeric(substr_r(par_AR, n = 1)))))))
  par_AR_list <- list(
    group1 = par_AR[!is.null(settings$group1$variable) & 
                          grepl(paste0(settings$group1$variable, collapse = "|"), par_AR)],
    group2 = par_AR[!is.null(settings$group2$variable) & 
                         grepl(paste0(settings$group2$variable, collapse = "|"), par_AR)],
    misc = par_AR[!is.null(vars_agg) & 
                          grepl(paste0(vars_agg, collapse = "|"), par_AR)],
    subgroup1 = par_AR[!is.null(settings$subgroup1$variable) & 
                          grepl(paste0(settings$subgroup1$variable, collapse = "|"), par_AR)]
  )
  par_AR_list <- par_AR_list[unlist(lapply(par_AR_list, function(x) length(x) > 0))]
  
  # variable names, loadings, lags and plot titles
  dfl <- lapply(par_AR_list, function(x) {
    y <- data.frame(
      variable = x,
      cycle_variable = gsub("_.*", "", gsub("cycle_", "", x)),
      cycle = gsub(".*_","",gsub("_L.*", "", gsub("cycle_load_", "", x))),
      lag = substr_r(x, n = 1)
    )
    y$lag[y$lag == "R"] <- 0
    # y$plot_title <- with(y, paste0(cycle_variable, " AR, lag ", lag))
    # y
    
    if (!is.null(df_labels)) {
      df_labels_tmp <- df_labels %>% rename(cycle_variable = variable)
      y <- left_join(y, df_labels_tmp, by = "cycle_variable")
    } else {
      y$series_label <- y$cycle_variable
    }
    # y$plot_title <- with(y, paste0(cycle_variable, " AR, lag ", lag))
    y$plot_title <- gsub('(.{1,30})(\\s|$)', '\\1\n', with(y, paste0(series_label, ", lag ", lag)))
    y$plot_title <- substr(y$plot_title, 1, nchar(y$plot_title) - 1)
    y
    
    
  })
  
  # loading data
  tib <- fit$mcmc$parameters[, par_AR] %>% 
    data.frame("draw" = 1:NROW(.), .) %>%
    pivot_longer(-draw, names_to = "variable", values_to = "posterior") %>%
    left_join(., do.call(rbind, dfl)[, c("variable", "plot_title")], by = "variable") %>%
    filter(draw > floor(n_draws * burnin))
  
  # loop through variable groups
  for (k1 in 1:length(dfl)) {
    
    try({
      
      # select variable group
      tib_posterior <- tib %>% filter(variable %in% dfl[[k1]]$variable)
      
      # get x limits
      xlim <- tib_posterior %>% 
        group_by(variable) %>% 
        summarize(lb = quantile(posterior, alpha, na.rm= TRUE), ub = quantile(posterior, 1 - alpha, na.rm= TRUE)) %>% 
        summarize(lb=min(lb), ub = max(ub)) %>% 
        as.numeric()
      
      # prior densities
      grid <- seq(xlim[1], xlim[2], by = diff(xlim) / floor(n_draws * burnin - 1))
      tib_prior <- lapply(unique(tib_posterior$variable), function(x) {
        data.frame( 
          draw = floor(n_draws * burnin+1):n_draws,
          variable = x,
          grid = grid,
          prior = dnorm(grid, distr[1, x], sqrt(distr[2, x]))
        )
      }) %>%
        do.call(rbind, .)
      tib_plot <- left_join(tib_posterior, tib_prior, by = c("draw", "variable"))
      
      
      p <- ggplot(tib_plot) +
        facet_wrap( ~ plot_title, ncol = n_col, scales = "fixed") +
        geom_density(aes(x = posterior, color = "posterior", linetype = "posterior"), 
                     linewidth = 0.75, show_guide = FALSE) +
        geom_line(aes(x = grid, y = prior, color = "prior", linetype = "prior"), 
                  linewidth = 0.75) +
        geom_vline(xintercept = 0, lwd = 0.2) +
        theme_minimal() +
        labs(x = NULL, y = "density") +
        xlim(xlim) +
        scale_color_manual(values = c("black", "grey60")) +
        scale_linetype_manual(values = 1:2, guide = guide_none()) +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              panel.border = element_rect(fill = NA),
              text = element_text(size=10),
              axis.ticks.x = element_line(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank()
        ) +
        guides(color = guide_legend(override.aes = list(linetype = 1:2)))
      
      if (print) print(p)
      if (save) {
        n_linebreak_title <- max(sapply(tib_posterior$plot_title, function(x) lengths(regmatches(x, gregexpr("\n", x)))))
        ggsave(
          filename = file.path(file_path, paste0("AR_", names(dfl)[k1], ".jpg")), 
          plot = p, 
          width = 10, 
          height = 1 + (1.5 + 1/6 * n_linebreak_title) * ceiling(NROW(dfl[[k1]]) / n_col)  
        )
        dev.off()
      }
    })
    
  }
  
}
