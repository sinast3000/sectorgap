
# -------------------------------------------------------------------------------------------

#' Prior and posterior plots
#' 
#' @description Creates diagnostic plots of the posterior distribution.
#'
#' @param fit fitted object
#' @inheritParams plot.ss_fit
#'
#' @importFrom stats dnorm quantile
#' @import ggplot2
#' @importFrom MCMCpack dinvgamma
#' @importFrom dplyr %>% select filter mutate group_by
#' 
#' @return nothing
#' @keywords internal
plot_densities <- function(
  fit, 
  file_path, 
  n_col = 4,
  alpha = 0.05,
  save = TRUE,
  title = TRUE,
  device = "jpg",
  width = 10,
  height = 2,
  units = "in"
) {
  
  # to avoid RMD check note
  . <- group <- variable_label <- plot_title <- draw <- parameter_name <- 
    posterior <- lb <- ub <- type <- variable <- distribution <- prior <- NULL
  
  # settings
  settings <- fit$settings
  df_set <- settings_to_df(x = settings)
  df_prior <- fit$prior %>% select(-variable, -parameter_name, -distribution) %>% t
  colnames(df_prior) <- fit$prior$parameter_name
  par_names <- colnames(df_prior)
  burnin <- attr(fit, "burnin")
  n_draws <- NROW(fit$mcmc$parameters)
  
  # LOADINGS, AR ---------------------------------------------------------------
    
  par_type <- c("loadings", "AR")[c(dim(df_set$loadings)[1]>0, dim(df_set$AR)[1]>0)]
  for (pt in par_type) {
    
    # sort by group
    par_loadings <- df_set[[pt]]$parameter_name
    groups <- df_set[[pt]] %>% .$group %>% unique 
    dfl <- lapply(groups, function(x) {
      df_set[[pt]] %>% 
        filter(group == x) %>%
        mutate(
          plot_title = gsub('(.{1,30})(\\s|$)', '\\1\n', paste0(variable_label, ", lag ", lag)),
          plot_title = substr(plot_title, 1, nchar(plot_title) - 1)
        )
    })
    titlel <- lapply(groups, function(x) {
      df_set[[pt]] %>% 
        filter(group == x) %>% .$group_label %>% .[1] %>%
        paste0(., ": ", pt)
    })
    names(dfl) <- names(titlel) <- groups
    
    # data
    tib <- fit$mcmc$parameters[, df_set[[pt]]$parameter_name] %>% 
      data.frame("draw" = 1:NROW(.), .) %>%
      pivot_longer(-draw, names_to = "parameter_name", values_to = "posterior") %>%
      left_join(., do.call(rbind, dfl)[, c("parameter_name", "plot_title")], by = "parameter_name") %>%
      filter(draw > floor(n_draws * burnin))
    
    # loop through variable groups
    for (k1 in 1:length(dfl)) {
  
        try({
          
          # select  parameters
          tib_posterior <- tib %>% 
            filter(parameter_name %in% dfl[[k1]]$parameter_name)
        
          # get x limits
          xlim <- tib_posterior %>% 
            group_by(parameter_name) %>% 
            summarize(lb = quantile(posterior, alpha, na.rm= TRUE), ub = quantile(posterior, 1 - alpha, na.rm= TRUE)) %>% 
            summarize(lb=min(lb), ub = max(ub)) %>% 
            as.numeric()
  
          # prior densities
          grid <- seq(xlim[1], xlim[2], by = diff(xlim) / floor(n_draws * burnin - 1))
          tib_prior <- lapply(unique(tib_posterior$parameter_name), function(x) {
              data.frame( 
                draw = floor(n_draws * burnin+1):n_draws,
                parameter_name = x,
                grid = grid,
                prior = dnorm(grid, df_prior[1, x], sqrt(df_prior[2, x]))
              )
            }) %>%
            do.call(rbind, .)
          tib_plot <- left_join(tib_posterior, tib_prior, by = c("draw", "parameter_name"))
          
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
          if (title) p <- p + labs(title = titlel[[k1]])
          
          if (save) {
            n_p <- NROW(dfl[[k1]])
            
            n_linebreak_title <- max(sapply(tib_posterior$plot_title, function(x) lengths(regmatches(x, gregexpr("\n", x)))))
            filename <- file.path(
              file_path, 
              gsub(":", "", titlel[[k1]]) %>%
                gsub(" ", "_", .) %>%
                paste0(., ".", device) %>%
                paste0("density_", .)
            )
            ggsave(
              filename = filename, 
              plot = p, 
              width = width * 0.05 + (width * 0.95) / n_col * min(n_p, n_col), 
              height = height * ceiling(n_p / n_col) + 1 + title * 0.5 + (n_linebreak_title - 1) * 0.25,
              units = units,
              device = device
            )
          } else {
            print(p)
          }
        })
        
    }
  }
  
  # VARIANCES -------------------------------------------------------------------
  
  # sort by type
  par_variances <- df_set$variance$parameter_name
  types <- df_set$variance %>% .$type %>% unique 
  dfl <- lapply(types, function(x) {
    df_set$variance %>% 
      filter(type == x) %>%
      mutate(
        plot_title = gsub('(.{1,30})(\\s|$)', '\\1\n', variable_label),
        plot_title = substr(plot_title, 1, nchar(plot_title) - 1)
      )
  })
  titlel <- lapply(types, function(x) {
    paste0(
      toupper(substr(x, 1, 1)), 
      tolower(substr(x, 2, nchar(x))),
      " variances"
    )
  })
  names(dfl) <- names(titlel) <- types
  
  # data
  tib <- fit$mcmc$parameters[, par_variances] %>% 
    data.frame("draw" = 1:NROW(.), .) %>%
    pivot_longer(-draw, names_to = "parameter_name", values_to = "posterior") %>%
    left_join(., do.call(rbind, dfl)[, c("parameter_name", "plot_title")], by = "parameter_name") %>%
    filter(draw > floor(n_draws * burnin))
  
  # loop through variable types
  for (k1 in 1:length(dfl)) {
    
    try({
      
      # select parameters
      tib_posterior <- tib %>% 
        filter(parameter_name %in% dfl[[k1]]$parameter_name)
      
      # get x limits
      xlim <- tib_posterior %>% 
        group_by(parameter_name) %>% 
        summarize(lb = quantile(posterior, alpha, na.rm= TRUE), ub = quantile(posterior, 1 - alpha, na.rm= TRUE)) %>% 
        summarize(lb=min(lb), ub = max(ub)) %>% 
        as.numeric()
      
      # prior densities
      grid <- seq(xlim[1], xlim[2], by = diff(xlim) / floor(n_draws * burnin - 1))
      tib_prior <- lapply(unique(tib_posterior$parameter_name), function(x) {
        data.frame( 
          draw = floor(n_draws * burnin+1):n_draws,
          parameter_name = x,
          grid = grid,
          prior = dinvgamma(grid, shape = df_prior[2, x]/2, scale = df_prior[1, x]/2)
        )
      }) %>%
        do.call(rbind, .)
      tib_plot <- left_join(tib_posterior, tib_prior, by = c("draw", "parameter_name"))
      
      
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
      if (title) p <- p + labs(title = titlel[[k1]])
      
      if (save) {
        n_p <- NROW(dfl[[k1]])
        n_linebreak_title <- max(sapply(tib_posterior$plot_title, function(x) lengths(regmatches(x, gregexpr("\n", x)))))
        filename <- file.path(
          file_path, 
          gsub(" ", "_", titlel[[k1]]) %>%
            paste0(., ".", device) %>%
            paste0("density_", .)
        )
        ggsave(
          filename = filename,
          plot = p, 
          width = width * 0.05 + (width * 0.95) / n_col * min(n_p, n_col), 
          height = height * ceiling(n_p / n_col) + 1 + title * 0.5 + (n_linebreak_title - 1) * 0.25,
          units = units,
          device = device          
        )
      } else {
        print(p)
      }
    })
    
  }
  
}
