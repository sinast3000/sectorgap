
# -------------------------------------------------------------------------------------------

#' Prior and posterior plots
#' 
#' @description Creates trace plots for the draws of the posterior distribution.
#'
#' @param fit fitted object
#' @inheritParams plot.ss_fit
#'
#' @import ggplot2
#' @importFrom dplyr %>% select filter mutate group_by
#' 
#' @return nothing
#' @keywords internal
plot_trace <- function(
  fit, 
  file_path, 
  include_burnin = FALSE,
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
    posterior <- lb <- ub <- type <- variable <- distribution <- prior <- 
    expanding_average <- NULL
  
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
      group_by(plot_title) %>%
      mutate(
        summary = mean(posterior),
        expanding_average = sapply(1:length(posterior), function(x) mean(posterior[1:x]))
      ) 
    if (!include_burnin) {
      tib <- tib %>%
        filter(draw > floor(n_draws * burnin))
    }
    
    # loop through variable groups
    for (k1 in 1:length(dfl)) {
      
      try({
        
        # select  parameters
        tib_plot <- tib %>% 
          filter(parameter_name %in% dfl[[k1]]$parameter_name) 

        p <- ggplot(tib_plot) +
          facet_wrap( ~ plot_title, ncol = n_col, scales = "free_y") +
          geom_line(aes(x = draw, y = posterior, color = "posterior", linetype = "posterior"), 
                    linewidth = 0.25) +
          geom_line(aes(x = draw, y = expanding_average, color = "expanding mean", linetype = "expanding mean"), 
                    linewidth = 0.75) +
          theme_minimal() +
          labs(x = NULL, y = NULL) +
          scale_color_manual(values = c("black", "grey60")) +
          scale_linetype_manual(values = c(1,1), guide = guide_none()) +
          theme(legend.position="bottom",
                legend.title = element_blank(),
                panel.border = element_rect(fill = NA),
                text = element_text(size=10),
                axis.ticks.x = element_line(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
          ) # +
          # guides(color = guide_legend(override.aes = list(linetype = 1:2)))
        if (include_burnin) p <- p + geom_vline(xintercept = floor(n_draws * burnin), lwd = 0.2)
        if (title) p <- p + labs(title = titlel[[k1]])
        
        if (save) {
          n_p <- NROW(dfl[[k1]])
          
          n_linebreak_title <- max(sapply(tib_plot$plot_title, function(x) lengths(regmatches(x, gregexpr("\n", x)))))
          filename <- file.path(
            file_path, 
            gsub(":", "", titlel[[k1]]) %>%
              gsub(" ", "_", .) %>%
              paste0(., ".", device) %>%
              paste0("trace_", .)
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
    group_by(parameter_name) %>%
    mutate(
      summary = mean(posterior),
      expanding_average = sapply(1:length(posterior), function(x) mean(posterior[1:x]))
    ) 
  if (!include_burnin) {
    tib <- tib %>%
      filter(draw > floor(n_draws * burnin))
  }

  # loop through variable types
  for (k1 in 1:length(dfl)) {

    try({

      # select  parameters
      tib_plot <- tib %>% 
        filter(parameter_name %in% dfl[[k1]]$parameter_name) 
      
      p <- ggplot(tib_plot) +
        facet_wrap( ~ plot_title, ncol = n_col, scales = "free_y") +
        geom_line(aes(x = draw, y = posterior, color = "posterior", linetype = "posterior"), 
                  linewidth = 0.25) +
        geom_line(aes(x = draw, y = expanding_average, color = "expanding mean", linetype = "expanding mean"), 
                  linewidth = 0.75) +
        theme_minimal() +
        labs(x = NULL, y = NULL) +
        scale_color_manual(values = c("black", "grey60")) +
        scale_linetype_manual(values = c(1,1), guide = guide_none()) +
        theme(legend.position="bottom",
              legend.title = element_blank(),
              panel.border = element_rect(fill = NA),
              text = element_text(size=10),
              axis.ticks.x = element_line(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank()
        ) # +
      # guides(color = guide_legend(override.aes = list(linetype = 1:2)))
      if (include_burnin) p <- p + geom_vline(xintercept = floor(n_draws * burnin), lwd = 0.2)
      if (title) p <- p + labs(title = titlel[[k1]])

      if (save) {
        n_p <- NROW(dfl[[k1]])
        n_linebreak_title <- max(sapply(tib_plot$plot_title, function(x) lengths(regmatches(x, gregexpr("\n", x)))))
        filename <- file.path(
          file_path,
          gsub(" ", "_", titlel[[k1]]) %>%
            paste0(., ".", device) %>%
            paste0("trace_", .)
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
