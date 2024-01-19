
#' Time series plots
#' 
#' @description Creates a set of time series plots of the results.
#'
#' @inheritParams define_ssmodel
#' @inheritParams plot.ss_fit
#' 
#' @importFrom dplyr %>% filter mutate select distinct pull full_join rename
#' @importFrom grDevices hcl.colors
#' @importFrom stats aggregate
#' 
#' @return nothing
#' 
#' @keywords internal
plot_time_series <- function(
  df,
  settings = settings,
  n_col = 3,
  n_sep = 5,
  highlighted_area = NULL,
  plot_start = NULL,
  plot_end = NULL,
  file_path = NULL,
  title = TRUE,
  save = TRUE,
  device = "jpg",
  width = 10,
  height = 3,
  units = "in"
) {

  # to avoid RMD check note
  group <- type <- obs_name <- series_label <- value <- obs <- lb <- ub <- 
    HPDI <- series <- . <- idiosynchratic <- gap <- common <- 
    contr <- fit <- NULL
  
  # settings to data frames
  df_set <- settings_to_df(x = settings)
  
  # x axis settings
  dates <- sort(unique(df$date))
  if (is.null(plot_start)) plot_start <- floor(min(dates)) - floor(min(dates)) %% n_sep
  if (is.null(plot_end)) plot_end <- ceiling(max(dates)) + n_sep - ceiling(max(dates)) %% n_sep

  # manipulate highighted area such they are drawn on borders
  highlight <- !is.null(highlighted_area)
  if (highlight) {
    idx <- apply(highlighted_area, 1, function(x) plot_start >= x[1] & plot_start <= x[2])
    highlighted_area[idx, 1] <- plot_start
    idx <- apply(highlighted_area, 1, function(x) plot_end >= x[1] & plot_end <= x[2])
    highlighted_area[idx, 2] <- plot_end
  }
  
  # start of x ticks
  tick_start <- ifelse(
    plot_start %% n_sep == 0, 
    plot_start,
    plot_start + n_sep - (plot_start + n_sep) %% n_sep
  )
  
  # colors
  colorl <- list(
    agg = "black",
    agggroup = hcl.colors(
      max(2, df_set$obs %>% filter(group %in% c("agg", "misc", "agggroup")) %>% NROW), 
      palette = "Plasma"
    ),
    group1 = hcl.colors(
      max(2, df_set$obs %>% filter(group %in% "group1") %>% NROW + 1), 
      palette = "Roma"
    ),
    group2 = hcl.colors(
      max(2, df_set$obs %>% filter(group %in% "group2") %>% NROW + 1), 
      palette = "Zissou 1"
    ),
    subgroup1 = hcl.colors(
      max(2, df_set$obs %>% filter(group %in% "subgroup1") %>% NROW + 1), 
      palette = "Greens"
    ),
    misc = hcl.colors(
      max(2, df_set$obs %>% filter(group %in% "misc") %>% NROW + 1), 
      palette = "Dark3"
    )
  )
  # adjust subgroup1 colors if not all subindicators are present
  if (!is.null(settings$subgroup1)) {
    colors <- colorl$group1
    names(colors) <- c(settings$group1$variable, settings$group1$name_residual)
    # idx with matching names of group1
    idx <- settings$subgroup1$match_group1[!is.na(settings$subgroup1$match_group1)]
    # name color vector for assignment
    names(colorl$subgroup1) <- c(settings$subgroup1$match_group1, settings$subgroup1$name_residual)
    colorl$subgroup1[idx] <- colors[idx]
    colorl$subgroup1[length(colorl$subgroup1)] <- colors[length(colors)]
    names(colorl$subgroup1) <- NULL
  }
  
  # ---------------------------------------------------------------------------
  # multiple plots in grid
  
  plotl <- c(
    lapply(df_set$obs$group %>% unique, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("gap"),
        obs_name = df$variable,
        title = paste0(df$group_label[1], " gaps (in %)"),
        scales = "free_y",
        hline = 0
      )
    }),
    lapply(df_set$obs$group %>% unique, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("drift"),
        obs_name = df$variable,
        title = paste0(df$group_label[1], " drifts (in %)"),
        scales = "free_y",
        hline = 0
      )
    }),
    lapply(df_set$obs$group %>% unique, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("trend"),
        obs_name = df$variable,
        title = paste0(df$group_label[1], " trends (in %)"),
        scales = "free_y",
        hline = NULL
      )
    })
  )
  
  for (px in 1:length(plotl)) {

    tab <- df %>% 
      filter(type %in% plotl[[px]]$type, obs_name %in% plotl[[px]]$obs_name) %>%
      mutate(series_label = factor(series_label, levels = series_label, labels = series_label))
      
    if (NROW(tab) > 0) {
      try({
        p <- ggplot(data = tab) +
          facet_wrap( ~ series_label, ncol = n_col, scales = plotl[[px]]$scales)
        if (highlight) {
          p <- p + 
            geom_rect(data = highlighted_area, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                      fill='grey80', alpha = 0.2)
        }
        p <- p +
          geom_hline(yintercept = plotl[[px]]$hline, lwd = 0.05) +
          geom_line(data = tab, aes(x = as.numeric(date), y = value, group = series_label, color = series_label),
                    linewidth = 0.5, color = "black") +
          geom_line(data = tab, aes(x = as.numeric(date), y = obs, group = series_label, color = series_label),
                    linewidth = 0.5, linetype = 5, color = "black") +
          geom_ribbon(data = tab, aes(x = as.numeric(date), ymin = lb, ymax = ub, fill = paste0(HPDI, "% HPDI")), 
                      fill = "grey12", alpha = 0.2) +
          labs(title = NULL, x = NULL, y = NULL)  +
          theme_minimal() +
          scale_x_continuous(
            limits = c(plot_start, plot_end - 0.25), 
            breaks = seq(tick_start, plot_end, n_sep), 
            expand = expansion(mult = 0.005, add = 0)
          ) +
          scale_linewidth_manual(values = 0.8) +
          theme(legend.position="bottom",
                panel.grid.major.x = element_blank(),
                panel.border = element_rect(fill = NA),
                text = element_text(size=10),
                axis.ticks.x = element_line(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank())  
        if (title) p <- p + labs(title = plotl[[px]]$title)
        
        if (save) {
          n_p <- tab %>% select(series) %>% distinct %>% pull %>% length
          filename <- file.path(
            file_path, 
            gsub(" \\(in.*", "", paste0(plotl[[px]]$title)) %>%
              gsub(" ", "_", .) %>%
              paste0("separate_", ., ".", device)
          )
          ggsave(
            filename = filename,
            plot = p, 
            width = width * 0.05 + (width * 0.95) / n_col * min(n_p, n_col), 
            height = height * ceiling(n_p / n_col),
            units = units,
            device = device
          )
        } else {
          print(p)
        }
      })
    }

  }
  
  # ---------------------------------------------------------------------------
  # combined plots
  
  plotl <- c(
    lapply(df_set$obs$group %>% unique, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("gap"),
        obs_name = df$variable,
        title = paste0(df$group_label[1], " gaps (in %)"),
        scales = "fixed",
        hline = 0,
        color = colorl[[x]]
      )
    }),
    lapply(df_set$obs$group %>% unique, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("drift"),
        obs_name = df$variable,
        title = paste0(df$group_label[1], " drifts (in %)"),
        scales = "fixed",
        hline = 0,
        color = colorl[[x]]
      )
    }),
    lapply(df_set$obs$group %>% unique, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("trend"),
        obs_name = df$variable,
        title = paste0(df$group_label[1], " trends (in %)"),
        scales = "free_y",
        hline = NULL,
        color = colorl[[x]]
      )
    })
  )
  
  for (px in 1:length(plotl)) {

    tab <- df %>% 
      filter(type %in% plotl[[px]]$type, obs_name %in% plotl[[px]]$obs_name)
    if (NROW(tab) > 0) {
      try({
        p <- ggplot()
        if (highlight) {
          p <- p + 
            geom_rect(data = highlighted_area, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                      fill='grey80', alpha = 0.2)
        }
        p <- p +
          geom_hline(yintercept = plotl[[px]]$hline, lwd = 0.05) +
          geom_line(data = tab, aes(x = as.numeric(date), y = value, color = series_label),
                    linewidth = 0.5) +
          labs(title = NULL, x = NULL, y = NULL)  +
          theme_minimal() +
          scale_color_manual(values = plotl[[px]]$color) +
          scale_x_continuous(
            limits = c(plot_start, plot_end - 0.25), 
            breaks = seq(tick_start, plot_end, n_sep), 
            expand = expansion(mult = 0.005, add = 0)
          ) +
          theme(legend.position="bottom",
                panel.grid.major.x = element_blank(),
                panel.border = element_rect(fill = NA),
                text = element_text(size=10),
                axis.ticks.x = element_line(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()) +
          guides(color = guide_legend(ncol = 4, title = ""))
        if (title) p <- p + labs(title = plotl[[px]]$title)
  
        if (save) {
          filename <- file.path(
            file_path, 
            gsub(" \\(in.*", "", paste0(plotl[[px]]$title)) %>%
              gsub(" ", "_", .) %>%
              paste0("combined_", ., ".", device)
          )
          ggsave(
            filename = filename,
            plot = p, 
            width = width, 
            height = height,
            units = units,
            device = device
          )
        } else {
          print(p)
        }
      })
    }
  }

  
  # ---------------------------------------------------------------------------
  # sector gap contributions, grid
  
  idx_group <- df_set$obs$group %>% unique %>% .[-1]
  plotl <- c(
    lapply(idx_group, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        obs_name = df$variable,
        title = paste0(df$group_label[1], " gap decomposition"),
        scales = "fixed",
        hline = 0
      )
    })
  )
  
  name_common <- "shared"
  
  for (px in seq_len(length(plotl))) {
   
    tab1 <- df %>% 
      filter(type %in% "cycle", obs_name %in% plotl[[px]]$obs_name) %>%
      mutate(idiosynchratic = value) %>%
      select(date, obs_name, series_label, idiosynchratic) 
    tab2 <- df %>% 
      filter(type %in% "gap", obs_name %in% plotl[[px]]$obs_name) %>%
      mutate(gap = value) %>%
      full_join(tab1, tab2, by = c("date", "obs_name", "series_label")) %>%
      mutate(common = gap - idiosynchratic) %>%
      select(date, obs_name, series_label, common, gap) 
    
    tab <- tab1 %>%
      rename(., value = idiosynchratic) %>%
      mutate(type = "idiosynchratic") %>%
      full_join(., df, by = c("date", "obs_name", "series_label", "type", "value"))
    tab <- tab2 %>%
      rename(., value = common) %>%
      mutate(type = "common") %>%
      full_join(., tab, by = c("date", "obs_name", "series_label", "type", "value"))
    tab <- tab %>%
      filter(type %in% c("common", "idiosynchratic"))
    tab$type[tab$type == "common"] <- name_common
    
    if (NROW(tab) > 0) {
      try({
      
        p <- ggplot(data = tab) +
          facet_wrap( ~ series_label, ncol = n_col, scales = plotl[[px]]$scales)
        if (highlight) {
          p <- p + 
            geom_rect(data = highlighted_area, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                      fill='grey80', alpha = 0.2)
        }
        p <- p +
          geom_hline(yintercept = 0, lwd = 0.05) +
          geom_bar(data = tab, aes(y = value, x = as.numeric(date), fill = type), 
                   stat = "identity") +
          geom_line(data = tab %>% filter(type == name_common), aes(x = as.numeric(date), y = gap, group = series_label, color = series_label),
                    linewidth = 0.5, color = "black") +
          labs(title = NULL, x = NULL, y = NULL)  +
          theme_minimal() +
          scale_fill_manual(values = c("grey60", "grey30")) +
          scale_x_continuous(
            limits = c(plot_start, plot_end - 0.25), 
            breaks = seq(tick_start, plot_end, n_sep), 
            expand = expansion(mult = 0.005, add = 0)
          ) +
          scale_linewidth_manual(values = 0.8) +
          theme(legend.position="bottom",
                panel.grid.major.x = element_blank(),
                panel.border = element_rect(fill = NA),
                text = element_text(size=10),
                axis.ticks.x = element_line(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank())  +
          guides(fill = guide_legend(title = ""))
        if (title) p <- p + labs(title = plotl[[px]]$title)
        
        if (save) {
          n_p <- tab %>% select(series_label) %>% distinct %>% pull %>% length
          filename <- file.path(
            file_path, 
            gsub(" \\(in.*", "", paste0(plotl[[px]]$title)) %>%
              gsub(" ", "_", .) %>%
              gsub("_decomposition", "", .) %>%
              paste0("loading_decomposition_", ., ".", device)
          )
          ggsave(
            filename = filename,
            plot = p, 
            width = width * 0.05 + (width * 0.95) / n_col * min(n_p, n_col), 
            height = height * ceiling(n_p / n_col),
            units = units,
            device = device
          )
        } else {
          print(p)
        }
      })
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # gap contributions
  
  idx_group <- c("group1", "group2", "subgroup1")
  plotl <- c(
    lapply(idx_group, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("gap"),
        obs_name = c(df$variable, paste0("error", x)),
        title = paste0("Gap decomposition (", df$group_label[1], ")"),
        scales = "fixed",
        hline = 0,
        color = colorl[[x]]
      )
    }),
    lapply(idx_group, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("drift"),
        obs_name = c(df$variable, paste0("error", x)),
        title = paste0("Drift decomposition (", df$group_label[1], ")"),
        scales = "fixed",
        hline = 0,
        color = colorl[[x]]
      )
    }),
    lapply(idx_group, function(x) {
      df <- df_set$obs %>% filter(group == x)
      list(
        type = c("trend"),
        obs_name = c(df$variable, paste0("error", x)),
        title = paste0("Trend decomposition (", df$group_label[1], ")"),
        scales = "fixed",
        hline = 0,
        color = colorl[[x]]
      )
    })
  )

  for (px in seq_len(length(plotl))) {
    
    tab <- df %>% 
      filter(type %in% plotl[[px]]$type, obs_name %in% plotl[[px]]$obs_name) 
    if (NROW(tab) > 0) {
      try({
        p <- ggplot()
        if (highlight) {
          p <- p + 
            geom_rect(data = highlighted_area, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf),
                      fill='grey80', alpha = 0.2)
        }
        p <- p +
          geom_hline(yintercept = 0, lwd = 0.05) +
          geom_bar(data = tab, aes(y = contr, x = as.numeric(date), fill = series_label), 
                   stat = "identity") +
          geom_line(data = tab %>% select(contr) %>% aggregate(by = list(date = tab$date), FUN = sum), 
                    aes(x = as.numeric(date), y = contr),
                    linewidth = 0.75, color = "black") +
          labs(title = NULL, x = NULL, y = NULL)  +
          theme_minimal() +
          scale_fill_manual(values = c(
            plotl[[px]]$color[1:(length(plotl[[px]]$obs_name) - 1)],
            plotl[[px]]$color[length(plotl[[px]]$color)]
          )) +
          scale_x_continuous(
            limits = c(plot_start, plot_end - 0.25), 
            breaks = seq(tick_start, plot_end, n_sep), 
            expand = expansion(mult = 0.005, add = 0)
          ) +
          scale_linewidth_manual(values = 0.8) +
          theme(legend.position="bottom",
                panel.grid.major.x = element_blank(),
                panel.border = element_rect(fill = NA),
                text = element_text(size=10),
                axis.ticks.x = element_line(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()) +
          guides(fill = guide_legend(ncol = 4, title = "", byrow = FALSE))
        if (title) p <- p + labs(title = plotl[[px]]$title)
        
        if (save) { 
          filename <- file.path(
            file_path, 
            gsub(" \\(in.*", "", paste0(plotl[[px]]$title)) %>%
              gsub(" ", "_", .) %>%
              gsub("_decomposition", "", .) %>%
              paste0("decomposition_", ., ".", device)
          )
          ggsave(
            filename = filename,
            plot = p, 
            width = width,
            height = height,
            units = units,
            device = device
          )
        } else {
          print(p)
        }
      })
    }
    
  }
  
}
