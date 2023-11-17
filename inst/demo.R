
# clear
rm(list=ls())

# functions
devtools::load_all()

# TODO
# - ...

# SETTINGS --------------------------------------------------------------------------------------

set <- initialize_settings()
df_set <- settings_to_df(x = set)

# DATA ------------------------------------------------------------------------------------------

load(file = "inst/demo_data2.RData")

tsm <- lapply(1:NROW(df_set$obs), function(x) {
    trans <- df_set$obs$transform[x]
    vx <- df_set$obs$variable[x]
    if (trans) {
      set$fun_transform(tsl[[vx]])
    } else {
      tsl[[vx]]
    }
  }) %>%
  do.call(cbind, .)
colnames(tsm) <- df_set$obs$variable

tsm_p <- do.call(cbind, tsl_p)

ts_start <- c(1990,1)
ts_end <- c(2025,4)

tsm <- window(tsm, start = ts_start, end = ts_end, extend = TRUE)
tsm_p <- window(tsm_p, start = ts_start, end = ts_end, extend = TRUE)
tsl_w <- lapply(tsl_w, window, start = ts_start, end = ts_end, extend = TRUE)


# MODEL ------------------------------------------------------------------------

model <- define_ssmodel(
  settings = set, 
  tsm = tsm, 
  weightl = tsl_w
)


# PRIOR ------------------------------------------------------------------------

prior <- initialize_prior(
  model = model, 
  settings = set, 
  lambda_t = 100, 
  lambda_d = 100, 
  df = 6
) 

# BAYESIAN ESTIMATION ----------------------------------------------------------

fit <- estimate_ssmodel(
  model = model, 
  settings = set, 
  prior = prior, 
  R = 1000, 
  burnin = 0.5,
  thin = 10, 
  HPDIprob = 0.68
)

# change HPDI
# fit2 <- estimation_results(
#   fit = fit,
#   HPDIprob = 0.85
# )

# save(fit, model, set, file = "inst/results/demo_results.RData")

# PLOTS ----------------------------------------------------------------------------

recessions <- get_recessions(country = "ch")

df <- prepare_output(
  fit = fit, 
  tsl_w = tsl_w, 
  tsm_p = tsm_p, 
  estimate = "median"
)

# create basic time series plots
create_plots(
  highlighted_area = recessions,
  n_col = 3,
  df = df, 
  settings = set, 
  file_path = "inst/fig", 
  # plot_start = 1992, 
  # plot_end = 2023.25,
  title = TRUE,
  save = TRUE
)

plot_densities(
  fit = fit, 
  file_path = "inst/fig"
)


# # posteropr plots
# plot_posterior(fitB, alpha = 0.4, type = "loading")
# plot_posterior(fitB, alpha = 0.4, type = "AR")
# plot_posterior(fitB, alpha = 0.4, type = "variance_trend")
# plot_posterior(fitB, alpha = 0.4, type = "variance_drift")
# plot_posterior(fitB, alpha = 0.4, type = "variance_cycle")
# 

