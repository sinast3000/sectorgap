
# clear
rm(list=ls())

# # packages
library(zoo)
library(timeseriesdb)
library(KFAS)
# library(tstools)
# library(ggplot2)
# library(seasonal)
# library(dplyr)
# library(tidyr)

# functions
# sapply(paste0("R/", list.files("R/")), source)
devtools::load_all()

# TODO
# - ...

# DATA ------------------------------------------------------------------------------------------

# fetch data
con <- db_connection_create("kofdb", host = "archivedb.kof.ethz.ch")
tsll <- fetch_data(con = con, annual = FALSE, fc = FALSE)



# SETTINGS --------------------------------------------------------------------------------------

tsll$agg$output <- tsll$agg$gdpos
tsll$agg_n$output <- tsll$agg_n$gdpos

tsll$group1$vaA <- kofvja::agg_chain(
  rlist = tsll$group1[c("vaabcde", "vaf")], 
  nlist = tsll$group1_n[c("vaabcde", "vaf")], 
  baseYear = 2020
)
# tsll$group1$vaB <- kofvja::agg_chain(
#   rlist = tsll$group1[c("vaghji", "vaklmn", "vaopqrstos")], 
#   nlist = tsll$group1_n[c("vaghji", "vaklmn", "vaopqrstos")], 
#   baseYear = 2020
# )
tsll$group1$vaB <- kofvja::agg_chain(
  rlist = tsll$group1[c("vaghji", "vaklmn", "vap", "vaq", "varstos")], 
  nlist = tsll$group1_n[c("vaghji", "vaklmn", "vap", "vaq", "varstos")], 
  baseYear = 2020
)
tsll$group1$vaC <- kofvja::agg_chain(
  rlist = tsll$group1[c("vao", "bericht")], 
  nlist = tsll$group1_n[c("vao", "bericht")], 
  baseYear = 2020
)
# tsll$group1$vaC <- tsll$group1$bericht
tsll$group1_n$vaA <- Reduce("+", tsll$group1_n[c("vaabcde", "vaf")])
# tsll$group1_n$vaB <- Reduce("+", tsll$group1_n[c("vaghji", "vaklmn", "vaopqrstos")])
tsll$group1_n$vaB <- Reduce("+", tsll$group1_n[c("vaghji", "vaklmn", "vap", "vaq", "varstos")])
tsll$group1_n$vaC <- Reduce("+", tsll$group1_n[c("vao", "bericht")])
# tsll$group1_n$vaC <- tsll$group1_n$bericht

tsll$group2$exp1 <- tsll$group2$constot
tsll$group2$exp2 <- tsll$group2$ifix
tsll$group2$exp3 <- tsll$group2$extot1
tsll$group2$exp4 <- tsll$group2$imtot1
tsll$group2_n$exp1 <- tsll$group2_n$constot
tsll$group2_n$exp2 <- tsll$group2_n$ifix
tsll$group2_n$exp3 <- tsll$group2_n$extot1
tsll$group2_n$exp4 <- tsll$group2_n$imtot1

tsll$subgroup1$vzaeqA <- Reduce("+", tsll$subgroup1[paste0("vzaeq", c("b", "c", "d", "e", "f"))])
# tsll$subgroup1$vzaeqB <- Reduce("+", tsll$subgroup1[paste0("vzaeq", c("g", "h", "j", "i", "k", "l", "m", "n", "o", "p", "q", "r", "s"))])
tsll$subgroup1$vzaeqB <- Reduce("+", tsll$subgroup1[paste0("vzaeq", c("g", "h", "j", "i", "k", "l", "m", "n", "p", "q", "r", "s"))])



tsll$inflation$inflation <- tsll$inflation$cpi
tsll$agggroup$employment <- tsll$agggroup$ltotv
tsll$agggroup$urate <- tsll$agggroup$urilo

set <- initialize_settings()




tsll <- prepare_data(tsll, settings = set) 



# matrix ts format, transformations
tsl <- c(tsll$agg, tsll$group1, tsll$group2, tsll$agggroup, tsll$inflation, tsll$subgroup1)
# tsl$ycsub <- -tsl$ycsub
df_set <- settings_to_df(x = set)
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



# time period
# # tsm <- window(tsm, start = c(1991,4), end = c(2019,4), extend = TRUE)
# tsm <- window(tsm, start = c(1991,4), end = c(2023,2), extend = TRUE)
tsm <- window(tsm, start = c(1992,1), end = c(2025,4), extend = TRUE)
# tsm <- window(tsm, start = c(1992,1), end = c(2025,4), extend = TRUE)
# tsm <- window(tsm, start = c(1980,1), end = c(2023,2), extend = TRUE)
# # tsm <- window(tsm, start = c(1991,3), end = c(2022,3), extend = TRUE)
# # tsm <- window(tsm, start = c(1990,1), end = c(2030,4), extend = TRUE)
# tsm <- window(tsm, start = c(1991, 1), end = c(2022,1), extend = TRUE)
tsm



save(tsll, tsm, file = "inst/demo_data.RData")

tsl <- c(tsll$agg, tsll$group1, tsll$group2, tsll$agggroup, tsll$inflation, tsll$subgroup1)
tsl_p <- c(tsll$agg_p, tsll$group1_p, tsll$group2_p)
tsl_w <- list(
  group1 = do.call(cbind, tsll$group1_nw),
  group2 = do.call(cbind, tsll$group2_nw),
  subgroup1 = do.call(cbind, tsll$subgroup1_w)
)

save(tsl, tsl_p, tsl_w, file = "inst/demo_data2.RData")









