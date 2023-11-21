
# clear
rm(list=ls())

# # packages
library(timeseriesdb)

# RAW DATA ---------------------------------------------------------------------

# output keys
keys <- c (
  "gdp",
  "gdpos",
  "vaa", 
  "vab", 
  "vac", 
  "van19_21",
  "vacon19_21",
  "vade",
  "vaf", 
  "vag", 
  "van47",
  "vagon47",
  "vahj", 
  "vai", 
  "vak", 
  "valmn", 
  "vao", 
  "vap", 
  "vaq", 
  "varsos", 
  "varstos", 
  "varos",
  "var",
  "vars",
  "vas",
  "vat",
  "van64", 
  "van65", 
  "ytcom", 
  "ycsub",
  "yctax",
  "vaabcde",
  "vaghji",
  "vaklmn",
  "vaopqrstos",
  "vaopqrst",
  "bericht",
  "consg",
  "consp",
  "icnstr",
  "ime",
  "exc1",
  "excm",
  "exs",
  "ext",
  "exst",
  "imc1",
  "ims",
  "imt",
  "imst",
  "imtot1",
  "extot1",
  "constot",
  "ifix"
)
keys <- c(keys, paste0("n", keys))

# remaining keys
keys <- c(keys,
  "ltotv",
  "vzaeqb",
  "vzaeqc", 
  "vzaeqd",
  "vzaeqe",
  "vzaeqf", 
  "vzaeqg", 
  "vzaeqh", 
  "vzaeqi", 
  "vzaeqj", 
  "vzaeqk", 
  "vzaeql", 
  "vzaeqm", 
  "vzaeqn", 
  "vzaeqo", 
  "vzaeqp", 
  "vzaeqq", 
  "vzaeqr", 
  "vzaeqs",
  "et",
  "ltoth",
  "urilo",
  "uroff",
  "cpi"
)
keys <- paste0("ch.kof.modelinput.", keys)

# fetch data
con <- db_connection_create("kofdb", host = "archivedb.kof.ethz.ch")
tsl <- db_ts_read(
  con = con, 
  ts_keys = keys,
  schema = "vja", 
  valid_on = "2023-07-01"
)
names(tsl) <- gsub(".*modelinput.", "", names(tsl))

# SETTINGS ---------------------------------------------------------------------

settings <- initialize_settings()

# DATA ---------------------------------------------------------------------

tsl_endo <- tsl_n <- list()

# aggregate output
tsl_endo$output <- tsl$gdpos
tsl_n$output <- tsl$ngdpos

# group1: production side
tsl_n$vaA <- Reduce("+", tsl[paste0("n", c("vaabcde", "vaf"))])
tsl_n$vaB <- Reduce("+", tsl[paste0("n", c("vaghji", "vaklmn", "vap", "vaq", "varstos"))])
tsl_n$vaC <- Reduce("+", tsl[paste0("n", c("vao", "bericht"))])
tsl_endo$vaA <- kofvja::agg_chain(
  rlist = tsl[c("vaabcde", "vaf")], 
  nlist = tsl[paste0("n", c("vaabcde", "vaf"))], 
  baseYear = 2020
)
tsl_endo$vaB <- kofvja::agg_chain(
  rlist = tsl[c("vaghji", "vaklmn", "vap", "vaq", "varstos")], 
  nlist = tsl[paste0("n", c("vaghji", "vaklmn", "vap", "vaq", "varstos"))], 
  baseYear = 2020
)
tsl_endo$vaC <- kofvja::agg_chain(
  rlist = tsl[c("vao", "bericht")], 
  nlist = tsl[paste0("n", c("vao", "bericht"))], 
  baseYear = 2020
)

# group2: expenditure side
tsl_endo$exp1 <- tsl$constot
tsl_endo$exp2 <- tsl$ifix
tsl_endo$exp3 <- tsl$extot1
tsl_endo$exp4 <- tsl$imtot1
tsl_n$exp1 <- tsl$nconstot
tsl_n$exp2 <- tsl$nifix
tsl_n$exp3 <- tsl$nextot1
tsl_n$exp4 <- tsl$nimtot1

# agggroup
tsl_endo$employment <- tsl$ltotv
tsl_endo$urate <- tsl$urilo
# compute inflation rate from consumer price index
tsl_endo$inflation <- 100 * (tsl$cpi / stats::lag(tsl$cpi, k = - 4) - 1)

# subgroup1: full time equivalent employment
tsl_endo$fteA <- Reduce("+", tsl[paste0("vzaeq", c("b", "c", "d", "e", "f"))])
tsl_endo$fteB <- Reduce("+", tsl[paste0("vzaeq", c("g", "h", "j", "i", "k", "l", "m", "n", "p", "q", "r", "s"))])

data_ch <- list(
  tsl = tsl_endo,
  tsl_n = tsl_n
)

# save package data
usethis::use_data(data_ch, internal = FALSE, overwrite = TRUE)

