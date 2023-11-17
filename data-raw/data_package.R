library(fredr)
library(xts)

fredr_set_key("932f5ba1f5a227813a70aee6312f04c5")



fredr(
  series_id = "A325RC1Q027SBEA",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01")
)


# usethis::use_data(gap, internal = FALSE, overwrite = TRUE)

fredr_series(series_id = "A325RC1Q027SBEA")



series <- fredr_series_search_text(
  search_text = "Real value added by industry",
  order_by = "popularity",
  sort_order = "desc"
  # limit = 1
)

s <- series %>% filter(
  frequency_short == "Q", 
  # grepl("BEA", id),
  # seasonal_adjustment_short == "SAAR"
  )
s$title






keys <- c(
  "A006RA3Q086SBEA", 
  "A191RA3Q086SBEA", 
  "B020RA3Q086SBEA", 
  "B020RG3Q086SBEA", 
  "B021RA3Q086SBEA", 
  "B021RG3Q086SBEA", 
  "B822RA3Q086SBEA", 
  "B822RG3Q086SBEA", 
  "DPCERA3Q086SBEA", 
  "EXPGSC1", 
  "GCEC1", 
  "GDPC1", 
  "GDPCTPI", 
  "GPDIC1", 
  "GPDICTPI", 
  "IMPGSC1", 
  "PCECC96", 
  "PCECTPI",
  "EXPGS", 
  "GCE", 
  "GDP", 
  "GPDI", 
  "IMPGS",
  "PCEC"
)
test <- lapply(keys, function(id) {
    y <- fredr(
      series_id = id,
      observation_start = as.Date("1990-01-01"),
      frequency = "q"
      # observation_end = as.Date("2000-01-01")
    )
    xts(x = y$value, order.by = y$date)
    ts(y$value, start = 1990, frequency = 4)
    
  }
) %>% do.call(cbind, .)
colnames(test) <- keys
test

info <- lapply(keys, fredr_series) %>% do.call(rbind, .) %>%
  mutate(title = tolower(title))


info_p <- info %>% filter(grepl("price index", title))
info_q <- info %>% filter(grepl("quantity index", title))
info_r <- info %>% filter(!grepl("index", title), grepl("real", title))
info_n <- info %>% filter(!grepl("index", title), !grepl("real", title))

info_p$title
title <- gsub("real ", "", info_r$title)

order_idx <- sapply(gsub("real ", "", info_r$title), function(x) which(grepl(x, info_p$title)))


tsm_n <- test[, info_r$id] * test[, info_p$id[order_idx]] / 100
colnames(tsm_n) <- title
cbind(Reduce("+", as.list(tsm_n[, !(title %in% "gross domestic product")])), tsm_n[, (title %in% "gross domestic product")])



order_idx <- sapply(gsub("real ", "", info_q$title), function(x) which(grepl(
  gsub(" \\(.*", "", x),
  gsub(" \\(.*", "", info_p$title))))


tsm_n <- test[, info_q$id] * test[, info_p$id[order_idx]] / 100
colnames(tsm_n) <- title
cbind(Reduce("+", as.list(tsm_n[, !(title %in% "gross domestic product")])), tsm_n[, (title %in% "gross domestic product")])



order_idx <- sapply(gsub("real ", "", info_r$title), function(x) which(grepl(x, info_q$title)))


tsm_n <- test[, info_r$id] * test[, info_q$id[order_idx]] / 100
colnames(tsm_n) <- title
cbind(Reduce("+", as.list(tsm_n[, !(title %in% "gross domestic product")])), tsm_n[, (title %in% "gross domestic product")])



tsm_n <- test[, info_n$id]

cbind(Reduce("+", as.list(tsm_n[, !(title %in% "gross domestic product")])), tsm_n[, (title %in% "gross domestic product")])







series <- fredr_series_search_text(
  search_text = "value added by industry",
  order_by = "popularity",
  sort_order = "desc"
  # limit = 1
)


s <- series %>% filter(
  frequency_short == "Q", 
  # grepl("BEA", id),
  seasonal_adjustment_short == "SAAR"
) %>% 
  mutate(title = tolower(title)) %>%
  filter(
    grepl("value added by industry", title),
    !grepl("private", title),
    !grepl("not allocated", title)
  ) 

######## real
s_real <- s %>% filter(grepl("real value added by industry", title)) %>%
  filter(
    !grepl(":", gsub(".*value added by industry: ", "", title))
  ) %>% 
  mutate(title = gsub(" \\(.*", "",
                      gsub(".*industry: ", "", title))) %>%
  arrange(title)
s_real$title %>% sort

s_real$id



tsm_real <- lapply(s_real$id, function(id) {
  y <- fredr(
    series_id = id,
    observation_start = as.Date("2005-01-01"),
    frequency = "q"
    # observation_end = as.Date("2000-01-01")
  )
  xts(x = y$value, order.by = y$date)
  ts(y$value, start = 2005, frequency = 4)
  
}
) %>% do.call(cbind, .)
colnames(tsm_real) <- gsub("real value added by industry: ", "", s_real$title)
tsm_real


######## nom
s_nom <- s %>% filter(
    !grepl("real value added by industry", title),
    grepl("Bill", units)
  ) %>%
  filter(
    !grepl(":", gsub(".*value added by industry: ", "", title))
  ) %>%  mutate(title = gsub(" \\(.*", "",
                              gsub(".*industry: ", "", title)))%>%
  arrange(title)
s_nom$title 

s_nom$id



tsm_nom <- lapply(s_nom$id, function(id) {
  y <- fredr(
    series_id = id,
    observation_start = as.Date("2005-01-01"),
    frequency = "q"
    # observation_end = as.Date("2000-01-01")
  )
  xts(x = y$value, order.by = y$date)
  ts(y$value, start = 2005, frequency = 4)
  
}
) %>% do.call(cbind, .)
colnames(tsm_nom) <- gsub("value added by industry: ", "", s_nom$title)
tsm_nom
Reduce("+", as.list(tsm_nom))

###### quantity index
s_quan <- s %>% filter(
  grepl("quantity index", title),
  grepl("Index", units)
) %>%
  filter(
    !grepl(":", gsub(".*value added by industry: ", "", title))
  ) %>% 
  mutate(title = gsub(" \\(.*", "",
                      gsub(".*industry: ", "", title))) %>%
  arrange(title)
s_quan$title

s_quan$id



tsm_quan <- lapply(s_quan$id, function(id) {
  y <- fredr(
    series_id = id,
    observation_start = as.Date("2005-01-01"),
    frequency = "q"
    # observation_end = as.Date("2000-01-01")
  )
  xts(x = y$value, order.by = y$date)
  ts(y$value, start = 2005, frequency = 4)
  
}
) %>% do.call(cbind, .)
colnames(tsm_quan) <- gsub("value added by industry: ", "", s_quan$title)
tsm_quan
Reduce("+", as.list(tsm_quan))


tsm_real2 <- sapply(1:NCOL(tsm_quan), function(x) {
  t(tsm_quan[,x] * as.numeric(window(ta(tsm_nom[,x])/4, start=2012, end = 2012))  / 100)
})  %>% ts(., start = 2005, frequency = 4)


# ------------------------------------------------------------------------------
sectors <- c(
  "private goods-producing industries",
  "private services-producing industries",
  "government"
)

unit <- c(
  "Billions of Dollars",
  "Index 2012=100",
  "Billions of Chained 2012 Dollars"
) 



s <- series %>% filter(
  frequency_short == "Q", 
  # grepl("BEA", id),
  seasonal_adjustment_short == "SAAR"
) %>% 
  mutate(title = tolower(title)) %>%
  filter(
    grepl("value added by industry", title),
    grepl(paste0(sectors, collapse = "|"), title),
    grepl(paste0(unit, collapse = "|"), units)
  ) 
s$title

######## nom
s_nom <- s %>% filter(
  grepl(unit[1], units)
) %>%
  filter(
    !grepl(":", gsub(".*value added by industry: ", "", title))
  ) %>%  mutate(title = gsub(" \\(.*", "",
                             gsub(".*industry: ", "", title))) %>%
  filter(title %in% sectors) %>%
  arrange(title)
s_nom$title 

s_nom$id



tsm_nom <- lapply(s_nom$id, function(id) {
  y <- fredr(
    series_id = id,
    observation_start = as.Date("2005-01-01"),
    frequency = "q"
    # observation_end = as.Date("2000-01-01")
  )
  xts(x = y$value, order.by = y$date)
  ts(y$value, start = 2005, frequency = 4)
  
}
) %>% do.call(cbind, .)
colnames(tsm_nom) <- gsub("value added by industry: ", "", s_nom$title)
tsm_nom
Reduce("+", as.list(tsm_nom))
