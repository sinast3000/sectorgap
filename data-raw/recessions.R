
# define recession periods

recessions_us <- as.data.frame(
  matrix(
    c(1960+4/12, 1961+2/12,
      1969+12/12, 1970+11/12,
      1973+11/12, 1975+03/12,
      1980+01/12, 1980+07/12,
      1981+07/12, 1982+11/12,
      1990+07/12, 1991+03/12,
      2001+03/12, 2001+11/12,
      2007+12/12, 2009+06/12,
      2020+2/12,  2020+4/12),
    ncol = 2, 
    byrow = T
  )
)

recessions_ch <- as.data.frame(
  matrix(
    c(1982.25, 1983.25,
      1991.00, 1993.25,
      2002.25, 2003.50,
      2008.75, 2009.50,
      2020.00, 2021.00),
    ncol = 2, 
    byrow = T
  )
)
    
colnames(recessions_ch)  <- colnames(recessions_us) <- c("start", "end") 

# save package data
usethis::use_data(recessions_ch, internal = FALSE, overwrite = TRUE)
usethis::use_data(recessions_us, internal = FALSE, overwrite = TRUE)



