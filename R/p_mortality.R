
#' from Williams paper
#'
p_mortality_williams <- function(age, ...) {
  tpDn_lookup <-
    c("(34,44]" = 0.0017,
      "(44,54]" = 0.0044,
      "(54,64]" = 0.0138,
      "(64,74]" = 0.0379,
      "(74,84]" = 0.0912,
      "(84,100]" = 0.1958)
  
  age_grp <- cut(age, breaks = c(34,44,54,64,74,84,100))
  tpDn_lookup[age_grp]
}


#' functional data analysis
#' 
p_mortality_fda <- function(age, year) {
  dat <- read.delim(here::here("data/"))
  
  dat[dat$year == year, as.character(age)]
}
