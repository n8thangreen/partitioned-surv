
# mortality input data
# can be in different formats


#' probability of death look-up
#' from Williams paper
#' @export
#' 
p_mortality_williams <- function(age, ...) {
  tpDn_lookup <-
    c("(34,44]" = 0.0017,
      "(44,54]" = 0.0044,
      "(54,64]" = 0.0138,
      "(64,74]" = 0.0379,
      "(74,84]" = 0.0912,
      "(84,100]" = 0.1958,
      "(100,110]" = 0.1958)   # not in original paper
  
  age_grp <- cut(age, breaks = c(34,44,54,64,74,84,100,110))
  tpDn_lookup[age_grp]
}


#' probability of death wide look-up table
#' 
#' to use with functional data analysis
#' @export
#' 
p_mortality_wide <- function(filename = "tpDn_wide.RData",
                             baseyear = NA) {
  load(here::here(glue::glue("data/{filename}")))
    
  if (is.na(baseyear)) baseyear <- 1
  dat_yr <<- dat[as.character(baseyear), ]
  
  function(age)
    dat_yr[as.character(age)]
}


#' probability of death long look-up table
#' @export
#' 
p_mortality_long <- function(filename = "tpDn_long.RData",
                             baseyear = NA) {
  load(here::here(glue::glue("data/{filename}")))
  year_included <<- "year" %in% names(dat)
  
  function (age_yr) {
    if (year_included)
      return(dplyr::filter(dat, year == baseyear, age == age_yr)$tpDn)
    else
      return(dplyr::filter(dat, age == age_yr)$tpDn)
  }
}
