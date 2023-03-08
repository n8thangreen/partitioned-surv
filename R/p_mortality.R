
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
                             filename_upper = NA,
                             filename_lower = NA,
                             baseyear = NA) {
  
  if (is.na(baseyear)) baseyear <- 1

  load(here::here(glue::glue("data/{filename}")))
  dat_yr <<- dat[as.character(baseyear), ]
  
  # sample probability
  if (!is.na(filename_upper) && !is.na(filename_lower)) {
    load(here::here(glue::glue("data/{filename_upper}")))
    dat_upper <<- dat[as.character(baseyear), ]
    
    load(here::here(glue::glue("data/{filename_lower}")))
    dat_lower <<- dat[as.character(baseyear), ]
    
    return(
      function(age) {
        upper <- as.numeric(dat_upper[as.character(age)])
        mu <- as.numeric(dat_yr[as.character(age)])
        stdev <- (upper - mu)/1.96
        s <- rnorm(1, mu, stdev)
        min(1, max(0, s))
      })
  }
  
  # point value
  function(age)
    as.numeric(dat_yr[as.character(age)])
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
