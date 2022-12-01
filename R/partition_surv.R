
#' Fit a partitioned survival model
#' 
#' @param fit_pfs flexsurvreg obj fitting pfs
#' @param fit_os flexsurvreg obj fitting os
#' @param time numeric vector of time to estimate probabilities
#' @return list w/ one entry of a data frame w/ probabilities
#'    associated w/ stable ,prog and dead.
#' 
#' @importFrom blendR make_surv
#' @export
#'  
partition_surv <- function(fit_pfs,
                           fit_os,
                           time = NULL, ...) {
  
  pfs_surv <- make_surv(fit_pfs, t = time, nsim = 1)
  os_surv <- make_surv(fit_os, t = time, nsim = 1)
  
  prog <- os_surv - pfs_surv
  prog[prog < 0] <- 0                 
  stable <- pfs_surv          
  dead <- 1 - os_surv
  
  data.frame(time = time,
             stable = stable,
             prog = prog,
             dead = dead)
}

