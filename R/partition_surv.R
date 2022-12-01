
#' Fit a partitioned survival model
#' 
#' @param fit.pfs flexsurv obj fitting pfs
#' @param fit.os flexsurv obj fitting os
#' @param time numeric vector of time to estimate probabilities
#' @return list w/ one entry of a data frame w/ probabilities
#'    associated w/ stable ,prog and dead.
#' 
partition_surv <- function(fit.pfs,
                           fit.os,
                           time = NULL, ...) {
  
  pfs.surv <- summary(fit.pfs, t = time, ci = FALSE)[[1]]$est
  os.surv <- summary(fit.os, t = time, ci = FALSE)[[1]]$est
  
  prog <- os.surv - pfs.surv
  prog[prog < 0] <- 0                 
  stable <- pfs.surv          
  dead <- 1 - os.surv
  
  data.frame(stable = stable,
             prog = prog,
             dead = dead)
}

