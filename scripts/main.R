
#### Fit partitioned survival model #### 

partsurv <- function(fit.pfs, fit.os, title = "trt", time = times){
  # Input
  # fit.pfs: flexsurv obj fitting pfs
  # fit.os: flexsurv obj fitting os
  # title:
  # time = numeric vector of time to estimate probabilities
  # output:
  #  res a list w/ one entry of a data frame w/ probabilities associated w/ stable ,prog and dead.
  
  pfs.surv <- summary(fit.pfs, t = time, ci = F)[[1]]$est
  os.surv <- summary(fit.os, t = time, ci = F)[[1]]$est
  prog                 <- os.surv - pfs.surv          # estimate the probability of remaining in the progressed state
  prog[prog < 0]       <- 0                           # in cases where the probability is negative replace with zero
  stable               <- pfs.surv                    # probability of remaining stable
  dead                 <- 1 - os.surv                 # probability of being dead
  trace <- data.frame(stable = stable, prog = prog, dead = dead)
  res   <- list(trace = trace)
  res   <- list(trace = trace)
  return(res)
}

