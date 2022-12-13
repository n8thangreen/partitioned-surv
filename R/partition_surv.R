
#' Fit a partitioned survival model
#' 
#' order of fit object is from top to bottom i.e.
#' least nested survival curve to most e.g.
#' OS, PFS
#' 
#' @param ... Fitted flexsurvreg obj
#' @param time numeric vector of time to estimate probabilities
#' @return data frame of times and probabilities for each state
#' 
#' @importFrom blendR make_surv
#' @export
#'  
partition_surv <- function(...,
                           time = NULL) {
  
  fits <- list(...)
  
  ##TODO:
  # largest subset of all fit times
  # at moment assumes all times the same
  
  surv <- NULL
  
  for (i in seq_along(fits)) {
    surv <- cbind(surv, make_surv(fits[[i]], t = time, nsim = 1))
  }
  
  surv <- cbind(1, surv, 0)
  
  prob <- apply(surv, 1, FUN = function(x) rev(diff(rev(x))))
  
  ##TODO:
  # constrain positive?
  
  time <- if (is.null(time)) 0:(ncol(prob) - 1) else time
  
  data.frame(time = time,
             t(prob))
}
