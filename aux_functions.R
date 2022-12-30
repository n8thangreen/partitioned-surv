pb_score <- function(y, z, tau){
  # y = actual value; q = quantile forecasted value; tau = quantile level
  indicator <- ifelse(y - z < 0, 1, 0)
  score <- (y - z) * (tau - indicator)
  return(score)
}

pinball.loss <- function(true, UB, LB, conf){
  Linf <- pb_score(true, LB, (1 - conf)/2)
  Lsup <- pb_score(true, UB, 1 - (1 - conf)/2)
  sL = mean(Linf) + mean(Lsup)
  return(sL)
}

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

invgen <- function(A, tol=1e-05){ 
  spa <- eigen(A, symmetric = T)
  p <- ncol(A)
  ind <- (1:p)[spa$values > tol] # eigen values that are not zero
  #  print(paste("[@invgen]:", length(ind), "out of", p, ">0"))
  U <- spa$vectors
  U <- U[, ind]
  if(length(ind) > 1) {
    B <- U %*% diag(1/spa$values[ind]) %*% t(U)
  } else {
    B <- 1 / spa$values[ind] * as.matrix(U) %*% 
      t(as.matrix(U))
  }
  B
}

eltwo <- function(grid, fcast, boot){
  nn = nrow(boot)
  dist = c()
  for (i in 1:nn){
    dist[i]<-sqrt(pracma::trapz(grid, (fcast - boot[i,])^2))
  }
  return(dist)
}
