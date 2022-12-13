
# example 
# from Williams (2016, 2017)

library(dplyr)


# overall survival
# using life tables

dat <- read.delim(file = "raw-data/bltper_1x1.txt", sep = "")

os <- 
  dat |> filter(Year == 2020) |>
  mutate(s = lx/100000) |> 
  pull()


# progression-free survival

n_sample <- 10
log_scale <- rnorm(n_sample, mean = 1.237, sd = 0.06)
log_shape <- rnorm(n_sample, mean = 0.310, sd = 0.051)

surv <- list()

for (i in 1:n_sample) {
  surv[[i]] <- pweibull(q = seq(0,10,0.1),
                        shape = exp(log_shape[i]),
                        scale = exp(log_scale[i]),
                        lower.tail = FALSE)
}

pfs <- do.call(cbind, surv)

# to test
pfs <- rowMeans(pfs)

os <- os[1:101]
os[101] <- pfs[101]

#
plot(os, type = "l", col = "red")
lines(pfs, type = "l", col = "blue")


## how to determine the overall survival?
## e.g. take the curve from the paper (2010?) and adjust life table
## for a hazard ratio/ratio RMST and then use this for future curves?

## what cohort age distribution?
## do the weighted average like in MCM analysis?


#
ps_res <- partition_surv(os, pfs)


matplot(ps_res$time, ps_res[,-1], type = "l", lty = 1, ylab = "probability")

save(ps_res, file = "data/ps_res.RData")

## check
# pfs.surv <- summary(fit.pfs, ci = FALSE)[[1]]$est
# 
# library(survHE)
# blendR::make_surv(fit.pfs, t = summary(fit.pfs, ci = FALSE)[[1]]$time)

# mean Costs and QALYs per cycle

c_S <- 
c_P <- 
c_D <- 

u_S <- 
u_P <- 
u_D <- 
  
# expected costs
costs <- ps_res %*% c(c_S, c_P, c_D)

# expected QALYs
qalys <- ps_res %*% c(u_S, u_P, u_D)

# discounting
d <- 0.035
disc <- 1/(1 + d)^times

c_total <-  t(costs) %*% disc
q_total <-  t(qalys) %*% disc

ce_res <- c(c_total, q_total)

save(pce_res, file = "data/pce_res.RData")
