
# example data from flexsurv package

ovarian$futime2 <- ovarian$futime + 100

fit.pfs <- flexsurvreg(formula = Surv(futime, fustat) ~ 1,
                       data = ovarian, dist = "weibull")

fit.os <- flexsurvreg(formula = Surv(futime2, fustat) ~ 1,
                      data = ovarian, dist = "weibull")

ps_res <- partition_surv(fit.pfs, fit.os)

matplot(ps_res, type = "l", lty = 1)

save(ps_res, file = "data/ps_res.RData")

# mean costs and QALYs per cycle

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
