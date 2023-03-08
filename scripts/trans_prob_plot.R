
# plot the mortality transition probabilities
# against age for different life tables


# williams paper
load(here::here("data", "tpDn_wide.RData"))

# FDA data
# mx: average mortality rate over year
fda_mx_wide <-
  read.delim(here::here("data", "mx_2021-2045_transformed.txt"), sep = " ")

fda_tpDn_wide <- 1 - exp(-fda_mx_wide)
colnames(fda_tpDn_wide) <- 0:110

png(filename = here::here("plots", "trans_prob_plot.png"))
matplot(t(fda_tpDn_wide[c("1922", "1971", "2021", "2041"), ]))
lines(1:100, dat, type = "o")
dev.off()

dat <- fda_tpDn_wide
save(dat, file = "data/fda_tpDn_wide.RData")

dat <- fda_tpDn_wide["2041", ]
save(dat, file = "data/fda_tpDn_wide_2041.RData")

