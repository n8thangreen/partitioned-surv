
# williams paper
load("data/tpDn_wide.RData")

fda_mx_wide <- read.delim("data/mx_2021-2045_transformed.txt", sep = " ")

fda_tpDn_wide <- 1 - exp(-fda_mx_wide)
colnames(fda_tpDn_wide) <- 0:110

png(filename = "plots/trans_prob_plot.png")
matplot(t(fda_tpDn_wide[c(1,50,100,120), ]))
lines(1:100, dat, type = "o")
dev.off()

dat <- fda_tpDn_wide[120, ]

save(dat, file = "data/fda_tpDn_wide.RData")

