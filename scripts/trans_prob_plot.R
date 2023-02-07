
load("C:/Users/Nathan/Documents/R/partitionedsurv/data/tpDn_wide.RData")

fda_dat <- read.delim("data/mx_2021-2045_transformed.txt", sep = " ")

fda_dat <- 1 - exp(-fda_dat)
colnames(fda_dat) <- 0:110

png(filename = "plots/trans_prob_plot.png")
matplot(t(fda_dat[c(1,50,100,120), ]))
lines(1:100, dat, type = "o")
dev.off()

fda_tpDn_wide <- fda_dat[120, ]

save(fda_tpDn_wide, file = "data/fda_tpDn_wide.RData")

