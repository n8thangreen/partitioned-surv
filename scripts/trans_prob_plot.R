
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

# original ONS life table
lifetable <- read.delim(file = "data/lifetable.txt", sep = "",
                    skip = 2, strip.white = TRUE, blank.lines.skip = TRUE) |> 
  filter(Year == 2016) 

# lt_tpDn <- 1 - exp(-lifetable$mx)
lt_tpDn <- lifetable$qx

# png(filename = here::here("plots", "trans_prob_plot.png"))
matplot(t(fda_tpDn_wide[c("1922", "1991", "2016", "2041"), ]))
lines(1:100, dat, type = "o")
lines(0:110, lt_tpDn, type = "o", col = "grey")
# dev.off()

dat <- fda_tpDn_wide
save(dat, file = "data/fda_tpDn_wide.RData")

dat <- fda_tpDn_wide["2041", ]
save(dat, file = "data/fda_tpDn_wide_2041.RData")


############
# CI bounds

fda_mx_lower <-
  read.delim(here::here("data", "mx_lower_bounds.txt"), sep = " ")

fda_mx_upper <-
  read.delim(here::here("data", "mx_upper_bounds.txt"), sep = " ")

fda_tpDn_upper <- 1 - exp(-fda_mx_upper)
fda_tpDn_lower <- 1 - exp(-fda_mx_lower)

matplot(t(fda_tpDn_wide[c("1921", "2030", "2045"), ]), type = "l")
matplot(t(fda_tpDn_upper[c("1921", "2030", "2045"), ]), type = "l", add = TRUE)
matplot(t(fda_tpDn_lower[c("1921", "2030", "2045"), ]), type = "l", add = TRUE)

# single year
matplot(t(fda_tpDn_wide[c("2045"), ]), type = "l")
matplot(t(fda_tpDn_upper[c("2045"), ]), type = "l", add = TRUE, lty = 2)
matplot(t(fda_tpDn_lower[c("2045"), ]), type = "l", add = TRUE, lty = 2)

dat <- fda_tpDn_upper["2045", ]
save(dat, file = "data/fda_tpDn_upper_2041.RData")

dat <- fda_tpDn_lower["2045", ]
save(dat, file = "data/fda_tpDn_lower_2041.RData")
