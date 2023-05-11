
# Markov model analysis
# with fda data


library(dplyr)


t_names <- c("without_drug", "with_drug")
n_treatments <- length(t_names)

s_names  <- c("Asymptomatic_disease", "Progressive_disease", "Dead")
n_states <- length(s_names)

n_cohort <- 1000
n_cycles <- 20
Initial_age <- 50

n_trials <- 400

# effect <- 0.1

all_pars <- read.csv(file = "data/dsa_inputs.csv")


for (j in 1:nrow(all_pars)) {
  
  pars <- all_pars[j, ] |> as.list()
  
  # convert fixed value variable to function
  for (x in names(pars)) {
    eval(parse(text = glue::glue("{x} <- \\() pars${x}")))
  }
  
  oDr <- 0.06; cDr <- 0.06
  tpDn <- 0.0379  # over 65 year old
  
  # cAsymp <- function() rnorm(1, 500, 127.55)
  # cDeath <- function() rnorm(1, 1000, 255.11)
  # cProg  <- function() rnorm(1, 3000, 510.21)
  # cDrug  <- function() rnorm(1, 1000, 102.04)
  # # tpDcm  <- function() rbeta(1, 29, 167)
  # tpDcm  <- function() rbeta(1, 1, 167)
  # tpProg <- function() rbeta(1, 15, 1506)
  # uAsymp <- function() rbeta(1, 69, 4)
  # uProg  <- function() rbeta(1, 24, 8)
  
  effect <- function() rnorm(1, 0.5, 0.051)
  
  
  # Define cost and QALYs as functions
  
  state_c_matrix <- function() {
    matrix(c(cAsymp(), cProg(), 0,            # without drug
             cAsymp() + cDrug(), cProg(), 0), # with drug
           byrow = TRUE,
           nrow = n_treatments,
           dimnames = list(t_names,
                           s_names))
  }
  
  state_q_matrix <- function() {
    matrix(c(uAsymp(), uProg(), 0,  # without drug
             uAsymp(), uProg(), 0), # with drug
           byrow = TRUE,
           nrow = n_treatments,
           dimnames = list(t_names,
                           s_names))
  }
  
  trans_c_matrix <- function() {
    matrix(c(0, 0, 0,         # Asymptomatic_disease
             0, 0, cDeath(),  # Progressive_disease
             0, 0, 0),        # Dead
           byrow = TRUE,
           nrow = n_states,
           dimnames = list(from = s_names,
                           to = s_names))
  }
  
  
  costs <- matrix(NA, nrow = n_trials, ncol = n_treatments,
                  dimnames = list(NULL, t_names))
  qalys <- matrix(NA, nrow = n_trials, ncol = n_treatments,
                  dimnames = list(NULL, t_names))
  
  # mortality point values
  data_filename <- "fda_tpDn_wide_2041.RData"
  # data_filename <- "fda_tpDn_lower_2041.RData"
  # data_filename <- "fda_tpDn_upper_2041.RData"
  
  for (i in 1:n_trials) {
    print(glue::glue("trial number: {i}"))
    ce_res <- ce_markov(start_pop = c(n_cohort, 0, 0),
                        n_treat = 2,
                        n_cycles = n_cycles, 
                        init_age = Initial_age,
                        lambda = 0,
                        # p_mortality_wide(filename = data_filename,                    # mortality point values
                        #                  baseyear = 2041),
                        # p_mortality_long(),
                        # p_mortality_williams,    # 2016
                        p_mortality_wide(filename = "fda_tpDn_wide_2041.RData",         # mortality prob uncertainty
                                         filename_upper = "fda_tpDn_upper_2041.RData",
                                         filename_lower = "fda_tpDn_lower_2041.RData"),
                        state_c_matrix(),
                        trans_c_matrix(),
                        state_q_matrix())
    
    costs[i, ] <- ce_res$total_costs
    qalys[i, ] <- ce_res$total_QALYs
  }
}

########
# plots

c_incr <- costs[, "with_drug"] - costs[, "without_drug"]
q_incr <- qalys[, "with_drug"] - qalys[, "without_drug"]

wtp <- 30000

plot(x = q_incr/n_cohort, y = c_incr/n_cohort, col = "blue",
     xlim = c(-1, 2),
     # xlim = c(-0.5, 1),  # 80 y/o
     ylim = c(0, 15e3),
     # ylim = c(0, 10e3),  # 80 y/o
     pch = 16, cex = 1.2,
     col = "grey",
     xlab = "QALY difference",
     ylab = paste0("Cost difference (", enc2utf8("\u00A3"), ")"),
     frame.plot = FALSE)
points(x = mean(q_incr, na.rm = TRUE)/n_cohort, y = mean(c_incr, na.rm = TRUE)/n_cohort,
       col = "red", pch = 16, cex = 1.5)
abline(a = 0, b = wtp, lwd = 2)  # willingness-to-pay threshold

