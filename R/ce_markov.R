
#' Cost-effectiveness Markov model
#'
#' @param start_pop 
#' @param n_treat 
#' @param p_mortality 
#' @param state_c_matrix 
#' @param trans_c_matrix 
#' @param state_q_matrix 
#' @param n_cycles 
#' @param init_age 
#' @param lambda Proportion to Progressive disease. How to redistribute individuals
#'    who no longer transition to death when changing from Williams probabilities
#'    to alternative. They can either go to Progressive disease or stay in
#'    Asymptomatic disease. 0<= lambda <= 1.
#' @param s_names 
#' @param t_names 
#'
#' @return List
#' @export
#'
ce_markov <- function(start_pop,
                      n_treat,
                      n_cycles = 46,
                      init_age = 55,
                      lambda = 0.5,
                      p_mortality,
                      state_c_matrix,
                      trans_c_matrix,
                      state_q_matrix,
                      s_names = NULL,
                      t_names = NULL) {
  
  n_states <- length(start_pop)
  
  pop <- array(data = NA,
               dim = c(n_states, n_cycles, n_treat),
               dimnames = list(state = s_names,
                               cycle = NULL,
                               treatment = t_names))
  trans <- array(data = NA,
                 dim = c(n_states, n_cycles, n_treat),
                 dimnames = list(state = s_names,
                                 cycle = NULL,
                                 treatment = t_names))
  
  for (i in 1:n_states) {
    pop[i, cycle = 1, ] <- start_pop[i]
  }
  
  cycle_empty_array <-
    array(NA,
          dim = c(n_treat, n_cycles),
          dimnames = list(treatment = t_names,
                          cycle = NULL))
  
  cycle_state_costs <- cycle_trans_costs <- cycle_empty_array
  cycle_costs <- cycle_QALYs <- cycle_empty_array
  LE <- LYs <- cycle_empty_array    # life expectancy; life-years
  cycle_QALE <- cycle_empty_array   # quality-adjusted life expectancy
  
  total_costs <- setNames(rep(NA, n_treat), t_names)
  total_QALYs <- setNames(rep(NA, n_treat), t_names)
  
  for (i in 1:n_treat) {
    
    age <- init_age
    
    for (j in 2:n_cycles) {
      
      # pass in functions for random sample
      # rather than fixed values
      p_matrix <- p_matrix_cycle(age, j - 1,
                                 p_mortality,
                                 tpProg = tpProg(),
                                 tpDcm = tpDcm(),
                                 effect = effect(),
                                 lambda = lambda)
      
      # matrix multiplication
      pop[, cycle = j, treatment = i] <-
        pop[, cycle = j - 1, treatment = i] %*% p_matrix[, , treatment = i]
      
      trans[, cycle = j, treatment = i] <-
        pop[, cycle = j - 1, treatment = i] %*% (trans_c_matrix * p_matrix[, , treatment = i])
      
      age <- age + 1
    }
    
    cycle_state_costs[i, ] <-
      (state_c_matrix[treatment = i, ] %*% pop[, , treatment = i]) * 1/(1 + cDr)^(1:n_cycles - 1)
    
    cycle_trans_costs[i, ] <-
      (c(1,1,1) %*% trans[, , treatment = i]) * 1/(1 + cDr)^(1:n_cycles - 2)
    
    cycle_costs[i, ] <- cycle_state_costs[i, ] + cycle_trans_costs[i, ]
    
    LE[i, ] <- c(1,1,0) %*% pop[, , treatment = i]
    
    LYs[i, ] <- LE[i, ] * 1/(1 + oDr)^(1:n_cycles - 1)
    
    cycle_QALE[i, ] <-
      state_q_matrix[treatment = i, ] %*%  pop[, , treatment = i]
    
    cycle_QALYs[i, ] <- cycle_QALE[i, ] * 1/(1 + oDr)^(1:n_cycles - 1)
    
    total_costs[i] <- sum(cycle_costs[treatment = i, -1])
    total_QALYs[i] <- sum(cycle_QALYs[treatment = i, -1])
  }
  
  list(pop = pop,
       cycle_costs = cycle_costs,
       cycle_QALYs = cycle_QALYs,
       total_costs = total_costs,
       total_QALYs = total_QALYs)
}
