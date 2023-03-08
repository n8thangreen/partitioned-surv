
#'
sim_pop <- function(n_cycles, age,
                    trans_c_matrix,
                    p_matrix, pop, trans, i) {
  
  for (j in 2:n_cycles) {
    p_matrix <- p_matrix_cycle(p_matrix, age, j - 1)
    pop[, cycle = j, i] <-
      pop[, cycle = j - 1, i] %*% p_matrix[, , i]
    trans[, cycle = j, i] <-
      pop[, cycle = j - 1, i] %*% (trans_c_matrix * p_matrix[, , i])
    age <- age + 1
  }
  
  list(pop = pop[, , i],
       trans = trans[, , i])
}
