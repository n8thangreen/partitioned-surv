
p_matrix_cycle <- function(age, cycle,
                           p_mortality,
                           tpProg = 0.01,
                           tpDcm = 0.15,
                           effect = 0.5,
                           lambda = 0.5) {
  
  tpDn_williams <- p_mortality_williams(age)
  tpDn <- p_mortality(age)
  tpDn_diff <- tpDn_williams - tpDn
  
  state_names <- c("Asymptomatic_disease", "Progressive_disease", "Dead")
  drug_names <- c("without_drug", "with_drug")
  
  p_matrix <- array(0, dim = c(3,3,2),
                    dimnames = list(state_names, state_names, drug_names))
  
  # Matrix containing transition probabilities for without_drug
  p_matrix["Asymptomatic_disease", "Progressive_disease", "without_drug"] <- tpProg*cycle + lambda*tpDn_diff
  p_matrix["Asymptomatic_disease", "Dead", "without_drug"] <- tpDn
  p_matrix["Asymptomatic_disease", "Asymptomatic_disease", "without_drug"] <- 1 - (tpProg*cycle + lambda*tpDn_diff) - tpDn
  p_matrix["Progressive_disease", "Dead", "without_drug"] <- tpDcm + tpDn
  p_matrix["Progressive_disease", "Progressive_disease", "without_drug"] <- 1 - tpDcm - tpDn
  p_matrix["Dead", "Dead", "without_drug"] <- 1
  
  # Matrix containing transition probabilities for with_drug
  p_matrix["Asymptomatic_disease", "Progressive_disease", "with_drug"] <- tpProg*(1 - effect)*cycle + lambda*tpDn_diff
  p_matrix["Asymptomatic_disease", "Dead", "with_drug"] <- tpDn
  p_matrix["Asymptomatic_disease", "Asymptomatic_disease", "with_drug"] <-
    1 - (tpProg*(1 - effect)*cycle + lambda*tpDn_diff) - tpDn
  p_matrix["Progressive_disease", "Dead", "with_drug"] <- tpDcm + tpDn
  p_matrix["Progressive_disease", "Progressive_disease", "with_drug"] <- 1 - tpDcm - tpDn
  p_matrix["Dead", "Dead", "with_drug"] <- 1
  
  return(p_matrix)
}
