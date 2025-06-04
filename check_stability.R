# Stability check function
check_stability <- function(rhunt, prey_min, base_pars, init_pop, times) {
  pars <- c(base_pars, rhunt = rhunt, prey_min = prey_min)
  sim <- ode(func = lotvhunt, y = init_pop, times = times, parms = pars)
  
  final_prey <- tail(sim[, "prey"], 1)
  final_pred <- tail(sim[, "pred"], 1)
  
  return(final_prey >= 200 & final_pred >= 20)
}