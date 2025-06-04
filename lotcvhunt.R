#' Hunting Predator-Prey Model
#'
#' Computes prey and predator population changes with hunting.
#'
#' @param t Time (from ODE solver).
#' @param pop Named vector of populations: prey and pred.
#' @param pars Named list of parameters.
#'
#' @return List of population change rates for prey and predator.
#' @export
#'
#' @examples

lotvhunt <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    
    hunt <- ifelse(prey >= prey_min, rhunt * prey^1.2, 0)
    hunt <- min(hunt, prey)
    
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - hunt
    dpred <- eff * alpha * prey * pred - pmort * pred
    
    return(list(c(dprey, dpred)))
  })
}