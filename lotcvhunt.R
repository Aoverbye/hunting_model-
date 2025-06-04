#' Hunting Model Function
#'
#' @param t 
#' @param pop 
#' @param pars 
#'
#' @return
#' @export
#'
#' @examples
lotvhunt <- function(t, pop, pars) {
  
  with(as.list(c(pars, pop)), {
    
    hunt <- ifelse(prey >= prey_min, rhunt * prey, 0)
    
    hunt <- min(hunt, prey)  # prevent overharvest
    
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - hunt
    
    dpred <- eff * alpha * prey * pred - pmort * pred
    
    return(list(c(dprey, dpred)))
    
  })
  
}
