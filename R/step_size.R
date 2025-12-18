#'  Step size of charger by Runge Kutta 4
#'
#' retrieves the characteristics of the charger components and calculates the maximum pitch for stability and precision
#'
#' @param R_charge charge resistance
#' @param R_L inductance resistance
#' @param L inductance
#' @param C capacitor capacity
#'
#' @returns  time space maximum for stability and precision
#' @export
#'
#' @examples
#' dt_max <- step_size(R_charge = 2.5,R_L = 0.1,L = 47e-6,C = 100e-6)
step_size <- function( L, C, R_L, R_charge) {

  if (R_L <= 0){
    print("R_L must be more than 0. Utilisation to R_L = 0.01 by fault")
    R_L <- 0.01
  }
  tau_L <- L / R_L
  tau_C <- R_charge * C

  tau_min <- min(tau_L, tau_C)

  dt_max <- tau_min / 20

  period_switch <- 1e-5
  dt_switch <- period_switch / 10

  if (dt_max > dt_switch){
    cat(sprintf("The step size for stability is %.3e s \n",dt_max))
    cat(sprintf("for more stability and pecision uses a maximum of %.3e s \n",dt_switch))
    return(dt_switch)
  }else{
    cat(sprintf("The step size for a minimum stability is %.3f s",dt_max))
    return(dt_max)
  }
}
