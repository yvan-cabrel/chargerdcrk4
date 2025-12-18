#' stability of runge kutta 4
#'
#' the equation is of the form  a y'' + b y' + c y = f(t)
#' This function displays the maximum step size for stability and the maximum step size for minimum accuracy
#' Note: a divergent solution does not mean that Runge Kutta cannot approximate the solution
#'
#' @param a coefficient of the second derivative
#' @param b coefficient of the first derivative
#' @param c coefficient of the solution function
#'
#' @returns step size maximum for stability and precision
#' @export
#'
#' @examples
#' stability(1,1,2)
stability <- function (a,b,c){
  if (a == 0){
    dividand <- c
    divisor <- b
  }else{
    dividand <- b
    divisor <- a
  }
  tau <- -dividand / divisor
  hmax <- NA
  if (tau < 0){
    hmax <- 2.7852935634/abs(tau)
    pmax <- 1/abs(tau)

  }else{
    pmax <- 1/tau
  }
  cat(sprintf("step size for stability (point of convergence) : %f \n",hmax))
  cat(sprintf("step size for precision : %f \n",pmax))
  return(pmax)
}
