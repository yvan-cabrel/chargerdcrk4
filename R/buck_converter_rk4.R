#'   buck converter with Runge kutta 4
#'
#'   We use Runge Kutta 4 to model the buck converter for simulations
#'
#' @param V_in input voltage
#' @param d duty cycle
#' @param R_charge charge resistance
#' @param R_L inductance resistance
#' @param L inductance
#' @param C capacitor capacity
#' @param dt time space : by how much does the time change at each iteration
#' @param t_final time after which iterations stop
#' @param i_L_0 initial current of inductance
#' @param v_C_0 initial voltage of capacitor
#'
#' @returns a 3 columns table consisting of  times, i_L, V_out
#' @export
#'
#' @examples
#' results <- buck_converter_rk4(V_in = 12,
#'   d = 0.417,R_charge = 2.5,
#'   R_L = 0.1,L = 47e-6,
#'   C = 100e-6,
#'   dt = 1e-5,
#'   t_final = 0.02
#' )
buck_converter_rk4 <- function(V_in, d, L, C, R_L, R_charge, dt, t_final, i_L_0 = 0, v_C_0 = 0) {

  equations_buck <- function(t, y) {
    i_L <- y[1]
    v_C <- y[2]

    di_L_dt <- (d * V_in - v_C - R_L * i_L) / L

    dv_C_dt <- (i_L - v_C / R_charge) / C

    return(c(di_L_dt, dv_C_dt))
  }


  t <- 0
  times <- c(0)
  i_L <- c(i_L_0)
  v_C <- c(v_C_0)
  y <- c(i_L_0, v_C_0)


  while(t <= t_final){

    k1 <- equations_buck(t, y)
    k2 <- equations_buck(t + dt/2, y + dt/2 * k1)
    k3 <- equations_buck(t + dt/2, y + dt/2 * k2)
    k4 <- equations_buck(t + dt, y + dt * k3)

    y <- y + (dt / 6) * (k1 + 2*k2 + 2*k3 + k4)
    t <- t + dt

    times <- c(times,t)
    i_L <- c(i_L, y[1])
    v_C <- c(v_C, y[2])
  }

  results <- data.frame(
    times = times,
    i_L = i_L,
    V_out = v_C
  )

  return(results)
}
