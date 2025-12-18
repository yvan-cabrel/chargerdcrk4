#'   Full simulation
#'
#'   Performs complete analysis and visualization of the DC charger's output current and voltage
#'
#' @importFrom utils install.packages
#'
#' @param V_in input voltage
#' @param V_out_target output voltage
#' @param L inductance
#' @param C capacitor capacity
#' @param R_L inductance resistance
#' @param R_charge charge resistance
#' @param t_final time after which iterations stop
#' @param i_L_0 initial current of inductance
#' @param v_C_0 initial voltage of capacitor
#'
#' @returns return the graphs and display the analyses
#' @export
#'
#' @examples
#' full_simulation ( 12.0, 5.0, 47e-6, 100e-6, 0.1, 2.5, 2e-2, 0, 0 )
full_simulation <- function( V_in, V_out_target, L, C, R_L, R_charge, t_final, i_L_0, v_C_0 ) {
  cat("========================================\n")
  cat("SIMULATION BUCK CONVERTER with RK4\n")
  cat("========================================\n\n")

  cat("Circuit parameters :\n")
  cat(sprintf("  V_in = %.1f V\n", V_in))
  cat(sprintf("  V_out target = %.1f V\n", V_out_target))
  cat(sprintf("  L = %.4e H\n", L * 1e6))
  cat(sprintf("  C = %.4e F\n", C * 1e6))
  cat(sprintf("  R_L = %.2f ohm\n", R_L))
  cat(sprintf("  R_charge = %.2f ohm (%.1f A)\n\n", R_charge, V_out_target/R_charge))

  duty <- duty_cycle(V_in, V_out_target)
  cat(sprintf("Duty cycle : %.3f (%.1f%%)\n\n", duty, duty * 100))


  dt_max <- step_size(R_charge, R_L, L, C)
  dt_max
  dt <- dt_max

  cat(sprintf("\nTime space recommended : %.2e s\n\n", dt))

  cat("simulation in progress ...\n")

  results <- buck_converter_rk4(
    V_in = V_in,
    duty = duty,
    R_charge = R_charge,
    R_L = R_L,
    L = L,
    C = C,
    dt = dt,
    t_final = t_final,
    i_L_0 = i_L_0 ,
    v_C_0 = v_C_0
    )

  cat("Simulation end !\n\n")

  cat("Generation of graphiques...\n")

  p1 <- voltage_visualisation(results)
  p2 <- current_visualisation(results)

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Le package 'patchwork' est requis pour cette fonction.")
  }

  analyse_performances(V_out_target, results)

  return(patchwork::wrap_plots(p1, p2, ncol = 1))
}
