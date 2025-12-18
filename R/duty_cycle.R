#' duty cycle
#'
#' Evaluate the duty cycle to obtain an output voltage close to the desired voltage
#'
#' @param V_in  input voltage
#' @param V_out output voltage
#'
#' @returns duty cycle : The potion's time during the input voltage is active for a period
#' @export
#'
#' @examples duty = duty_cycle(12,5)
duty_cycle <- function(V_in, V_out) {

  if (V_out >= V_in) {
    stop("Error : Vout must be less than Vin for a Buck converter")
  }

  duty <- V_out / V_in
  duty <- min(duty, 0.95)

  return(duty)
}
