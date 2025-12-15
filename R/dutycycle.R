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
#' @examples duty = dutycycle(12,5)
dutycycle <- function(V_in, V_out) {

  if (V_out >= V_in) {
    stop("Error : Vout must be less than Vin for a Buck converter")
  }

  d <- V_out / V_in
  d <- min(d, 0.95)

  return(d)
}
