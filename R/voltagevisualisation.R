#' Voltage visualisation
#'
#'  visualise the output voltage
#'
#' @importFrom utils install.packages
#' @importFrom ggplot2 aes geom_line element_text
#'
#' @param results  table resulting from the function  rk4buckconverter
#'
#' @returns Voltage visualisation
#' @export
#'
#' @examples
#' results <- data.frame(
#'    times = seq(0, 0.01, length.out = 100),
#'    i_L = runif(100, min = 0, max = 5),
#'    V_out = runif(100, min = 0, max = 12)
#' )
#' voltagevisualisation(results)
voltagevisualisation <- function(results) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package 'ggplot2' est requis pour cette fonction.")
  }

  return(ggplot2::ggplot(results, aes(x = times * 1000, y = V_out)) +
           ggplot2::geom_line(color = "blue", linewidth = 0.8) +
           ggplot2::labs(
             title = "Output voltage of Buck Converter",
             x = "Times (ms)",
             y = "Voltage V_out (V)"
           ) +
           ggplot2::theme(
             plot.title = element_text(hjust = 0.5 , face = "bold", size = 14),
             axis.title = element_text(hjust = 0.8 , face = "bold",size = 11)
           )
  )
}
