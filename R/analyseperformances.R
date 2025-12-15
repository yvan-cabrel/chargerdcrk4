#'  Analyse performances
#'
#' @importFrom stats sd
#'
#' @param V_out_target desired output voltage
#' @param results table resulting from the function  rk4buckconverter
#'
#' @returns the entire analysis
#' @export
#'
#' @examples
#'  results <- data.frame(
#'    times = seq(0, 0.01, length.out = 100),
#'    i_L = runif(100, min = 0, max = 5),
#'    V_out = runif(100, min = 0, max = 12)
#' )
#'   V_out_target <- 5
#' analyseperformances(V_out_target, results)
analyseperformances <- function(V_out_target, results) {

  times <- results$times
  V_out <- results$V_out
  N <- length(V_out)

  idx_stable <- floor(0.7 * N):N
  regime_stable <- V_out[idx_stable]

  V_max_stable <- max(regime_stable)
  V_min_stable <- min(regime_stable)
  V_moy_stable <- mean(regime_stable)

  ripple_peak_to_peak <- V_max_stable - V_min_stable
  ripple_rms <- sd(regime_stable)


  V_max_global <- max(V_out[floor(N*3/100):N])
  V_min_global <- min(V_out[floor(N*3/100):N])

  overshoot <- V_max_global - V_out_target
  overshoot_percent <- (overshoot / V_out_target) * 100

  undershoot <- V_out_target - V_min_global
  undershoot_percent <- (undershoot / V_out_target) * 100


  marge_stabilite <- ripple_peak_to_peak / 2
  V_min_acceptable <- V_moy_stable - marge_stabilite
  V_max_acceptable <- V_moy_stable + marge_stabilite


  stabilisation_time <- NA

  for (i in 1:(N - 50)) {
    fenetre_test <- V_out[i:(i + 50)]

    if (all(fenetre_test >= V_min_acceptable & fenetre_test <= V_max_acceptable)) {
      stabilisation_time <- times[i]
      break
    }
  }



  ripple_acceptable <- ripple_peak_to_peak < 0.050

  overshoot_acceptable <- overshoot_percent < 10

  undershoot_acceptable <- undershoot_percent < 5

  good_time_stab <- !is.na(stabilisation_time) && (stabilisation_time < 0.010)

  sufficiently_stable <- ripple_acceptable && overshoot_acceptable &&
    undershoot_acceptable



  cat("\n========================================\n")
  cat("PERFORMANCES ANALYSE \n")
  cat("========================================\n\n")

  cat("--- RIPPLE IN  STABLE REGIME ---\n")
  cat(sprintf("Average voltage   : %.4f V\n", V_moy_stable))
  cat(sprintf("Ripple peak-to-peak  : %.1f mV %s\n",
              ripple_peak_to_peak * 1000,
              ifelse(ripple_acceptable, "GOOD", "TOO HIGH")))
  cat(sprintf("Ripple RMS           : %.1f mV\n", ripple_rms * 1000))
  cat(sprintf("Intervalle stability : [%.4f V, %.4f V]\n\n",
              V_min_acceptable, V_max_acceptable))

  cat("--- EXCEEDING ---\n")
  cat(sprintf("Overshoot            : %.3f V (%.1f%%) %s\n",
              overshoot, overshoot_percent,
              ifelse(overshoot_acceptable, "GOOD", "TOO HIGH")))
  cat(sprintf("Undershoot           : %.3f V (%.1f%%) %s\n\n",
              undershoot, undershoot_percent,
              ifelse(undershoot_acceptable, "GOOD", "TOO HIGH")))

  cat("--- STABILISATION TIME ---\n")
  if (!is.na(stabilisation_time)) {
    cat(sprintf("Stabilisation time  : %.2f ms %s\n\n",
                stabilisation_time * 1000,
                ifelse(good_time_stab, "FAST (GOOD)", "SLOW (TO IMPROVE)")))
  } else {
    cat("STABILISATION TIME  : NOT REACHED \n\n")
  }

  cat("---  GLOBAL EVALUATION ---\n")
  cat(sprintf("System stable       : %s\n",
              ifelse(sufficiently_stable, "YES", "NO")))

  if (sufficiently_stable && good_time_stab) {
    cat("\n EXCELLENT : The system does not meet all the criteria !\n")
  } else if (sufficiently_stable) {
    cat("\n GOOD : The system is stable but the stabilization time is improveable \n")
  } else {
    cat("\n INSUFFICIENT : Adjust the components L, C or the duty cycle \n")
  }

  cat("\n========================================\n")

  return(list(
    ripple_peak_to_peak = ripple_peak_to_peak,
    ripple_rms = ripple_rms,
    overshoot = overshoot,
    overshoot_percent = overshoot_percent,
    undershoot = undershoot,
    undershoot_percent = undershoot_percent,
    stabilisation_time = stabilisation_time,
    V_moyenne = V_moy_stable,
    sufficiently_stable = sufficiently_stable,
    good_time_stab = good_time_stab,
    ripple_acceptable = ripple_acceptable,
    overshoot_acceptable = overshoot_acceptable,
    undershoot_acceptable = undershoot_acceptable
  ))

}
