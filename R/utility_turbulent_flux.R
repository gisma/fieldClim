#' sc PT coefficient
#'
#' Calculates sc for latent and sensible Priestley-Taylor-Method.
#' sc is the gradient of Clausius-Clapeyron equation.
#' This function is a polynomial fit for the table 6 in Foken (2013), p.48.
#'
#'
#' @param t Air temperature in °C.
#'
#' @returns sc coefficient for Priestley-Taylor calculations.
sc <- function(t) {
  8.5 * 10^(-7) * (t + 273.15)^2 - 0.0004479 * (t + 273.15) + 0.05919
}


#' gamma Priestly-Taylor coefficient
#'
#' Calculates gamma for latent and sensible Priestley-Taylor-Method.
#' gamma is the temperature-sensitive psychrometer constant.
#' This function is a polynomial fit for the table 6 in Foken (2013), p.48.
#'
#' @param t Air temperature in °C.
#'
#' @returns gamma coefficient for Priestley-Taylor calculations.
gam <- function(t) {
  0.0004 + (0.00041491 - 0.0004) / (1 + (299.44 / (t + 273.15))^383.4)
}


#' Bowen-ratio
#'
#' Calculates Bowen-ratio.
#'
#' @param t Air temperature in °C.
#' @param dpot Difference in potential temperature between the two measurement
#' heights in °C.
#' @param dah Difference in absolute humidity (kg/m³) between the two measurement heights.
#'
#' @returns Bowen-ratio
#' @export
#' @references p221eq9.21.
bowen_ratio <- function(t, dpot, dah) {
  heat_cap <- heat_capacity(t)
  evap_heat <- hum_evap_heat(t)
  (heat_cap * dpot) / (evap_heat * dah)
}

#' Volumetric heat capacity
#'
#' Calculates volumetric heat capacity
#'
#' @param t Air temperature in °C.
#'
#' @returns Heat capacity density in J/(K*m³)
#' @export
#' @references p261.
heat_capacity <- function(t) {
  1005 * (1.2754298 - 0.0047219538 * t + 1.6463585 * 10^-5 * t)
}
