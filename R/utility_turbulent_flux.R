#' sc PT coefficient
#'
#' Calculates sc for latent and sensible Priestley-Taylor-Method.
#' sc is the gradient of Clausius-Clapeyron equation.
#' This function is a polynomial fit for the table 6 in Foken (2013), p.48.
#'
#'
#' @param t Air temperature in °C.
#'
#' @return sc coefficient for Priestley-Taylor calculations.
sc <- function(t){
  sc <- 8.5 * 10^(-7) * (t + 273.15)^2 - 0.0004479 * (t + 273.15) + 0.05919
  return(sc)
}


#' gamma Priestly-Taylor coefficient
#'
#' Calculates gamma for latent and sensible Priestley-Taylor-Method.
#' gamma is the temperature-sensitive psychrometer constant.
#' This function is a polynomial fit for the table 6 in Foken (2013), p.48.
#'
#' @param t Air temperature in °C.
#'
#' @return gamma coefficient for Priestley-Taylor calculations.
gam <- function(t){
  gam <- 0.0004 + (0.00041491 - 0.0004) / (1 + (299.44 / (t + 273.15))^383.4)
  return(gam)
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
#' @return Bowen-ratio
#' @export
#' @references p221eq9.21.
bowen_ratio <- function(t, dpot, dah){
  heat_cap <- heat_capacity(t)
  evap_heat <- hum_evap_heat(t)
  Bow2 <- (heat_cap*dpot) / (evap_heat*dah)
  return(Bow2)
}

#' Volumetric heat capacity
#'
#' Calculates volumetric heat capacity
#'
#' @param t Air temperature in °C.
#'
#' @return Heat capacity density in J/(K*m³)
#' @export
#' @references p261.
heat_capacity <- function(t){
  ca <- 1005 * (1.2754298 - 0.0047219538 * t + 1.6463585 * 10^-5 * t)
  return(ca)
}
