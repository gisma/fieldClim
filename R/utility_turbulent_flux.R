#' sc PT coefficient
#'
#' Calculates sc for latent and sensible Priestley-Taylor-Method.
#'
#' @param t Air temperature in °C.
#'
#' @return sc coefficient for Priestley-Taylor calculations
sc <- function(t){
  # Calculates sc for latent and sensible Priestley-Taylor-Method
  sc <- 8.5*10^(-7)*(t+273.15)^2 - 0.0004479*(t+273.15) + 0.05919
  return(sc)
}

#' lambda PT coefficient
#'
#' Calculates lambda for latent and sensible Priestley-Taylor-Method.
#'
#' @param t Air temperature in °C
#'
#' @return lambda coefficient for Priestley-Taylor calculations
lamb <- function(t){
  # formula: sc/gamma = -0.4 + 1.042 * e^(0.0443 * t)
  lamb <- 0.0004+(0.00041491-0.0004)/(1+(299.44/(t+273.15))^383.4)
  return(lamb)
}

# # noch überprüfen
# #' Psychrometric constant gamma
# #'
# #' Calculates the psychrometric constant gamma for Priestley-Taylor calculations.
# #'
# #' @param t temperature in °C.
# #' @param z elevation of measurement in m.
# #' @return psychrometric constant in kPa/°C
# gam <- function (t = 15, z = 270){
#   heat_capacity()
#   # 0.001005 is the specific heat capacity of air with constant pressure in MJ/(kg*K)
#   return(0.001005 * (pres_p(z, t)/10) /2.26 * 0.622)
# }



#' Bowen-ratio
#'
#' Calculates Bowen-ratio.
#'
#' @param t Air temperature in degrees C.
#' @param dpot Difference in potential temperature between the two measurement
#' heights in °C.
#' @param dah Difference in absolute humidity (kg/m^3) between the two measurement heights.
#'
#' @return Bowen-ratio
#' @export
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
#' @param t Air temperature in degrees C.
#'
#' @return Heat capacity density in J/(K*m^3)
#' @export
#'
heat_capacity <- function(t){
  ca <- 1005*(1.2754298-0.0047219538*t+1.6463585*10^-5*t)
  return(ca)
}
