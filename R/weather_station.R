#' Build a weather station object
#'
#' Creates a list of class `weather_station` that contains all input arguments.
#'
#' Provided input arguments will only be used if they are listed in the section "Arguments".
#'
#' @param ... Additional arguments.
#' @param weather_station Object of class `weather_station`.
#' @param datetime Datetime of class `POSIXlt`. See [base::as.POSIXlt()].
#' @param lon Longitude in degree.
#' @param lat Latitude in degree.
#' @param elev Elevation above sea level in m.
#' @param temp Temperature in \eqn{^\circ}C.
#' @param t1 Air temperature at lower height in °C.
#' @param t2 Air temperature at upper height in °C.
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param slope Slope in degree.
#' @param exposition Exposition in degree.
#' @param surface_type Surface type for which a specific emissivity will be selected.
#' @param obs_height Height of obstacle in m.
#' @param valley If the position is in a valley (TRUE) or on a slope (FALSE).
#' @param surface_temp Surface temperature in \eqn{^\circ}C.
#' @param rh Relative humidity in %.
#' @param hum1 Relative humidity at lower height in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param rad_bal Radiation balance in W/m\eqn{^2}.
#' @param texture Soil texture. Either `sand`, `peat` or `clay`.
#' @param moisture Soil moisture in cubic meter/cubic meter.
#' @param soil_flux Soil flux in W/m\eqn{^2}.
#' @param soil_temp1 Soil temperature in °C at upper height.
#' @param soil_temp2 Soil temperature in °C at lower height.
#' @param soil_depth Depth of the soil temperature measurement at upper height in m.
#' @param soil_depth2 Depth of the soil temperature measurement at upper height in m.
#' @param sol_const Solar constant in W/m\eqn{^2}. Default = `r sol_const_default`.
#' @param ozone_column Atmospheric ozone as column in cm, default `r ozone_column_default`.
#' @param vis Visibility in km. Default = `r vis_default`.
#' @param p0 Standard pressure in hPa. Default = `r p0_default`.
#' @param g Gravitational acceleration in m/s\eqn{^2}. Default = `r g_default`.
#' @param rl Specific gas constant for air in m\eqn{^2}/s\eqn{^2}/K. Default = `r rl_default`.
#' @param a Constant a is 7.5 (default) over water, 7.6 over undercooled water, and 9.5 over ice.
#' @param b Constant b is 235 (defalut) over water, 240.7 over undercooled water, and 265.5 over ice.
#' @param sigma Stefan-Boltzmann constant in W/m\eqn{^2}/K\eqn{^4}. Default = `r sigma_default'.
#'
#' @export
build_weather_station <- function(...) {
  # Function code here...
}

#' @export
build_weather_station <- function(...) {
  # Function code here...
}

#' @export
build_weather_station.default <- function(...) {
  out <- list()

  args <- list(...)
  for (i in seq_along(args)) {
    # Add additional parameters to the right spot in the list
    name <- names(args)[i]
    value <- args[[i]]
    out[[name]] <- value
  }

  class(out) <- "weather_station"

  out
}
