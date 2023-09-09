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
#' @param slope Slope in degree.
#' @param exposition Exposition in degree.
#' @param surface_type Surface type for which a specific emissivity will be selected.
#' @param valley If the position is in a valley (TRUE) or on a slope (FALSE).
#' @param surface_temp Surface temperature in \eqn{^\circ}C.
#' @param rh Relative humidity in %.
#' @param texture Soil texture. Either `sand`, `peat` or `clay`.
#' @param moisture Soil moisture in cubic meter/cubic meter.
#' @param soil_temp1, soil_temp2 Soil temperature in °C.
#' @param soil_depth, soil_depth2 Depth of the soil temperature measurement in m.
#' @param sol_const Solar constant in W/m\eqn{^2}, default `r sol_const_default`.
#' @param ozone_column Atmospheric ozone as column in cm, default `r ozone_column_default`.
#' @param vis Visibility in km, default `r vis_default`.
#' @param p0 Standard pressure in hPa, default `r p0_default`.
#' @param g Gravitational acceleration in m/s\eqn{^2}, default `r g_default`.
#' @param rl Specific gas constant for air in m\eqn{^2}/s\eqn{^2}/K, default `r rl_default`.
#' @param a Constant a is 7.5 (default) over water, 7.6 over undercooled water, and 9.5 over ice.
#' @param b Constant b is 235 (defalut) over water, 240.7 over undercooled water, and 265.5 over ice.
#' @param sigma Stefan-Boltzmann constant in W/m\eqn{^2}/K\eqn{^4}, default `r sigma_default'.
#'
#' @section Arguments without default value:
#'
#' | Argument         | Description                                      |
#' |------------------|--------------------------------------------------|
#' | datetime         | Datetime in `POSIXlt`.                            |
#' | lon              | Longitude in degree.                             |
#' | lat              | Latitude in degree.                              |
#' | elev             | Elevation above sea level in m.                 |
#' | temp             | Temperature in \eqn{^\circ}C.                        |
#' | slope            | Slope in degree.                                 |
#' | exposition        | Exposition in degree.                            |
#' | surface_type     | Surface type for which a specific emissivity will be selected. |
#' | valley           | If the position is in a valley (TRUE) or on a slope (FALSE). |
#' | surface_temp     | Surface temperature in \eqn{^\circ}C.                |
#' | rh               | Relative humidity in %.                          |
#' | texture          | Soil texture. Either sand, peat, or clay.       |
#' | moisture         | Soil moisture in cubic meter/cubic meter.       |
#' | soil_temp1       | Soil temperature in \eqn{^\circ}C (upper height).          |
#' | soil_temp2       | Soil temperature in \eqn{^\circ}C (lower height).          |
#' | soil_depth       | Depth of the soil temperature measurement in m (upper height). |
#' | soil_depth2      | Depth of the soil temperature measurement in m (lower height). |
#'
#' @section Arguments with default value:
#'
#' | Argument         | Description                                      |
#' |------------------|--------------------------------------------------|
#' | sol_const        | Solar constant in W/m\eqn{^2}, default = `r sol_const_default`. |
#' | ozone_column     | Atmospheric ozone as column in cm, default = `r ozone_column_default`. |
#' | vis              | Visibility in km, default = `r vis_default`.    |
#' | p0               | Standard pressure in hPa, default = `r p0_default`. |
#' | g                | Gravitational acceleration in m/s\eqn{^2}, default = `r g_default`. |
#' | rl               | Specific gas constant for air in m\eqn{^2}/s\eqn{^2}/K, default = `r rl_default`. |
#' | a                | Constant a is 7.5 (default) over water, 7.6 over undercooled water, and 9.5 over ice. |
#' | b                | Constant b is 235 (default) over water, 240.7 over undercooled water, and 265.5 over ice. |
#' | sigma            | Stefan-Boltzmann constant in W/m\eqn{^2}/K\eqn{^4}, default = `r sigma_default`. |
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
build_weather_station <- function(...) {
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
