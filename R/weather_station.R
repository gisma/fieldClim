#' Build a weather station object
#'
#' Creates a list of class `weather_station` that contains all input arguments.
#' Only the following is used:
#' * datetime Datetime in `POSIXt`.
#' * lon Longitude in degree.
#' * lat Latitude in degree.
#' * elev Elevation above sea level in m.
#' * temp Temperature in \eqn{^\circ}C.
#' * surface_temp Surface temperature in \eqn{^\circ}C.
#' * surface_type Surface type for which a specific emissivity will be selected.
#' * rh Relative humidity in %.
#' * sol_const Solar constant in W/m\eqn{^2}, default `r sol_const_default`.
#' * p0 Standard pressure in hPa, default `r p0_default`.
#' * g Gravitational acceleration in m/s\eqn{^2}, default `r g_default`.
#' * rl Specific gas constant for air in m\eqn{^2}/s\eqn{^2}/K, default `r rl_default`.
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