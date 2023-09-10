#' Build a weather station object
#'
#' Creates a list of class `weather_station` that contains all input arguments.
#'
#' Provided input arguments will only be used if they are listed in the section
#' "Arguments". No warning message is generated for unused arguments.
#'
#' @param ... Additional arguments.
#' @param weather_station Object of class `weather_station`.
#' @param datetime Datetime of class `POSIXlt`. See [base::as.POSIXlt()].
#' @param lon Longitude in degree.
#' @param lat Latitude in degree.
#' @param elev Elevation above sea level in m.
#' @param temp Air temperature in degree Celcius.
#' @param t1 Air temperature at lower height in degree Celcius.
#' @param t2 Air temperature at upper height in degree Celcius.
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param slope Slope in degree.
#' @param exposition Exposition in degree.
#' @param surface_type Surface type.
#'   Allowed values are: `r surface_properties$surface_type`.
#'   Object `surface_properties` is a table of surface properties.
#' @param obs_height Height of obstacle in m.
#' @param valley Is the position in a valley (`TRUE`) or on a slope (`FALSE`)?
#' @param surface_temp Surface temperature in degree Celcius.
#' @param rh Relative humidity in %.
#' @param hum1 Relative humidity at lower height in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param rad_bal Radiation balance in W/m\eqn{^2}.
#' @param texture Soil texture. Either "sand", "clay", or "peat".
#' @param moisture Soil moisture in cubic meter/cubic meter.
#' @param soil_flux Soil flux in W/m\eqn{^2}.
#' @param soil_temp1 Soil temperature in degree Celcius of measurement 1.
#' @param soil_temp2 Soil temperature in degree Celcius of measurement 2.
#' @param soil_depth1 Depth of the soil temperature measurement 1 in m.
#' @param soil_depth2 Depth of the soil temperature measurement 2 in m.
#' @inheritDotParams rad_sw_toa.default sol_const
#' @inheritDotParams pres_p.default p0 g rl
#' @inheritDotParams trans_ozone.default ozone_column
#' @inheritDotParams trans_aerosol.default vis
#' @inheritDotParams pres_sat_vapor_p.default a b
#' @inheritDotParams rad_lw_in.default sigma
#'
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