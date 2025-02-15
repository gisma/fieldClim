#' Potential Temperature
#'
#' Calculation of the potential air temperature.
#'
#' @rdname temp_pot_temp
#' @param ... Additional arguments.
#' @returns Potential temperature in °C.
#' @export
#'
temp_pot_temp <- function(...) {
  UseMethod("temp_pot_temp")
}

#' @rdname temp_pot_temp
#' @param t Temperature in °C.
#' @param elev Elevation above sea level in m.
#' @export
#' @references Bendix 2004, p. 261.
temp_pot_temp.default <- function(t, elev, ...) {
  p0 <- p0_default # standard air pressure in hPa
  p <- pres_p(elev, t, ...) # calculate air pressure
  air_const <- 0.286 # specific gas constant / specific heat capacity
  t <- c2k(t) # to Kelvin
  k2c(t * (p0 / p)**air_const)
}

#' @rdname temp_pot_temp
#' @inheritParams build_weather_station
#' @param height Height of measurement, either "upper" or "lower".
#' @export
temp_pot_temp.weather_station <- function(weather_station, height = "lower", ...) {
  if (height == "lower") {
    check_availability(weather_station, "t1", "elev")
    t <- weather_station$t1
    elev <- weather_station$elev
  } else if (height == "upper") {
    check_availability(weather_station, "t2", "elev")
    t <- weather_station$t2
    elev <- weather_station$elev
  }
  return(temp_pot_temp(t, elev))
}
