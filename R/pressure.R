#' Air pressure
#'
#' Calculation of pressure as a function of the elevation above sea level.
#'
#' @rdname pres_p
#' @param ... Additional parameters passed to later functions.
#' @return Pressure in hPa.
#' @export
#'
pres_p <- function (...) {
  UseMethod("pres_p")
}

#' @rdname pres_p
#' @method pres_p numeric
#' @param elev Elevation above sea level in m.
#' @param t Temperature in °C.
#' @export
#'
pres_p.numeric <- function(elev, t, ...){
  t <- t + 273.15  # to Kelvin
  p0 <- 1013.25    # standard pressure in hPa
  g <- 9.81        # gravity acceleration
  rl <- 287.05     # specific gas constant
  p <- p0 * exp(- (g * elev) / (rl * t))
  return(p)
}


#' @rdname pres_p
#' @method pres_p weather_station
#' @param weather_station Object of class weather_station.
#' @param height 'lower' or 'upper'
#' @export
#'
pres_p.weather_station <- function(weather_station, height = "lower", ...){
  if(height=="lower"){
    check_availability(weather_station, "t1", "elevation", "z1")
    t <- weather_station$measurements$t1   # to Kelvin
    elev <- weather_station$location_properties$elevation + weather_station$properties$z1
  } else if(height=="upper"){
    check_availability(weather_station, "t2", "elevation", "z2")
    t <- weather_station$measurements$t2   # to Kelvin
    elev <- weather_station$location_properties$elevation + weather_station$properties$z2
  }
  return(pres_p(elev, t))
}


#' Air density
#'
#' Calculation of the air density.
#'
#' @rdname pres_air_density
#' @param ... Additional parameters passed to later functions.
#' @return Air density in kg/m³.
#' @export
#'
pres_air_density <- function (...) {
  UseMethod("pres_air_density")
}

#' @rdname pres_air_density
#' @method pres_air_density weather_station
#' @param weather_station Object of class weather_station.
#' @param height "lower" or "upper"
#' @export
#'
pres_air_density.weather_station <- function(weather_station, height = "lower", ...){
  if(height=="lower"){
    check_availability(weather_station, "t1", "p1")
    t <- weather_station$measurements$t1   # to Kelvin
    p <- weather_station$properties$p1
  } else if(height=="upper"){
    check_availability(weather_station, "t2", "p2")
    t <- weather_station$measurements$t2   # to Kelvin
    p <- weather_station$properties$p2
  }
  check_availability(t, p)
  return(pres_air_density(p, t))
}

#' @rdname pres_air_density
#' @method pres_air_density numeric
#' @param p Pressure in hPa.
#' @param t Temperature in °C.
#' @export
#'
pres_air_density.numeric <- function(p, t, ...){
  ad <- (p * 100) / (287.05 * (t + 273.15))
  return(ad)
}
