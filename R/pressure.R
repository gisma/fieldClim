#' Air pressure
#'
#' Calculate pressure based on barometric formula.
#'
#' The formula assumes that the temperature does not change with altitude.
#' The results are satisfying enough for an height under 5 km.
#'
#' @rdname pres_p
#' @param ... Additional arguments.
#' @returns Pressure in hPa.
#' @export
pres_p <- function(...) {
  UseMethod("pres_p")
}

#' @rdname pres_p
#' @param elev Elevation above sea level in m.
#' @param temp Temperature in degree Celcius.
#' @param p0 Standard pressure in hPa, default `r p0_default`.
#' @param g Gravitational acceleration in m/s\eqn{^2}, default `r g_default`.
#' @param rl Specific gas constant for air in m\eqn{^2}/s\eqn{^2}/K, default `r rl_default`.
#' @export
#' @references Lente & Ősz 2020 eq5.
pres_p.default <- function(elev, temp, ...,
    p0 = p0_default, g = g_default, rl = rl_default) {
  temp <- c2k(temp)

  p0 * exp(-(g * elev) / (rl * temp))
}

#' @rdname pres_p
#' @inheritParams sol_julian_day
#' @export
pres_p.weather_station <- function(weather_station, ...) {
  a <- formalArgs(pres_p.default)
  a <- a[1:(length(a)-4)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  pres_p(elev, temp, weather_station)
}

#' Vapor pressure
#'
#' Calculates vapor pressure from relative humidity and saturation vapor pressure.
#'
#' @param ... Additional arguments.
#' @returns Vapor pressure in hPa.
#' @export
pres_vapor_p <- function(...) {
  UseMethod("pres_vapor_p")
}

#' @rdname pres_vapor_p
#' @inheritParams pres_sat_vapor_p
#' @inheritDotParams pres_sat_vapor_p
#' @param rh Relative humidity in %.
#' @export
pres_vapor_p.default <- function(temp, rh, ...) {
  sat_vapor_p <- pres_sat_vapor_p(temp, ...)
  (rh / 100) * sat_vapor_p
}

#' @rdname pres_vapor_p
#' @export
#' @inheritParams sol_julian_day
#' @param height Height of measurement. "lower" or "upper".
pres_vapor_p.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2", "hum1", "hum2")
  if (!height %in% c("upper", "lower")) {
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  hum <- weather_station$measurements[[paste0("hum", height_num)]]
  return(pres_vapor_p(hum, t))
}

#' Saturated vapor pressure
#'
#' Calculates the saturation vapor pressure from air temperature using the \emph{Magnus}
#' formula (applicable over water surfaces).
#'
#' @param ... Additional arguments.
#' @returns Saturation vapor pressure in hPa.
#' @export
pres_sat_vapor_p <- function(...) {
  UseMethod("pres_sat_vapor_p")
}

#' @rdname pres_sat_vapor_p
#' @param temp Air temperature in °C.
#' @param a Constant a is 7.5 (default) over water, 7.6 over undercooled water, and 9.5 over ice.
#' @param b Constant b is 235 (defalut) over water, 240.7 over undercooled water, and 265.5 over ice.
#' @export
#' @references p261.
pres_sat_vapor_p.default <- function(temp, a = 7.5, b = 235, ...) {
  6.1078 * 10^((a * temp) / (b + temp))
}

#' @rdname pres_sat_vapor_p
#' @export
#' @inheritParams sol_julian_day
#' @param height Height of measurement. "lower" or "upper".
pres_sat_vapor_p.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2")
  if (!height %in% c("upper", "lower")) {
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  return(pres_sat_vapor_p(t))
}

#' Air density
#'
#' Calculation of the air density.
#'
#' @rdname pres_air_density
#' @param ... Additional arguments.
#' @returns Air density in kg/m\eqn{^3}.
#' @export
#'
pres_air_density <- function(...) {
  UseMethod("pres_air_density")
}

#' @rdname pres_air_density
#' @inheritParams sol_julian_day
#' @param height "lower" or "upper"
#' @export
#'
pres_air_density.weather_station <- function(weather_station, height = "lower", ...) {
  if (height == "lower") {
    check_availability(weather_station, "t1", "p1")
    t <- weather_station$measurements$t1 # to Kelvin
    p <- weather_station$measurements$p1
  } else if (height == "upper") {
    check_availability(weather_station, "t2", "p2")
    t <- weather_station$measurements$t2 # to Kelvin
    p <- weather_station$measurements$p2
  }
  check_availability(t, p)
  return(pres_air_density(p, t))
}

#' @rdname pres_air_density
#' @param p Pressure in hPa.
#' @param t Temperature in °C.
#' @export
#'
pres_air_density.default <- function(p, t, ...) {
  (p * 100) / (287.05 * (t + 273.15))
}
