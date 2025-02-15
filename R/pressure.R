#' Air pressure
#'
#' Calculate pressure based on barometric formula.
#'
#' The formula assumes that the temperature does not change with altitude.
#' The results should be satisfying enough for an elevation lower than 5 km.
#'
#' @rdname pres_p
#' @inheritParams build_weather_station
#' @returns hPa.
#' @export
pres_p <- function(...) {
  UseMethod("pres_p")
}

#' @rdname pres_p
#' @inheritParams build_weather_station
#' @param p0 Standard pressure in hPa, default `r p0_default`.
#' @param g Gravitational acceleration in \eqn{m \cdot s^{-2}}, default `r g_default`.
#' @param rl Specific gas constant for air in \eqn{m^2 \cdot s^{-2} \cdot K^{-1}}, default `r rl_default`.
#' @export
#' @references Lente & Ősz 2020 eq. 5.
pres_p.default <- function(elev, temp, ...,
    p0 = p0_default, g = g_default, rl = rl_default) {
  temp <- c2k(temp)

  p0 * exp(-(g * elev) / (rl * temp))
}

#' @rdname pres_p
#' @inheritParams build_weather_station
#' @export
pres_p.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(pres_p.default)
  a <- a[1:(length(a)-4)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }

  pres_p(elev, temp, ...)
}

#' Vapor pressure
#'
#' Calculates vapor pressure from relative humidity and saturation vapor pressure.
#'
#' @inheritParams build_weather_station
#' @returns hPa.
#' @export
pres_vapor_p <- function(...) {
  UseMethod("pres_vapor_p")
}

#' @rdname pres_vapor_p
#' @inheritParams build_weather_station
#' @inheritDotParams pres_sat_vapor_p.default a b
#' @export
#' @references Bendix 2004, p. 262
pres_vapor_p.default <- function(temp, rh, ...) {
  sat_vapor_p <- pres_sat_vapor_p(temp, ...)
  (rh / 100) * sat_vapor_p
}

#' @rdname pres_vapor_p
#' @inheritParams build_weather_station
#' @export
pres_vapor_p.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(pres_vapor_p.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }

  pres_vapor_p(temp, rh, ...)
}

#' Saturated vapor pressure
#'
#' @inheritParams build_weather_station
#' @returns hPa.
#' @export
pres_sat_vapor_p <- function(...) {
  UseMethod("pres_sat_vapor_p")
}

#' @rdname pres_sat_vapor_p
#' @inheritParams build_weather_station
#' @param a Constant a is 7.5 (default) over water, 7.6 over undercooled water, and 9.5 over ice.
#' @param b Constant b is 235 (defalut) over water, 240.7 over undercooled water, and 265.5 over ice.
#' @export
#' @references Bendix 2004, p. 261.
pres_sat_vapor_p.default <- function(temp, ..., a = 7.5, b = 235) {
  6.1078 * 10^((a * temp) / (b + temp))
}

#' @rdname pres_sat_vapor_p
#' @inheritParams build_weather_station
#' @export
pres_sat_vapor_p.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(pres_sat_vapor_p.default)
  a <- a[1:(length(a)-3)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }

  pres_sat_vapor_p(temp, ...)
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
#' @inheritParams build_weather_station
#' @export
pres_air_density.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "elev", "temp")
  elev <- weather_station$elev
  temp <- weather_station$temp
  return(pres_air_density(elev, temp))
}

#' @rdname pres_air_density
#' @inheritParams build_weather_station
#' @export
#'
pres_air_density.default <- function(elev, temp, ...) {
  p <- pres_p(elev, temp)
  (p * 100) / (287.05 * (temp + 273.15))
}
