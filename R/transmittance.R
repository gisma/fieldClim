#' Transmittance due to gas
#'
#' Calculates transmittance due to O$_{2}$ and CO$_{2}$.
#'
#' @param ... Additional arguments.
#' @returns Transmittance due to gas (0-1), unitless
#' @export
trans_gas <- function(...) {
  UseMethod("trans_gas")
}

#' @rdname trans_gas
#' @inheritParams trans_air_mass_abs
#' @export
#' @references p246.
trans_gas.default <- function(datetime, lon, lat, elev, temp, ...) {
  air_mass_abs <- trans_air_mass_abs(datetime, lon, lat, elev, temp)
  
  exp(-0.0127 * air_mass_abs^0.26)
}

#' @rdname trans_gas
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_gas.weather_station <- function(weather_station, ...) {
  air_mass_abs <- trans_air_mass_abs(weather_station)
  return(trans_gas(air_mass_abs))
}

#' Absolute optical air mass
#'
#' Calculates absolute optical air mass.
#'
#' @param ... Additional arguments.
#' @returns Absolute optical air mass.
#' @export
trans_air_mass_abs <- function(...) {
  UseMethod("trans_air_mass_abs")
}

#' @rdname trans_air_mass_abs
#' @inheritParams trans_air_mass_rel
#' @inheritParams pres_p
#' @inheritDotParams pres_p.default g rl
#' @export
#' @references p247.
trans_air_mass_abs.default <- function(datetime, lon, lat, elev, temp, ...) {
  air_mass_rel <- trans_air_mass_rel(datetime, lon, lat)
  p <- pres_p(elev, temp, ...)
  p0 <- p0_default # will be cancled in pres_p
  
  air_mass_rel * (p / p0)
}

#' @rdname trans_air_mass_abs
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_air_mass_abs.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "p2")

  p <- weather_station$measurements$p2
  air_mass_rel <- trans_air_mass_rel(weather_station)
  return(trans_air_mass_abs(air_mass_rel, p))
}

#' Relative optical air mass
#'
#' Calculates relative optical air mass. Returns NA for negative values.
#'
#' @param ... Additional arguments.
#' @returns Relative optical air mass.
#' @export
trans_air_mass_rel <- function(...) {
  UseMethod("trans_air_mass_rel")
}

#' @rdname trans_air_mass_rel
#' @inheritParams sol_elevation
#' @export
#' @references p246.
trans_air_mass_rel.default <- function(datetime, lon, lat, ...) {
  elevation <- sol_elevation(datetime, lon, lat)
  
  1 / (sin(deg2rad(elevation)) + 1.5 * elevation^-0.72)
}

#' @rdname trans_air_mass_rel
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_air_mass_rel.weather_station <- function(weather_station, ...) {
  sol_elevation <- sol_elevation(weather_station)
  return(trans_air_mass_rel(sol_elevation))
}

#' Transmittance due to ozone
#'
#' Calculates transmittance due to ozone.
#'
#' @param ... Additional arguments.
#' @returns Transmittance due to ozone (0-1). unitless
#' @export
trans_ozone <- function(...) {
  UseMethod("trans_ozone")
}

#' @rdname trans_ozone
#' @inheritParams trans_air_mass_rel
#' @param ozone_column Atmospheric ozone as column in cm. Default is average value of 0.35 cm.
#' @export
#' @references p245.
trans_ozone.default <- function(datetime, lon, lat, ozone_column = 0.35, ...) {
  air_mass_rel <- trans_air_mass_rel(datetime, lon, lat)
  x <- ozone_column * air_mass_rel
  
  1 - (
    0.1611 * x * (1 + 139.48 * x)^-0.3035 -
    0.002715 * x * (1 + 0.044 * x + 0.0003 * x^2)^-1
  )
}

#' @rdname trans_ozone
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_ozone.weather_station <- function(weather_station, ...) {
  air_mass_rel <- trans_air_mass_rel(weather_station)
  return(trans_ozone(air_mass_rel, ...))
}

#' Transmittance due to rayleigh scattering
#'
#' Calculates transmittance due to rayleigh scattering.
#'
#' @param ... Additional arguments.
#' @returns Transmittance due to rayleigh scattering (0-1). unitless
#' @export
trans_rayleigh <- function(...) {
  UseMethod("trans_rayleigh")
}

#' @rdname trans_rayleigh
#' @inheritParams trans_air_mass_abs
#' @export
#' @references p245.
trans_rayleigh.default <- function(datetime, lon, lat, elev, temp, ...) {
  air_mass_abs <- trans_air_mass_abs(datetime, lon, lat, elev, temp)
  
  exp(-0.0903 * air_mass_abs^0.84 * (1 + air_mass_abs - air_mass_abs^1.01))
}

#' @rdname trans_rayleigh
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_rayleigh.weather_station <- function(weather_station, ...) {
  air_mass_abs <- trans_air_mass_abs(weather_station)
  return(trans_rayleigh(air_mass_abs))
}

#' Transmittance due to water vapor
#'
#' Calculates transmittance due to water vapor.
#'
#' @rdname trans_vapor
#' @param ... Additional arguments.
#' @returns Transmittance due to water vapor (0-1). unitless
#' @export
trans_vapor <- function(...) {
  UseMethod("trans_vapor")
}

#' @rdname trans_vapor
#' @inheritParams hum_precipitable_water
#' @inheritParams trans_air_mass_rel
#' @export
#' @references p245.
trans_vapor.default <- function(datetime, lon, lat, elev, temp, ...) {
  precipitable_water <- hum_precipitable_water(datetime, lat, elev, temp)
  air_mass_rel <- trans_air_mass_rel(datetime, lon, lat)
  x <- precipitable_water * air_mass_rel
  
  1 - 2.4959 * x * ((1 + 79.034 * x)^0.6828 + 6.385 * x)^-1
}

#' @rdname trans_vapor
#' @param weather_station Object of class weather_station.
#' @export
#'
trans_vapor.weather_station <- function(weather_station, ...) {
  air_mass_rel <- trans_air_mass_rel(weather_station)
  precipitable_water <- hum_precipitable_water(weather_station)
  return(trans_vapor(air_mass_rel, precipitable_water))
}

#' Transmittance due to aerosols
#'
#' Calculates transmittance due to aerosols.
#'
#' @param ... Additional arguments.
#' @returns Transmittance due to aerosols (0-1). unitless
#' @export
trans_aerosol <- function(...) {
  UseMethod("trans_aerosol")
}

#' @rdname trans_aerosol
#' @inheritParams trans_air_mass_abs
#' @param vis Visibility in km.
#' @export
#' @references p246.
trans_aerosol.default <- function(datetime, lon, lat, elev, temp, vis = 30, ...) {
  air_mass_abs <- trans_air_mass_abs(datetime, lon, lat, elev, temp)
  
  df <- data.frame(
    vis = seq(10, 60, 10),
    tau38 = c(0.71, 0.43, 0.33, 0.27, 0.22, 0.20),
    tau50 = c(0.46, 0.28, 0.21, 0.17, 0.14, 0.13)
  )
  mod38 <- lm(log(df$tau38)~log(df$vis))
  mod50 <- lm(log(df$tau50)~log(df$vis))
  
  tau38 <- exp(mod38$coefficients[[1]]) * vis^mod38$coefficients[[2]]
  tau50 <- exp(mod50$coefficients[[1]]) * vis^mod50$coefficients[[2]]
  
  x <- 0.2758 * tau38 + 0.35 * tau50
  
  exp(-x^0.873 * (1 + x - x^0.7088) * air_mass_abs^0.9108)
}

#' @rdname trans_aerosol
#' @param weather_station Object of class weather_station.
#' @export
trans_aerosol.weather_station <- function(weather_station, ...) {
  air_mass_abs <- trans_air_mass_abs(weather_station)
  return(trans_aerosol(weather_station, ...))
}















#' Total transmittance
#'
#' Calculates total transmittance of the atmosphere.
#'
#' @rdname trans_total
#' @param ... Additional arguments.
#' @returns Total transmittance (0-1)
#' @export
#'
#trans_total <- function(...) {
#  UseMethod("trans_total")
#}

#' @rdname trans_total
#' @param sol_elevation Solar elevation in degrees.
#' @param t Air temperature in °C.
#' @param elev Altitude above sea level in m.
#' @param oz Atmospheric ozone as column in cm. Default is average global value.
#' @param vis Meteorological visibility in km. Default is the visibility on a clear day.
#' @param p OPTIONAL. Pressure in hPa. Estimated from elev and t if not available.
#' @export
#' @references p46.
#trans_total.default <- function(sol_elevation, t, elev, oz = 0.35, vis = 30,
#                                p = NULL, ...) {
#  if (is.null(p)) p <- pres_p(elev, t)
#  pw <- hum_precipitable_water(p, t, elev)
#  mr <- trans_air_mass_rel(sol_elevation)
#  ma <- trans_air_mass_abs(mr, p)
#  trans_total <- data.frame(
#    rayleigh = trans_rayleigh(ma),
#    ozone = trans_ozone(mr, oz),
#    vapor = trans_vapor(mr, pw),
#    aerosol = trans_aerosol(ma, vis),
#    gas = trans_gas(ma)
#  )
#  trans_total$total <- apply(trans_total, 1, FUN = prod)
#  return(trans_total$total)
#}

#' @rdname trans_total
#' @param weather_station Object of class weather_station.
#' @param oz OPTIONAL. Columnar ozone in cm.
#' Default is average global value.
#' @param vis OPTIONAL. Meteorological visibility in km.
#' Default is the visibility on a clear day.
#' @export
#'
#trans_total.weather_station <- function(weather_station, oz = 0.35, vis = 30, ...) {
#  sol_elevation <- sol_elevation(weather_station)
#  check_availability(weather_station, "t2", "z2", "elevation", "p2")
#  t <- weather_station$measurements$t2
#  elev <- weather_station$location_properties$elevation + weather_station$properties$z2
#  p2 <- weather_station$measurements$p2
#  return(trans_total(sol_elevation, t, elev, oz = oz, vis = vis, p = p2, ...))
#}
