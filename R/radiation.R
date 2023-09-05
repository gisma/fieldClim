#' Total radiation balance
#'
#' Calculate total radiation balance.
#'
#' @param ... Additional arguments.
#' @returns Total radiation balance in W/m\eqn{^2}.
#' @export
rad_bal <- function(...) {
  UseMethod("rad_bal")
}

#' @rdname rad_bal
#' @export
#' @references p45eq3.1
rad_bal.default <- function(...) {
  rad_sw_bal + rad_lw_bal
}

#' @rdname rad_bal
#' @param weather_station Object of class weather_station.
#' @export
rad_bal.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "lw_in", "lw_out")

  rad_lw_surface <- weather_station$measurements$lw_out
  rad_lw_atmospheric <- weather_station$measurements$lw_in
  rad_sw_radiation_balance <- rad_sw_radiation_balance(weather_station)

  rad_bal(rad_sw_radiation_balance, rad_lw_surface, rad_lw_atmospheric)
}

#' Shortwave radiation balance
#'
#' @param ... Additional arguments.
#' @returns W/m\eqn{^2}
#' @export
rad_sw_bal <- function(...) {
  UseMethod("rad_sw_bal")
}

#' @rdname rad_sw_bal
#' @export
#' @references p45eq3.1, p63eq3.18
rad_sw_bal.default <- function(...) {
  sw_in <- rad_sw_in()
  diffuse_in <- rad_diffuse_in()
  albedo <- 0.5
  sky_view <- terr_sky_view()
  terrain_view <- 1 - sky_view

  (sw_in + diffuse_in) * (1 - albedo + albedo * terrain_view - albedo^2 * terrain_view)
}

#' Shortwave incoming radiation
#'
#' Provide `slope` and `exposition` to perform topographic correction.
#'
#' @param ... Additional arguments.
#' @returns W/m\eqn{^2}.
#' @export
rad_sw_in <- function(...) {
  UseMethod("rad_sw_in")
}

#' @rdname rad_sw_in
#' @inheritParams trans_rayleigh
#' @inheritDotParams rad_sw_toa.default sol_const
#' @inheritDotParams trans_ozone.default ozone_column
#' @inheritDotParams trans_aerosol.default vis
#' @inheritDotParams terr_terrain_angle.default slope exposition
#' @export
#' @references p46eq3.3, p52eq3.8
rad_sw_in.default <- function(datetime, lon, lat, elev, temp, ...) {
  sw_toa <- rad_sw_toa(datetime, lon, lat, ...)
  elevation <- sol_elevation(datetime, lon, lat)
  elevation <- deg2rad(elevation)

  gas <- trans_gas(datetime, lon, lat, elev, temp)
  ozone <- trans_ozone(datetime, lon, lat, ...)
  rayleigh <- trans_rayleigh(datetime, lon, lat, elev, temp)
  vapor <- trans_vapor(datetime, lon, lat, elev, temp)
  aerosol <- trans_aerosol(datetime, lon, lat, elev, temp, ...)
  trans_total <- gas * ozone * rayleigh * vapor * aerosol

  terrain_angle <- terr_terrain_angle(datetime, lon, lat, ...)
  terrain_angle <- deg2rad(terrain_angle)

  sw_toa * 0.9751 * trans_total / sin(elevation) * cos(terrain_angle)
}

#' @rdname rad_sw_in
#' @export
#' @param weather_station Object of class weather_station.
#' @param trans_total Total transmittance of the atmosphere.
#' @param oz OPTIONAL. Needed if trans_total = NULL. Columnar ozone in cm.
#' Default is average global value.
#' @param vis OPTIONAL. Needed if trans_total = NULL. Meteorological visibility in km.
#' Default is the visibility on a clear day.
rad_sw_in.weather_station <- function(weather_station,
                                      trans_total = NULL,
                                      oz = 0.35, vis = 30, ...) {
  rad_sw_toa <- rad_sw_toa(weather_station)
  if (is.null(trans_total)) {
    trans_total <- trans_total(weather_station, oz = oz, vis = vis)
  }
  return(rad_sw_in(rad_sw_toa, trans_total))
}

#' Shortwave radiation at top of atmosphere
#'
#' Calculation of the shortwave radiation at the top of the atmosphere.
#'
#' @param ... Additional arguments.
#' @returns Shortwave radiation at top of atmosphere in W/m\eqn{^2}.
#' @export
rad_sw_toa <- function(...) {
  UseMethod("rad_sw_toa")
}

#' @rdname rad_sw_toa
#' @inheritParams sol_elevation
#' @param sol_const Solar constant in W/m\eqn{^2}
#' @export
#' @references p244
rad_sw_toa.default <- function(datetime, lon, lat, sol_const = 1368, ...) {
  eccentricity <- sol_eccentricity(datetime)
  elevation <- sol_elevation(datetime, lon, lat)
  elevation <- deg2rad(elevation)

  sol_const * eccentricity * sin(elevation)
}
#rad_sw_toa.default <- function(datetime, lat, lon, ...) {
#  if (!inherits(datetime, "POSIXt")) {
#    stop("datetime has to be of class POSIXt.")
#  }
#  sol_const <- 1368
#  sol_eccentricity <- sol_eccentricity(datetime)
#  sol_elevation <- sol_elevation(datetime, lat, lon)
#  rad_sw_toa <- sol_const * sol_eccentricity * sin(sol_elevation * (pi / 180))
#  return(rad_sw_toa)
#}

#' @rdname rad_sw_toa
#' @export
#' @param weather_station Object of class weather_station.
rad_sw_toa.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(rad_sw_toa(datetime, lat, lon))
}


#' Incoming diffused radiation
#'
#' @param ... Additional arguments.
#' @returns Description.
#' @export
rad_diffuse_in <- function(...) {
  UseMethod("rad_diffuse_in")
}

#' @rdname rad_diffuse_in
#' @inheritParams trans_vapor
#' @inheritDotParams trans_vapor
#' @inheritDotParams trans_ozone.default
#' @inheritDotParams rad_sw_toa sol_constant
#' @inheritDotParams terr_sky_view valley
#' @inheritDotParams terr_terrain_angle slope exposition
#' @export
#' @references p58eq3.14, p55eq3.9
rad_diffuse_in.default <- function(datetime, lon, lat, elev, temp, ...) {
  vapor <- trans_vapor(datetime, lon, lat, elev, temp)
  ozone <- trans_ozone(datetime, lon, lat, ...)
  sw_toa <- rad_sw_toa(datetime, lon, lat, ...)
  sw_in <- rad_sw_in(datetime, lon, lat, elev, temp)
  sky_view <- terr_sky_view(slope, ...)
  terrain_angle <- terr_terrain_angle(datetime, lon, lat, ...)
  terrain_angle <- deg2rad(terrain_angle)

  elevation <- sol_elevation(datetime, lon, lat)
  z <- 90 - elevation
  z <- deg2rad(z)

  0.5 * ((1 - (1 - vapor) - (1 - ozone)) * sw_toa - sw_in) *
  sky_view * (1 + cos(terrain_angle)^2 * sin(z)^3)
}

#' Long wave radiation balance
#'
#' @param ... Additional arguments.
#' @returns W/m\eqn{^2}.
#' @export
rad_lw_bal <- function(...) {
  UseMethod("rad_lw_bal")
}

#' @rdname rad_lw_bal
#' @inheritParams rad_lw_in
#' @inheritParams rad_lw_out
#' @inheritDotParams rad_lw_in.default sigma
#' @inheritDotParams rad_lw_out.default surface_type
#' @export
#' @references p68eq3.25
rad_lw_bal.default <- function(temp, rh, surface_temp, ...) {
  lw_in <- rad_lw_in(temp, rh, ...)
  lw_out <- rad_lw_out(surface_temp, ...)
  
  lw_in - lw_out
}

#' Longwave radiation of the atmosphere
#'
#' Calculation of the longwave radiation of the atmosphere.
#'
#' The second part of the equation related to the surrounding terrain is not included.
#'
#' @param ... Additional arguments.
#' @returns Atmospheric radiation in W/m\eqn{^2}.
#' @export
rad_lw_in <- function(...) {
  UseMethod("rad_lw_in")
}

#' @rdname rad_lw_in
#' @inheritParams rad_emissivity_air
#' @inheritDotParams rad_emissivity_air.default elev p p0
#' @inheritDotParams terr_sky_view.default slope valley
#' @param sigma Stefan-Boltzmann constant
#'   with the default 5.6993e-8 W/m\eqn{^2}/K$^{4}$
#' @export
#' @references p66eq3.24
rad_lw_in.default <- function(temp, rh, sigma = sigma_default, ...) {
  emissivity_air <- rad_emissivity_air(temp, rh, ...)
  sky_view <- terr_sky_view(...)
  temp <- c2k(temp)
  
  emissivity_air * sigma * temp^4 * sky_view
}
#rad_lw_in.default <- function(hum, t, ...) {
#  sigma <- 5.6693e-8
#  gs <- (0.594 + 0.0416 * sqrt(pres_vapor_p(hum, t))) * sigma * (t + 273.15)^4
#  return(gs)
#}

#' @rdname rad_lw_in
#' @export
#' @param weather_station Object of class weather_station.
rad_lw_in.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "t2", "hum2")
  hum <- weather_station$measurements$hum2
  t <- weather_station$measurements$t2

  return(rad_lw_in(hum, t))
}

#' Emissivity of the atmosphere
#'
#' Calculation of the emissivity of the atmosphere.
#'
#' @param ... Additional arguments.
#' @returns Emissivity of the atmosphere (0-1).
#' @export
rad_emissivity_air <- function(...) {
  UseMethod("rad_emissivity_air")
}

#' @rdname rad_emissivity_air
#' @param temp Air temperature in °C.
#' @param rh relative humidity.
#' @param elev Meters above sea level in m.
#' @param p OPTIONAL. Air pressure in hPa.
#' @param p0 Standard air pressure in hPa.
#' @export
#' @references p67eq3.23, 3.24, 3.25
rad_emissivity_air.default <- function(temp, rh,
    elev = 0, p = p0_default, p0 = p0_default, ...) {
  temp <- c2k(temp) - 0.0065 * elev
  sat_vapor_p <- pres_sat_vapor_p(temp, ...)
  vapor_p <- rh * sat_vapor_p
  
  (1.24 * vapor_p / temp)^(1 / 7) * p / p0
}
#rad_emissivity_air.default <- function(t, elev, hum, p = NULL, ...) {
#  if (is.null(p)) p <- pres_p(elev, t)

#   Calculate temperature adjusted to elevation (with saturated adiabatic lapse rate)
#  t_adj <- t * (0.0065 * elev)

#   Calculate saturated vapor pressure with adjusted temperature
#  svp <- hum_sat_vapor_pres(t_adj)

#   Calculate vapor pressure with saturated vapor pressure and measured humidity
#  e <- hum * svp

#   Calculate emissivity
#  eat <- ((1.24 * e / (t_adj + 273.15))**1 / 7) * (p / 1013.25)

#  return(eat)
#}

#' @rdname rad_emissivity_air
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
rad_emissivity_air.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2", "elevation", "p1", "p2", "hum1", "hum2")
  if (!height %in% c("upper", "lower")) {
    stop("'height' must be either 'lower' or 'upper'.")
  }

  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  p <- weather_station$measurements[[paste0("p", height_num)]]
  elev <- weather_station$location_properties$elevation
  hum <- weather_station$measurements[[paste0("hum", height_num)]]

  return(rad_emissivity_air(t, elev, p, hum))
}

#' Longwave radiation of the surface
#'
#' Calculates emissions of a surface.
#'
#' If a weather_station object is given, the lower air temperature will be used
#' instead of the surface temperature.
#'
#' @param ... Additional arguments.
#' @returns Emissions in W/m\eqn{^2}.
#' @export
rad_lw_out <- function(...) {
  UseMethod("rad_lw_out")
}

#' @rdname rad_lw_out
#' @param surface_temp Surface temperature in °C.
#' @param surface_type Surface type for which a specific emissivity will be selected.
#' @param sigma Stefan-Boltzmann constant
#'   with the default 5.6993e-8 W/m\eqn{^2}/K$^{4}$
#' Default is 'field' as surface type.
#' @export
#' @references p66eq3.20
rad_lw_out.default <- function(surface_temp, surface_type = "field", sigma = sigma_default, ...) {
  surface_properties <- surface_properties
  emissivity <- surface_properties[which(surface_properties$surface_type == surface_type), ]$emissivity
  surface_temp <- c2k(surface_temp)
  
  emissivity * sigma * surface_temp^4
}

#' @rdname rad_lw_out
#' @export
#' @param weather_station Object of class weather_station.
#' @param surface_type Surface type for which a specific emissivity will be selected.
#' Default is 'field' as surface type.
rad_lw_out.weather_station <- function(weather_station, surface_type = "field", ...) {
  if (exists("weather_station$measurements$t_surface")) {
    t <- weather_station$measurements$t_surface
  } else {
    t <- weather_station$measurements$t1
    warning("There is no surface temperature available in this weather_station object. The 2 m air temperature will be used instead.")
  }

  surface_properties <- surface_properties
  emissivity <- surface_properties[which(surface_properties$surface_type == surface_type), ]$emissivity

  return(rad_lw_out(t, surface_type))
}