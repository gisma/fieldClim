#' Vapor pressure
#'
#' Calculates vapor pressure from relative humidity and saturation vapor pressure.
#'
#' @param ... Additional arguments.
#' @returns Vapor pressure in hPa.
#' @export
hum_vapor_pres <- function(...) {
  UseMethod("hum_vapor_pres")
}

#' @rdname hum_vapor_pres
#' @export
#' @param hum Relative humidity in %.
#' @param t Air temperature in °C.
hum_vapor_pres.default <- function(hum, t, ...) {
  sat_vapor_p <- pres_sat_vapor_p(t)
  (hum / 100) * sat_vapor_p
}

#' @rdname hum_vapor_pres
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_vapor_pres.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2", "hum1", "hum2")
  if (!height %in% c("upper", "lower")) {
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  hum <- weather_station$measurements[[paste0("hum", height_num)]]
  return(hum_vapor_pres(hum, t))
}

#' Specific humidity
#'
#' Calculates specific humidity from vapor pressure and air pressure.
#'
#' @param ... Additional arguments.
#' @returns Specific humidity in kg/kg.
#' @export
#'
hum_specific <- function(...) {
  UseMethod("hum_specific")
}

#' @rdname hum_specific
#' @param hum Relative humidity in %.
#' @param t Temperature in °C.
#' @param elev Elevation above sea level in m.
#' @export
#' @references p262.
hum_specific.default <- function(hum, t, elev, ...) {
  p_vapor <- hum_vapor_pres(hum, t)
  p <- pres_p(elev, t)
  0.622 * (p_vapor / p)
}

#' @rdname hum_specific
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_specific.weather_station <- function(weather_station, height, ...) {
  check_availability(weather_station, "t1", "t2", "hum1", "hum2", "elevation")
  if (!height %in% c("upper", "lower")) {
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  hum <- weather_station$measurements[[paste0("hum", height_num)]]
  t <- weather_station$measurements[[paste0("t", height_num)]]
  elev <- weather_station$location_properties$elevation
  return(hum_specific(hum, t, elev))
}


#' Absolute humidity
#'
#' Calculates absolute humidity from vapor pressure and air temperature.
#'
#' @param ... Additional arguments.
#' @returns Absolute humidity in kg/m³.
#' @export
#'
hum_absolute <- function(...) {
  UseMethod("hum_absolute")
}

#' @rdname hum_absolute
#' @param hum Relative humidity in %.
#' @param t Air temperature in °C.
#' @export
#' @references p262.
hum_absolute.default <- function(hum, t, ...) {
  p_vapor <- hum_vapor_pres(hum, t)
  t <- c2k(t)
  (0.21668 * p_vapor) / t
}

#' @rdname hum_absolute
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_absolute.weather_station <- function(weather_station, height, ...) {
  check_availability(weather_station, "hum1", "hum2", "t1", "t2")
  if (!height %in% c("upper", "lower")) {
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  hum <- weather_station$measurements[[paste0("hum", height_num)]]
  t <- weather_station$measurements[[paste0("t", height_num)]]
  return(hum_absolute(hum, t))
}

#' Enthalpy of vaporization
#'
#' Calculates heat of evaporation for water from air temperature.
#'
#' @param ... Additional arguments.
#' @returns Enthalpy of vaporization in J/kg.
#' @export
#'
hum_evap_heat <- function(...) {
  UseMethod("hum_evap_heat")
}

#' @rdname hum_evap_heat
#' @export
#' @param t Air temperature in °C.
#' @references p261.
hum_evap_heat.default <- function(t, ...) {
  (2.5008 - 0.002372 * t) * 10^6
}

#' @rdname hum_evap_heat
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_evap_heat.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2")
  if (!height %in% c("upper", "lower")) {
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  return(hum_evap_heat(t))
}


#' Total precipitable water
#'
#' Estimates total precipitable water in the atmosphere.
#' It uses a moist adiabatic temperature gradient which might not be
#' suitable for every application.
#'
#' @param ... Additional arguments.
#' @returns Total precipitable water in cm (grams).
#' @export
hum_precipitable_water <- function(...) {
  UseMethod("hum_precipitable_water")
}

#' @rdname hum_precipitable_water
#' @inheritParams pres_p
#' @param datetime Datetime
#' @param lat Latitude
#' @param p0 Standard pressure
#' @export
#' @references p246
hum_precipitable_water.default <- function(datetime, lat, elev, temp, p0 = p0_default, ...) {
  if (abs(lat) <= 30) { # tropic
    temp_standard <- 300
    pw_standard <- 4.1167
  } else if ((abs(lat) <= 60) && (lat > 0)) { # temperate, north hemisphere
    if (datetime$mon + 1 %in% seq(4, 9)) {
      temp_standard <- 294
      pw_standard <- 2.9243
    } else {
      temp_standard <- 272.2
      pw_standard <- 0.8539
    }
  } else if ((abs(lat) <= 60) && (lat < 0)) { # temperate, south hemisphere
    if (datetime$mon + 1 %in% seq(4, 9)) {
      temp_standard <- 272.2
      pw_standard <- 0.8539
    } else {
      temp_standard <- 294
      pw_standard <- 2.9243
    }
  } else if (lat > 0) { # subarctic, north hemisphere
    if (datetime$mon + 1 %in% seq(4, 9)) {
      temp_standard <- 287
      pw_standard <- 2.0852
    } else {
      temp_standard <- 257.1
      pw_standard <- 0.4176
    }
  } else if (lat > 0) { # subarctic, south hemisphere
    if (datetime$mon + 1 %in% seq(4, 9)) {
      temp_standard <- 257.1
      pw_standard <- 0.4176
    } else {
      temp_standard <- 287
      pw_standard <- 2.0852
    }
  }
  p <- pres_p(elev, temp, ...)
  
  pw_standard * (p / p0) * (temp_standard / temp)^0.5
}
#hum_precipitable_water.default <- function(p, t, elev, ...) {
#  p0 <- 1013.25 # Pressure standard atmosphere
#  t <- t + 273.15 # °C in K
#  cof <- (elev / 100) * 0.6 # average moist adiabatic T-gradient, might have to be adjusted
#  t0 <- t + cof
#  pw_st <- 0.0000004 * exp(0.0538 * t0)
#  pw <- pw_st * (p / p0) * (t0 / t)**0.5
#  return(pw)
#}

#' @rdname hum_precipitable_water
#' @export
#' @param weather_station Object of class weather_station.
#' @param height Height of measurement. "lower" or "upper".
hum_precipitable_water.weather_station <- function(weather_station, height = "lower", ...) {
  check_availability(weather_station, "t1", "t2", "elevation")
  if (!height %in% c("upper", "lower")) {
    stop("'height' must be either 'lower' or 'upper'.")
  }
  height_num <- which(height == c("lower", "upper"))
  t <- weather_station$measurements[[paste0("t", height_num)]]
  elev <- weather_station$location_properties$elevation
  return(hum_precipitable_water(t, elev))
}


#' Moisture gradient
#'
#' Calculates moisture gradient.
#'
#' @param ... Additional arguments.
#' @returns Moisture gradient.
#' @export
#'
hum_moisture_gradient <- function(...) {
  UseMethod("hum_moisture_gradient")
}

#' @rdname hum_moisture_gradient
#' @export
#' @param hum1 Relative humidity at lower height in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param t1 Air temperature at lower height in °C.
#' @param t2 Air temperature at upper height in °C.
#' @param z1 Lower measurement height in m.
#' @param z2 Upper measurement height in m.
#' @param elev Elevation above sea level in m.
hum_moisture_gradient.default <- function(hum1, hum2, t1, t2, z1 = 2, z2 = 10, elev, ...) {
  # specific humidity
  sh1 <- hum_specific(hum1, t1, elev)
  sh2 <- hum_specific(hum2, t2, elev)
  (sh2 - sh1) / (z2 - z1)
}

#' @rdname hum_moisture_gradient
#' @param weather_station Object of class weather_station.
#' @export
hum_moisture_gradient.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "z1", "z2", "t1", "t2", "hum1", "hum2", "elevation")
  hum1 <- weather_station$measurements$hum1
  hum2 <- weather_station$measurements$hum2
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  elev <- weather_station$location_properties$elevation
  return(hum_moisture_gradient(hum1, hum2, t1, t2, z1, z2, elev))
}
