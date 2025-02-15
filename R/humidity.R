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
#' @param rh Relative humidity in %.
#' @param temp Temperature in °C.
#' @param elev Elevation above sea level in m.
#' @export
#' @references Bendix 2004, p. 262.
hum_specific.default <- function(rh, temp, elev, ...) {
  p_vapor <- pres_vapor_p(temp, rh)
  p <- pres_p(elev, temp, ...)
  0.622 * (p_vapor / p)
}

#' @rdname hum_specific
#' @export
#' @inheritParams build_weather_station
hum_specific.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "temp", "rh", "elev")
  rh <- weather_station$rh
  temp <- weather_station$temp
  elev <- weather_station$elev
  return(hum_specific(rh, temp, elev))
}


#' Absolute humidity
#'
#' Calculates absolute humidity from vapor pressure and air temperature.
#'
#' @param ... Additional arguments.
#' @returns Absolute humidity in kg/m\eqn{^3}.
#' @export
#'
hum_absolute <- function(...) {
  UseMethod("hum_absolute")
}

#' @rdname hum_absolute
#' @param rh Relative humidity in %.
#' @param temp Temperature in °C.
#' @export
#' @references Bendix 2004, p. 262.
hum_absolute.default <- function(rh, temp, ...) {
  p_vapor <- pres_vapor_p(temp, rh)
  temp <- c2k(temp) # to Kelvin
  (0.21668 * p_vapor) / temp
}

#' @rdname hum_absolute
#' @export
#' @inheritParams build_weather_station
hum_absolute.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "temp", "rh")
  rh <- weather_station$rh
  temp <- weather_station$temp
  return(hum_absolute(rh, temp))
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
#' @param temp Air temperature in °C.
#' @references Bendix 2004, p. 261.
hum_evap_heat.default <- function(temp, ...) {
  (2.5008 - 0.002372 * temp) * 10^6
}

#' @rdname hum_evap_heat
#' @export
#' @inheritParams build_weather_station
hum_evap_heat.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "temp")
  temp <- weather_station$temp
  return(hum_evap_heat(temp))
}


#' Precipitable water
#'
#' Selects reference temperature and pressure based on location and season.
#' Then calculates precipitable water.
#'
#' Latitude <= 30 degrees is defined as tropic; <= 60 is temperate; others is subarctic.
#' Summer is defined as April to September in the northern hemisphere.
#'
#' @inheritParams build_weather_station
#' @returns \eqn{cm \cdot grams}.
#' @export
hum_precipitable_water <- function(...) {
  UseMethod("hum_precipitable_water")
}

#' @rdname hum_precipitable_water
#' @inheritParams build_weather_station
#' @inheritDotParams pres_p.default g rl
#' @export
#' @references Bendix 2004, p. 246. Column name "subarctic_summer" and "subarctic_winter" were switched.
hum_precipitable_water.default <- function(datetime, lat, elev, temp, ...) {
  df <- data.frame(
    t0 = c(300, 294, 272.2, 287, 257.1),
    pwst = c(4.1167, 2.9243, 0.8539, 2.0852, 0.4176),
    row.names = c("tropic", "temperate_summer", "temperate_winter", "subarctic_summer", "subarctic_winter")
  )

  temp_standard <- c()
  pw_standard <- c()

  for (i in seq_along(datetime)) {
    if (abs(lat) <= 30) { # tropic
      temp_standard <- df["tropic", "t0"]
      pw_standard <- df["tropic", "pwst"]
    } else if ((abs(lat) <= 60) && (lat > 0)) { # temperate, northern hemisphere
      if ((datetime[i]$mon + 1) %in% seq(4, 9)) {
        temp_standard[i] <- df["temperate_summer", "t0"]
        pw_standard[i] <- df["temperate_summer", "pwst"]
      } else {
        temp_standard[i] <- df["temperate_winter", "t0"]
        pw_standard[i] <- df["temperate_winter", "pwst"]
      }
    } else if ((abs(lat) <= 60) && (lat < 0)) { # temperate, southern hemisphere
      if ((datetime[i]$mon + 1) %in% seq(4, 9)) {
        temp_standard[i] <- df["temperate_winter", "t0"]
        pw_standard[i] <- df["temperate_winter", "pwst"]
      } else {
        temp_standard[i] <- df["temperate_summer", "t0"]
        pw_standard[i] <- df["temperate_summer", "pwst"]
      }
    } else if (lat > 0) { # subarctic, northern hemisphere
      if ((datetime[i]$mon + 1) %in% seq(4, 9)) {
        temp_standard[i] <- df["subarctic_summer", "t0"]
        pw_standard[i] <- df["subarctic_summer", "pwst"]
      } else {
        temp_standard[i] <- df["subarctic_winter", "t0"]
        pw_standard[i] <- df["subarctic_winter", "pwst"]
      }
    } else if (lat < 0) { # subarctic, southern hemisphere
      if ((datetime[i]$mon + 1) %in% seq(4, 9)) {
        temp_standard[i] <- df["subarctic_winter", "t0"]
        pw_standard[i] <- df["subarctic_winter", "pwst"]
      } else {
        temp_standard[i] <- df["subarctic_summer", "t0"]
        pw_standard[i] <- df["subarctic_summer", "pwst"]
      }
    }
  }

  p <- pres_p(elev, temp, ...)
  p0 <- p0_default # will be cancled in pres_p

  pw_standard * (p / p0) * (temp_standard / temp)^0.5
}

#' @rdname hum_precipitable_water
#' @inheritParams build_weather_station
#' @export
hum_precipitable_water.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(hum_precipitable_water.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }

  hum_precipitable_water(datetime, lat, elev, temp, ...)
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
#' @inheritParams build_weather_station
#' @export
hum_moisture_gradient.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "z1", "z2", "t1", "t2", "hum1", "hum2", "elev")
  hum1 <- weather_station$hum1
  hum2 <- weather_station$hum2
  t1 <- weather_station$t1
  t2 <- weather_station$t2
  z1 <- weather_station$z1
  z2 <- weather_station$z2
  elev <- weather_station$elev
  return(hum_moisture_gradient(hum1, hum2, t1, t2, z1, z2, elev))
}
