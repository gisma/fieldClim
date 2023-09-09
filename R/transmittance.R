#' Transmittance due to gas
#'
#' Calculates transmittance due to O\eqn{_2} and CO\eqn{_2}.
#'
#' @inheritParams build_weather_station
#' @returns Ratio from 0 to 1, unitless.
#' @export
trans_gas <- function(...) {
  UseMethod("trans_gas")
}

#' @rdname trans_gas
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 246.
trans_gas.default <- function(datetime, lon, lat, elev, temp, ...) {
  air_mass_abs <- trans_air_mass_abs(datetime, lon, lat, elev, temp)
  
  exp(-0.0127 * air_mass_abs^0.26)
}

#' @rdname trans_gas
#' @inheritParams build_weather_station
#' @export
trans_gas.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(trans_gas.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  trans_gas(datetime, lon, lat, elev, temp)
}


#' Absolute optical air mass
#'
#' @inheritParams build_weather_station
#' @returns Unitless.
#' @export
trans_air_mass_abs <- function(...) {
  UseMethod("trans_air_mass_abs")
}

#' @rdname trans_air_mass_abs
#' @inheritParams build_weather_station
#' @inheritDotParams build_weather_station.default g rl
#' @export
#' @references Bendix 2004, p. 247.
trans_air_mass_abs.default <- function(datetime, lon, lat, elev, temp, ...) {
  air_mass_rel <- trans_air_mass_rel(datetime, lon, lat)
  p <- pres_p(elev, temp)
  p0 <- p0_default # will be cancled in pres_p
  
  air_mass_rel * (p / p0)
}

#' @rdname trans_air_mass_abs
#' @inheritParams build_weather_station
#' @export
trans_air_mass_abs.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(trans_air_mass_abs.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  trans_air_mass_abs(datetime, lon, lat, elev, temp)
}

#' Relative optical air mass
#'
#' @inheritParams build_weather_station
#' @returns Unitless.
#' @export
trans_air_mass_rel <- function(...) {
  UseMethod("trans_air_mass_rel")
}

#' @rdname trans_air_mass_rel
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 246.
trans_air_mass_rel.default <- function(datetime, lon, lat, ...) {
  elevation <- sol_elevation(datetime, lon, lat)
  
  out <- 1 / (sin(deg2rad(elevation)) + 1.5 * elevation^-0.72)
}

#' @rdname trans_air_mass_rel
#' @inheritParams build_weather_station
#' @export
trans_air_mass_rel.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(trans_air_mass_rel.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  trans_air_mass_rel(datetime, lon, lat)
}

#' Transmittance due to ozone
#'
#' @inheritParams build_weather_station
#' @returns Ratio from 0 to 1, unitless.
#' @export
trans_ozone <- function(...) {
  UseMethod("trans_ozone")
}

#' @rdname trans_ozone
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 245.
trans_ozone.default <- function(datetime, lon, lat, ...,
    ozone_column = ozone_column_default) {
  air_mass_rel <- trans_air_mass_rel(datetime, lon, lat)
  x <- ozone_column * air_mass_rel
  
  1 - (
    0.1611 * x * (1 + 139.48 * x)^-0.3035 -
    0.002715 * x * (1 + 0.044 * x + 0.0003 * x^2)^-1
  )
}

#' @rdname trans_ozone
#' @inheritParams build_weather_station
#' @export
trans_ozone.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(trans_ozone.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  trans_ozone(datetime, lon, lat, ...)
}


#' Transmittance due to rayleigh scattering
#'
#' @inheritParams build_weather_station
#' @returns Ratio from 0 to 1, unitless.
#' @export
trans_rayleigh <- function(...) {
  UseMethod("trans_rayleigh")
}

#' @rdname trans_rayleigh
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 245.
trans_rayleigh.default <- function(datetime, lon, lat, elev, temp, ...) {
  air_mass_abs <- trans_air_mass_abs(datetime, lon, lat, elev, temp)
  
  exp(-0.0903 * air_mass_abs^0.84 * (1 + air_mass_abs - air_mass_abs^1.01))
}

#' @rdname trans_rayleigh
#' @inheritParams build_weather_station
#' @export
trans_rayleigh.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(trans_rayleigh.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  trans_rayleigh(datetime, lon, lat, elev, temp)
}

#' Transmittance due to water vapor
#'
#' @rdname trans_vapor
#' @inheritParams build_weather_station
#' @returns Ratio from 0 to 1, unitless.
#' @export
trans_vapor <- function(...) {
  UseMethod("trans_vapor")
}

#' @rdname trans_vapor
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 245.
trans_vapor.default <- function(datetime, lon, lat, elev, temp, ...) {
  precipitable_water <- hum_precipitable_water(datetime, lat, elev, temp)
  air_mass_rel <- trans_air_mass_rel(datetime, lon, lat)
  x <- precipitable_water * air_mass_rel
  
  1 - 2.4959 * x * ((1 + 79.034 * x)^0.6828 + 6.385 * x)^-1
}

#' @rdname trans_vapor
#' @inheritParams build_weather_station
#' @export
trans_vapor.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(trans_vapor.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  trans_vapor(datetime, lon, lat, elev, temp)
}

#' Transmittance due to aerosols
#'
#' Transmittance due to fine particles in the air.
#'
#' Visibility is used to linearly interpolate aerosol optical thickness.
#'
#' @inheritParams build_weather_station
#' @returns Ratio from 0 to 1, unitless.
#' @export
trans_aerosol <- function(...) {
  UseMethod("trans_aerosol")
}

#' @rdname trans_aerosol
#' @inheritParams build_weather_station
#' @inheritDotParams build_weather_station
#' @export
#' @references Bendix 2004, p. 246.
trans_aerosol.default <- function(datetime, lon, lat, elev, temp, ...,
    vis = vis_default) {
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
#' @inheritParams build_weather_station
#' @export
trans_aerosol.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(trans_aerosol.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  trans_aerosol(datetime, lon, lat, elev, temp)
}