#' Soil heat flux
#'
#' Calculates soil heat flux from measurements in two different
#' depths and thermal conductivity of the soil.
#'
#' Negative values signify flux towards the atmosphere, positive values signify flux into the soil.
#'
#' @inheritParams build_weather_station
#' @returns eqn{W \cdot m^-2}.
#' @export
soil_heat_flux <- function(...) {
  UseMethod("soil_heat_flux")
}

#' @rdname soil_heat_flux
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 71 eq. 4.2.
soil_heat_flux.default <- function(texture, moisture,
    soil_temp1, soil_temp2, soil_depth1, soil_depth2, ...) {
  thermal_cond <- soil_thermal_cond(texture, moisture)
  
  thermal_cond * (soil_temp1 - soil_temp2) / (soil_depth1 - soil_depth2)
}

#' @rdname soil_heat_flux
#' @export
#' @inheritParams build_weather_station
soil_heat_flux.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(soil_heat_flux.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  soil_heat_flux(texture, moisture,
    soil_temp1, soil_temp2, soil_depth1, soil_depth2)
}

#' Soil thermal conductivity
#'
#' Calculates soil thermal conductivity from soil texture and soil moisture.
#'
#' Works by linearly interpolating thermal conductivity based on measured data.
#'
#' @inheritParams build_weather_station
#' @returns Soil thermal conductivity in W/m/K.
#' @export
soil_thermal_cond <- function(...) {
  UseMethod("soil_thermal_cond")
}

#' @rdname soil_thermal_cond
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 254.
soil_thermal_cond.default <- function(texture, moisture, ...) {
  # convert moisture from [cubic m/cubic m] to [Vol-%]
  moisture <- moisture * 100

  if (texture == "sand") {
    y <- c(0.269, 1.46, 1.98, 2.18, 2.31, 2.49, 2.58)
    x <- c(0, 5, 10, 15, 20, 30, 43)
  } else if (texture == "clay") {
    y <- c(0.276, 0.586, 1.1, 1.43, 1.57, 1.74, 1.95)
    x <- c(0, 5, 10, 15, 20, 30, 43)
  } else if (texture == "peat") {
    y <- c(0.033, 0.042, 0.130, 0.276, 0.421, 0.478, 0.528)
    x <- c(0, 10, 30, 50, 70, 80, 90)
  } else {
    stop("Texture not available. Input has to be either 'sand', 'peat' or 'clay'")
  }

  # linear interpolation of values
  approx(x, y, moisture)$y
}

#' @rdname soil_thermal_cond
#' @inheritParams build_weather_station
#' @export
soil_thermal_cond.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(soil_thermal_cond.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  soil_thermal_cond(texture, moisture)
}














#' Soil volumetric heat capacity
#'
#' Calculates soil volumetric heat capacity (MJ / (m\eqn{^3} * K)) from soil moisture (Cubic meter/cubic meter) and texture.
#'
#' Works by linearly interpolating volumetric heat capacity based on measured data.
#'
#' @rdname soil_heat_cap
#' @param ... Additional arguments.
#' @returns Numeric vector with volumetric heat capacity in  MJ/ (m\eqn{^3} * K).
#' @noRd
soil_heat_cap <- function(...) {
  UseMethod("soil_heat_cap")
}

#' @rdname soil_heat_cap
#' @param moisture Soil moisture in Cubic meter/cubic meter
#' @param texture Soil texture. Either "sand", "peat" or "clay".
#' @importFrom stats approx
#' @export
#' @noRd
#' @references Bendix 2004, p. 254.
soil_heat_cap.default <- function(moisture, texture = "sand", ...) {
  # convert moisture from [cubic m/cubic m] to [Vol-%]
  moisture <- moisture * 100

  if (texture == "sand") {
    y <- c(1.17, 1.38, 1.59, 1.8, 2.0, 2.42, 2.97)
    x <- c(0, 5, 10, 15, 20, 30, 43)
  } else if (texture == "clay") {
    y <- c(1.19, 1.4, 1.61, 1.82, 2.03, 2.45, 2.99)
    x <- c(0, 5, 10, 15, 20, 30, 43)
  } else if (texture == "peat") {
    y <- c(0.25, 0.67, 1.51, 2.35, 3.19, 3.61, 4.03)
    x <- c(0, 10, 30, 50, 70, 80, 90)
  } else {
    stop("Texture not available. Input either 'sand', 'peat' or 'clay'")
  }

  # linear interpolation of values
  vol_heat <- approx(x, y, xout = moisture, yleft = NA, yright = y[7])
  vol_heat$y
}


#' @rdname soil_heat_cap
#' @inheritParams sol_julian_day
#' @export
#' @noRd
soil_heat_cap.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "moisture", "texture")
  moisture <- weather_station$measurements$moisture
  texture <- weather_station$location_properties$texture
  return(soil_heat_cap(moisture, texture))
}

#' Soil attenuation length
#'
#' Calculates soil attenuation length.
#'
#' @rdname soil_attenuation
#' @param ... Additional arguments.
#' @returns Soil attenuation length in m.
#' @noRd
soil_attenuation <- function(...) {
  UseMethod("soil_attenuation")
}

#' @rdname soil_attenuation
#' @param moisture Soil moisture in Cubic meter/cubic meter
#' @param texture Soil texture. Either "sand", "peat" or "clay".
#' @export
#' @noRd
#' @references Bendix 2004, p. 253.
soil_attenuation.default <- function(moisture, texture = "sand", ...) {
  thermal_cond <- soil_thermal_cond(moisture, texture)
  vol_heat_cap <- soil_heat_cap(moisture, texture)
  soil_att <- sqrt(thermal_cond / (vol_heat_cap * 10^6 * pi) * 86400)
  soil_att
}

#' @rdname soil_attenuation
#' @inheritParams sol_julian_day
#' @export
#' @noRd
soil_attenuation.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "moisture", "texture")
  moisture <- weather_station$measurements$moisture
  texture <- weather_station$location_properties$texture
  return(soil_attenuation(moisture, texture))
}