#' Soil heat flux
#'
#' Calculates soil heat flux from measurements in two different
#' depths and thermal conductivity of the soil.
#'
#' Negative values signify flux towards the atmosphere, positive values signify flux into the soil.
#'
#' @param ... Additional arguments.
#' @returns Soil heat flux in W * m^(-2).
#' @export
soil_heat_flux <- function(...) {
  UseMethod("soil_heat_flux")
}

#' @rdname soil_heat_flux
#' @inheritDotParams soil_thermal_cond.default
#' @param ts1 Upper soil temperature (closest to the surface) in °C.
#' @param ts2 Lower soil temperature in °C.
#' @param depth1 Depth of upper measurement (closest to the surface) in m.
#' @param depth2 Depth of lower measurement in m.
#' @export
#' @references p71eq4.2.
soil_heat_flux.default <- function(ts1, ts2, depth1, depth2, ...) {
  thermal_cond <- soil_thermal_cond(...)
  
  thermal_cond * (ts1 - ts2) / (depth1 - depth2)
}

#' @rdname soil_heat_flux
#' @export
#' @param weather_station Object of class weather_station.
soil_heat_flux.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "ts1", "ts2", "depth1", "depth2")
  ts1 <- weather_station$measurements$ts1
  ts2 <- weather_station$measurements$ts2
  depth1 <- weather_station$properties$depth1
  depth2 <- weather_station$properties$depth2
  
  soil_heat_flux(ts1, ts2, depth1, depth2, ...)
}

#' Soil thermal conductivity
#'
#' Calculates soil thermal conductivity (W/m K) from soil moisture (Cubic meter/cubic meter) and texture.
#'
#' Works by linearly interpolating thermal conductivity based on measured data.
#'
#' @param ... Additional arguments.
#' @returns Soil thermal conductivity in W/m K.
#' @export
soil_thermal_cond <- function(...) {
  UseMethod("soil_thermal_cond")
}

#' @rdname soil_thermal_cond
#' @param texture Soil texture. Either `sand` (default), `peat` or `clay`.
#' @param moisture Soil moisture in Cubic meter/cubic meter
#' @export
#' @references p254.
soil_thermal_cond.default <- function(texture = "sand", moisture = 0, ...) {
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
#' @param weather_station Object of class weather_station.
#' @export
soil_thermal_cond.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "texture", "moisture")
  texture <- weather_station$location_properties$texture
  moisture <- weather_station$measurements$moisture
  
  soil_thermal_cond(moisture, texture, ...)
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
#' @export
#'
soil_heat_cap <- function(...) {
  UseMethod("soil_heat_cap")
}

#' @rdname soil_heat_cap
#' @param moisture Soil moisture in Cubic meter/cubic meter
#' @param texture Soil texture. Either "sand", "peat" or "clay".
#' @importFrom stats approx
#' @export
#' @references p254.
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
#' @param weather_station Object of class weather_station.
#' @export
#'
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
#' @export
#'
soil_attenuation <- function(...) {
  UseMethod("soil_attenuation")
}

#' @rdname soil_attenuation
#' @export
#' @param moisture Soil moisture in Cubic meter/cubic meter
#' @param texture Soil texture. Either "sand", "peat" or "clay".
#' @references p253.
soil_attenuation.default <- function(moisture, texture = "sand", ...) {
  thermal_cond <- soil_thermal_cond(moisture, texture)
  vol_heat_cap <- soil_heat_cap(moisture, texture)
  soil_att <- sqrt(thermal_cond / (vol_heat_cap * 10^6 * pi) * 86400)
  soil_att
}

#' @rdname soil_attenuation
#' @export
#' @param weather_station Object of class weather_station.
soil_attenuation.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "moisture", "texture")
  moisture <- weather_station$measurements$moisture
  texture <- weather_station$location_properties$texture
  return(soil_attenuation(moisture, texture))
}
