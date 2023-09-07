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
rad_bal.default <- function(datetime, lon, lat, elev, temp, rh, surface_temp, ...) {
  sw_bal <- rad_sw_bal(datetime, lon, lat, elev, temp)
  lw_bal <- rad_lw_bal(temp, rh, surface_temp)
  
  sw_bal + lw_bal
}

#' @rdname rad_bal
#' @inheritParams sol_julian_day
#' @export
rad_bal.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_bal.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_bal(datetime, lon, lat, elev, temp, rh, surface_temp, weather_station)
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
rad_sw_bal.default <- function(datetime, lon, lat, elev, temp, ...) {
  sw_in <- rad_sw_in(datetime, lon, lat, elev, temp, ...)
  sw_out <- rad_sw_out(datetime, lon, lat, elev, temp, ...)
  diffuse_in <- rad_diffuse_in(datetime, lon, lat, elev, temp, ...)
  diffuse_out <- rad_diffuse_in(datetime, lon, lat, elev, temp, ...)
  
  sw_in - sw_out + diffuse_in - diffuse_out
}

#' @rdname rad_sw_bal
#' @inheritParams sol_julian_day
#' @export
rad_sw_bal.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_sw_bal.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_sw_bal(datetime, lon, lat, elev, temp, weather_station)
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
rad_sw_in.default <- function(datetime, lon, lat, elev, temp, ...,
    surface_type = "field") {
  sw_toa <- rad_sw_toa(datetime, lon, lat, ...)
  elevation <- sol_elevation(datetime, lon, lat)
  
  gas <- trans_gas(datetime, lon, lat, elev, temp, ...)
  ozone <- trans_ozone(datetime, lon, lat, ...)
  rayleigh <- trans_rayleigh(datetime, lon, lat, elev, temp, ...)
  vapor <- trans_vapor(datetime, lon, lat, elev, temp, ...)
  aerosol <- trans_aerosol(datetime, lon, lat, elev, temp, ...)
  trans_total <- gas * ozone * rayleigh * vapor * aerosol

  terrain_angle <- terr_terrain_angle(datetime, lon, lat, ...)
  
  albedo <- surface_properties[which(surface_properties$surface_type == surface_type), ]$albedo
  terrain_view <- 1 - terr_sky_view(...)

  elevation <- deg2rad(elevation)
  terrain_angle <- deg2rad(terrain_angle)
  
  out <- sw_toa * 0.9751 * trans_total / sin(elevation) * cos(terrain_angle)
  out * (1 + albedo * terrain_view)
}

#' @rdname rad_sw_in
#' @inheritParams sol_julian_day
#' @export
rad_sw_in.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_sw_in.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_sw_in(datetime, lon, lat, elev, temp, weather_station)
}


#' Shortwave radiation at top of atmosphere
#'
#' @param ... Additional arguments.
#' @returns W/m\eqn{^2}
#' @export
rad_sw_toa <- function(...) {
  UseMethod("rad_sw_toa")
}

#' @rdname rad_sw_toa
#' @inheritParams sol_elevation
#' @param sol_const Solar constant in W/m\eqn{^2}, default `r sol_const_default`.
#' @export
#' @references p244
rad_sw_toa.default <- function(datetime, lon, lat, ..., sol_const = sol_const_default) {
  eccentricity <- sol_eccentricity(datetime)
  elevation <- sol_elevation(datetime, lon, lat)
  elevation <- deg2rad(elevation)

  sol_const * eccentricity * sin(elevation)
}

#' @rdname rad_sw_toa
#' @inheritParams sol_julian_day
#' @export
rad_sw_toa.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_sw_toa.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_sw_toa(datetime, lon, lat, weather_station)
}



#' Incoming diffused radiation
#'
#' @param ... Additional arguments.
#' @returns W/m\eqn{^2}
#' @export
rad_diffuse_in <- function(...) {
  UseMethod("rad_diffuse_in")
}

#' @rdname rad_diffuse_in
#' @inheritParams trans_vapor
#' @inheritDotParams trans_vapor
#' @inheritDotParams trans_ozone.default
#' @inheritDotParams rad_sw_toa sol_constant
#' @inheritDotParams terr_sky_view
#' @inheritDotParams terr_terrain_angle slope exposition
#' @export
#' @references p58eq3.14, p55eq3.9
rad_diffuse_in.default <- function(datetime, lon, lat, elev, temp, ...,
    surface_type = "field") {
  vapor <- trans_vapor(datetime, lon, lat, elev, temp, ...)
  ozone <- trans_ozone(datetime, lon, lat, ...)
  sw_toa <- rad_sw_toa(datetime, lon, lat, ...)
  sw_in <- rad_sw_in(datetime, lon, lat, elev, temp, ...)
  sky_view <- terr_sky_view(...)
  terrain_angle <- terr_terrain_angle(datetime, lon, lat, ...)
  
  elevation <- sol_elevation(datetime, lon, lat)
  solar_angle <- 90 - elevation
  
  albedo <- surface_properties[which(surface_properties$surface_type == surface_type), ]$albedo
  terrain_view <- 1 - terr_sky_view(...)
  
  terrain_angle <- deg2rad(terrain_angle)
  solar_angle <- deg2rad(solar_angle)
  
  out <- 0.5 * ((1 - (1 - vapor) - (1 - ozone)) * sw_toa - sw_in) *
    sky_view * (1 + cos(terrain_angle)^2 * sin(solar_angle)^3)
  out * (1 + albedo * terrain_view)
}

#' @rdname rad_diffuse_in
#' @inheritParams sol_julian_day
#' @export
rad_diffuse_in.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_diffuse_in.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_diffuse_in(datetime, lon, lat, elev, temp, weather_station)
}

#' Shortwave outgoing radiation
#'
#' Provide `slope` and `exposition` to perform topographic correction.
#'
#' @param ... Additional arguments.
#' @returns W/m\eqn{^2}.
#' @export
rad_sw_out <- function(...) {
  UseMethod("rad_sw_out")
}

#' @rdname rad_sw_out
#' @export
#' @references p46eq3.3, p52eq3.8
rad_sw_out.default <- function(datetime, lon, lat, elev, temp, ...,
    surface_type = "field") {
  sw_in <- rad_sw_in(datetime, lon, lat, elev, temp, ...)
  surface_type
  albedo <- surface_properties[which(surface_properties$surface_type == surface_type), ]$albedo
  
  sw_in * albedo
}

#' @rdname rad_sw_out
#' @inheritParams sol_julian_day
#' @export
rad_sw_out.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_sw_out.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_sw_out(datetime, lon, lat, elev, temp, weather_station)
}

#' Diffused outgoing radiation
#'
#' Provide `slope` and `exposition` to perform topographic correction.
#'
#' @param ... Additional arguments.
#' @returns W/m\eqn{^2}.
#' @export
rad_diffuse_out <- function(...) {
  UseMethod("rad_diffuse_out")
}

#' @rdname rad_diffuse_out
#' @export
#' @references p46eq3.3, p52eq3.8
rad_diffuse_out.default <- function(datetime, lon, lat, elev, temp, ...,
    surface_type = "field") {
  diffuse_in <- rad_diffuse_in(datetime, lon, lat, elev, temp, ...)
  albedo <- surface_properties[which(surface_properties$surface_type == surface_type), ]$albedo
  
  diffuse_in * albedo
}

#' @rdname rad_diffuse_out
#' @inheritParams sol_julian_day
#' @export
rad_diffuse_out.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_diffuse_out.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_diffuse_out(datetime, lon, lat, elev, temp, weather_station)
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

#' @rdname rad_lw_bal
#' @inheritParams sol_julian_day
#' @export
rad_lw_bal.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_lw_bal.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_lw_bal(temp, rh, surface_temp, weather_station)
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
#' @inheritDotParams terr_sky_view.default
#' @param sigma Stefan-Boltzmann constant in W/m\eqn{^2}/K\eqn{^4},
#'   default `r sigma_default'.
#' @export
#' @references p66eq3.24
rad_lw_in.default <- function(temp, rh, ..., sigma = sigma_default) {
  emissivity_air <- rad_emissivity_air(temp, rh, ...)
  sky_view <- terr_sky_view(...)
  temp <- c2k(temp)
  
  emissivity_air * sigma * temp^4 * sky_view
}

#' @rdname rad_lw_in
#' @inheritParams sol_julian_day
#' @export
rad_lw_in.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_lw_in.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_lw_in(temp, rh)
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
#' @inheritParams pres_p
#' @inheritDotParams pres_p
#' @inheritDotParams pres_sat_vapor_p
#' @param rh relative humidity.
#' @export
#' @references p66eq3.22
rad_emissivity_air.default <- function(temp, rh, ...) {
  vapor_p <- pres_vapor_p(temp, rh, ...)
  temp <- c2k(temp)
  
  (1.24 * vapor_p / temp)^(1 / 7)
}

#' @rdname rad_emissivity_air
#' @inheritParams sol_julian_day
#' @export
rad_emissivity_air.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_emissivity_air.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_emissivity_air(temp, rh)
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
rad_lw_out.default <- function(surface_temp, ...,
    surface_type = "field", sigma = sigma_default) {
  emissivity <- surface_properties[which(surface_properties$ surface_type == surface_type), ]$emissivity
  surface_temp <- c2k(surface_temp)
  
  emissivity * sigma * surface_temp^4
}

#' @rdname rad_lw_out
#' @inheritParams sol_julian_day
#' @export
rad_lw_out.weather_station <- function(weather_station, ...) {
  a <- formalArgs(rad_lw_out.default)
  a <- a[1:(length(a)-3)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_lw_out(surface_temp, weather_station)
}