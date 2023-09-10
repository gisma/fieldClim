#' Total radiation balance
#'
#' Sum of shortwave and longwave radiation balance.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_bal <- function(...) {
  UseMethod("rad_bal")
}

#' @rdname rad_bal
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 45 eq. 3.1.
rad_bal.default <- function(datetime, lon, lat, elev, temp, rh,
    slope, exposition, valley, surface_type, surface_temp, ...) {
  sw_bal <- rad_sw_bal(datetime, lon, lat, elev, temp,
    slope, exposition, valley, surface_type, ...)
  lw_bal <- rad_lw_bal(temp, rh, slope, valley, surface_type, surface_temp, ...)
  
  sw_bal + lw_bal
}

#' @rdname rad_bal
#' @inheritParams build_weather_station
#' @export
rad_bal.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_bal.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_bal(datetime, lon, lat, elev, temp, rh,
    slope, exposition, valley, surface_type, surface_temp, ...)
}

#' Shortwave radiation balance
#'
#' Sum of shortwave incoming and outgoing as well as
#' diffused incoming and outgoing radiation.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_sw_bal <- function(...) {
  UseMethod("rad_sw_bal")
}

#' @rdname rad_sw_bal
#' @export
#' @references Bendix 2004, p. 45 eq. 3.1.
rad_sw_bal.default <- function(datetime, lon, lat, elev, temp,
    slope, exposition, valley, surface_type, ...) {
  sw_in <- rad_sw_in(datetime, lon, lat, elev, temp,
    slope, exposition, ...)
  sw_out <- rad_sw_out(datetime, lon, lat, elev, temp,
    slope, exposition, surface_type, ...)
  diffuse_in <- rad_diffuse_in(datetime, lon, lat, elev, temp,
    slope, exposition, valley, ...)
  diffuse_out <- rad_diffuse_out(datetime, lon, lat, elev, temp,
    slope, exposition, valley, surface_type, ...)
  
  sw_in - sw_out + diffuse_in - diffuse_out
}

#' @rdname rad_sw_bal
#' @inheritParams build_weather_station
#' @export
rad_sw_bal.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_sw_bal.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_sw_bal(datetime, lon, lat, elev, temp,
    slope, exposition, valley, surface_type, ...)
}

#' Shortwave incoming radiation
#'
#' Direct shortwave incoming radiation.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_sw_in <- function(...) {
  UseMethod("rad_sw_in")
}

#' @rdname rad_sw_in
#' @inheritParams build_weather_station
#' @inheritDotParams trans_ozone.default ozone_column
#' @inheritDotParams trans_aerosol.default vis
#' @export
#' @references Bendix 2004, p. 46 eq. 3.3, p. 52 eq. 3.8.
rad_sw_in.default <- function(datetime, lon, lat, elev, temp,
    slope, exposition, ...) {
  sw_toa <- rad_sw_toa(datetime, lon, lat, ...)
  
  gas <- trans_gas(datetime, lon, lat, elev, temp, ...)
  ozone <- trans_ozone(datetime, lon, lat, ...)
  rayleigh <- trans_rayleigh(datetime, lon, lat, elev, temp, ...)
  vapor <- trans_vapor(datetime, lon, lat, elev, temp, ...)
  aerosol <- trans_aerosol(datetime, lon, lat, elev, temp, ...)
  trans_total <- gas * ozone * rayleigh * vapor * aerosol

  elevation <- sol_elevation(datetime, lon, lat)
  terrain_angle <- terr_terrain_angle(datetime, lon, lat, slope, exposition)
  
  elevation <- deg2rad(elevation)
  terrain_angle <- deg2rad(terrain_angle)
  
  out <- sw_toa * 0.9751 * trans_total / sin(elevation) * cos(terrain_angle)
  # if out < 0, which means no direct shortwave radiation, out is set to 0
  ifelse(out < 0, 0, out)
}

#' @rdname rad_sw_in
#' @inheritParams build_weather_station
#' @export
rad_sw_in.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_sw_in.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_sw_in(datetime, lon, lat, elev, temp,
    slope, exposition, ...)
}

#' Shortwave radiation at top of atmosphere
#'
#' Shortwave radiation without influence of the atmosphere.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_sw_toa <- function(...) {
  UseMethod("rad_sw_toa")
}

#' @rdname rad_sw_toa
#' @inheritParams build_weather_station
#' @param sol_const Solar radiation constant in \eqn{W \cdot m^{-2}}. Default = `r sol_const_default`.
#' @export
#' @references Bendix 2004, p. 244.
rad_sw_toa.default <- function(datetime, lon, lat, ..., sol_const = sol_const_default) {
  eccentricity <- sol_eccentricity(datetime)
  elevation <- sol_elevation(datetime, lon, lat)
  elevation <- deg2rad(elevation)
  
  out <- sol_const * eccentricity * sin(elevation)
  # negative value comes from elevation < 0, which means night, and out is therefore set to 0
  ifelse(out < 0, 0, out)
}

#' @rdname rad_sw_toa
#' @inheritParams build_weather_station
#' @export
rad_sw_toa.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_sw_toa.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_sw_toa(datetime, lon, lat, ...)
}

#' Incoming diffused radiation
#'
#' Diffused shortwave incoming radiation.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_diffuse_in <- function(...) {
  UseMethod("rad_diffuse_in")
}

#' @rdname rad_diffuse_in
#' @inheritParams build_weather_station
#' @inheritDotParams rad_sw_toa.default sol_const
#' @inheritDotParams trans_ozone.default ozone_column
#' @export
#' @references Bendix 2004, p. 58 eq. 3.14, p. 55 eq. 3.9.
rad_diffuse_in.default <- function(datetime, lon, lat, elev, temp,
    slope, exposition, valley, ...) {
  vapor <- trans_vapor(datetime, lon, lat, elev, temp, ...)
  ozone <- trans_ozone(datetime, lon, lat, ...)
  sw_toa <- rad_sw_toa(datetime, lon, lat, ...)
  sw_in <- rad_sw_in(datetime, lon, lat, elev, temp,
    slope, exposition, ...)
  
  sky_view <- terr_sky_view(slope, valley)
  terrain_angle <- terr_terrain_angle(datetime, lon, lat, slope, exposition)
  elevation <- sol_elevation(datetime, lon, lat)
  solar_angle <- 90 - elevation
  
  terrain_angle <- deg2rad(terrain_angle)
  solar_angle <- deg2rad(solar_angle)
  
  out <- 0.5 * ((1 - (1 - vapor) - (1 - ozone)) * sw_toa - sw_in) *
    sky_view * (1 + cos(terrain_angle)^2 * sin(solar_angle)^3)
  # if sw_toa is 0, which means night, out is set to 0
  ifelse(sw_toa == 0, 0, out)
}

#' @rdname rad_diffuse_in
#' @inheritParams build_weather_station
#' @export
rad_diffuse_in.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_diffuse_in.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_diffuse_in(datetime, lon, lat, elev, temp,
    slope, exposition, valley, ...)
}

#' Shortwave outgoing radiation
#'
#' Reflected shortwave incoming radiation.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_sw_out <- function(...) {
  UseMethod("rad_sw_out")
}

#' @rdname rad_sw_out
#' @export
#' @references Bendix 2004, p. 45 eq. 3.1.
rad_sw_out.default <- function(datetime, lon, lat, elev, temp,
    slope, exposition, surface_type, ...) {
  sw_in <- rad_sw_in(datetime, lon, lat, elev, temp, slope, exposition, ...)
  albedo <- surface_properties[which(surface_properties$surface_type == surface_type), ]$albedo
  
  sw_in * albedo
}

#' @rdname rad_sw_out
#' @inheritParams build_weather_station
#' @export
rad_sw_out.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_sw_out.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_sw_out(datetime, lon, lat, elev, temp,
    slope, exposition, surface_type, ...)
}

#' Diffused outgoing radiation
#'
#' Reflected diffused incoming radiation.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_diffuse_out <- function(...) {
  UseMethod("rad_diffuse_out")
}

#' @rdname rad_diffuse_out
#' @export
#' @references Bendix 2004, p. 45 eq. 3.1.
rad_diffuse_out.default <- function(datetime, lon, lat, elev, temp,
    slope, exposition, valley, surface_type, ...) {
  diffuse_in <- rad_diffuse_in(datetime, lon, lat, elev, temp,
    slope, exposition, valley, ...)
  albedo <- surface_properties[which(surface_properties$surface_type == surface_type), ]$albedo
  
  diffuse_in * albedo
}

#' @rdname rad_diffuse_out
#' @inheritParams build_weather_station
#' @export
rad_diffuse_out.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_diffuse_out.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_diffuse_out(datetime, lon, lat, elev, temp,
    slope, exposition, valley, surface_type, ...)
}

#' Long wave radiation balance
#'
#' Sum of longwave incoming and outgoing radiation.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_lw_bal <- function(...) {
  UseMethod("rad_lw_bal")
}

#' @rdname rad_lw_bal
#' @inheritParams build_weather_station
#' @inheritDotParams rad_lw_in.default sigma
#' @export
#' @references Bendix 2004, p. 68. eq. 3.25.
rad_lw_bal.default <- function(temp, rh, slope, valley, surface_type, surface_temp, ...) {
  lw_in <- rad_lw_in(temp, rh, slope, valley, ...)
  lw_out <- rad_lw_out(surface_type, surface_temp, ...)
  
  lw_in - lw_out
}

#' @rdname rad_lw_bal
#' @inheritParams build_weather_station
#' @export
rad_lw_bal.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_lw_bal.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_lw_bal(temp, rh, slope, valley, surface_temp, surface_type, ...)
}

#' Longwave incoming radiation
#'
#' Longwave radiation of the atmosphere.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_lw_in <- function(...) {
  UseMethod("rad_lw_in")
}

#' @rdname rad_lw_in
#' @inheritParams build_weather_station
#' @param sigma Stefan-Boltzmann constant in \eqn{W \cdot m{^-2} \cdot K{^-4}},
#'   default `r sigma_default`.
#' @export
#' @references Bendix 2004, p. 68 eq. 3.24. The second part of the equation
#'   related to the surrounding terrain is not included.
rad_lw_in.default <- function(temp, rh, slope, valley, ..., sigma = sigma_default) {
  emissivity_air <- rad_emissivity_air(temp, rh, ...)
  sky_view <- terr_sky_view(slope, valley)
  temp <- c2k(temp)
  
  emissivity_air * sigma * temp^4 * sky_view
}

#' @rdname rad_lw_in
#' @inheritParams build_weather_station
#' @export
rad_lw_in.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_lw_in.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_lw_in(temp, rh, slope, valley, ...)
}

#' Emissivity of the atmosphere
#'
#' How much does the air emit longwave radiation compared to a black body?
#'
#' @inheritParams build_weather_station
#' @returns Ratio from 0 to 1, unitless.
#' @export
rad_emissivity_air <- function(...) {
  UseMethod("rad_emissivity_air")
}

#' @rdname rad_emissivity_air
#' @inheritParams build_weather_station
#' @inheritDotParams pres_sat_vapor_p.default a b
#' @export
#' @references Bendix 2004, p. 66 eq. 3.22.
rad_emissivity_air.default <- function(temp, rh, ...) {
  vapor_p <- pres_vapor_p(temp, rh, ...)
  temp <- c2k(temp)
  
  (1.24 * vapor_p / temp)^(1 / 7)
}

#' @rdname rad_emissivity_air
#' @inheritParams build_weather_station
#' @export
rad_emissivity_air.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_emissivity_air.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_emissivity_air(temp, rh, ...)
}


#' Longwave outgoing radiation
#'
#' Longwave radiation of the surface.
#'
#' @inheritParams build_weather_station
#' @returns W/m\eqn{^2}.
#' @export
rad_lw_out <- function(...) {
  UseMethod("rad_lw_out")
}

#' @rdname rad_lw_out
#' @inheritParams build_weather_station
#' @inheritParams rad_lw_in
#' @export
#' @references Bendix 2004, p. 66 eq. 3.20.
rad_lw_out.default <- function(surface_type, surface_temp, ...,
    sigma = sigma_default) {
  emissivity <- surface_properties[which(surface_properties$ surface_type == surface_type), ]$emissivity
  surface_temp <- c2k(surface_temp)
  
  emissivity * sigma * surface_temp^4
}

#' @rdname rad_lw_out
#' @inheritParams build_weather_station
#' @export
rad_lw_out.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(rad_lw_out.default)
  a <- a[1:(length(a)-2)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  rad_lw_out(surface_type, surface_temp, ...)
}