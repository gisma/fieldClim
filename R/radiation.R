#' Total radiation balance
#'
#' Calculate total radiation balance.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Total radiation balance in W/m$^{2}$.
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
#rad_bal.default <- function(rad_sw_radiation_balance,
#                                  rad_lw_in,
#                                  rad_lw_out, ...) {
#  radbil <- rad_sw_radiation_balance + (rad_lw_in - rad_lw_out)
#  return(radbil)
#}

#' @rdname rad_bal
#' @export
#' @param weather_station Object of class weather_station.
rad_bal.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "lw_in", "lw_out")

  rad_lw_surface <- weather_station$measurements$lw_out
  rad_lw_atmospheric <- weather_station$measurements$lw_in
  rad_sw_radiation_balance <- rad_sw_radiation_balance(weather_station)

  return(rad_bal(rad_sw_radiation_balance, rad_lw_surface, rad_lw_atmospheric))
}

#' Shortwave radiation balance
#'
#' @param ... Additional parameters passed to later functions.
#' @return W/m$^{2}$
#' @export
rad_sw_bal <- function(...) {
  UseMethod("rad_sw_bal")
}

#' @rdname rad_sw_bal
#' @export
#' @references p45eq3.1, p63eq3.18
rad_sw_bal.default <- function(..., valley = FALSE) {
  sw_in <- rad_sw_in()
  diffuse_in <- rad_diffuse_in()
  albedo <- 0.5
  sky_view <- terr_sky_view(slope, valley = valley)
  terrain_view <- 1 - sky_view
  
  (rad_sw_in + rad_diffuse_in) * (1 - albedo + albedo * terrain_view - albedo^2 * terrain_view)
}

#' Shortwave incoming radiation
#'
#' Provide `slope` and `exposition` to perform topographic correction.
#' 
#' @param ... Additional arguments.
#' @returns W/m$^{2}$.
#' @export
rad_sw_in <- function(...) {
  UseMethod("rad_sw_in")
}

#' @rdname rad_sw_in
#' @inheritParams trans_rayleigh
#' @inheritDotParams rad_sw_toa.default sol_const
#' @inheritDotParams trans_gas.default p0
#' @inheritDotParams trans_ozone.default ozone_column
#' @inheritDotParams trans_aerosol.default vis
#' @inheritDotParams terr_terrain_angle.default slope exposition
#' @export
#' @references p46eq3.3, p52eq3.8
rad_sw_in.default <- function(datetime, lon, lat, elev, temp, ...) {
  sw_toa <- rad_sw_toa(datetime, lon, lat, ...)
  elevation <- sol_elevation(datetime, lon, lat)
  elevation <- deg2rad(elevation)
  
  gas <- trans_gas(datetime, lon, lat, elev, temp, ...)
  ozone <- trans_ozone(datetime, lon, lat, ...)
  rayleigh <- trans_rayleigh(datetime, lon, lat, elev, temp)
  vapor <- trans_vapor(datetime, lon, lat, elev, temp)
  aerosol <- trans_aerosol(datetime, lon, lat, elev, temp, ...)
  trans_total <- gas * ozone * rayleigh * vapor * aerosol
  
  terrain_angle <- terr_terrain_angle(datetime, lon, lat, ...)
  terrain_angle <- deg2rad(terrain_angle)
  
  sw_toa / sin(elevation) * 0.9751 * trans_total * cos(terrain_angle)
}
#rad_sw_in.default <- function(rad_sw_toa, trans_total, ...) {
#  rad_sw_ground_horizontal <- rad_sw_toa * 0.9751 * trans_total
#  return(rad_sw_ground_horizontal)
#}

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
#' @param ... Additional parameters passed to later functions.
#' @return Shortwave radiation at top of atmosphere in W/m$^{2}$.
#' @export
rad_sw_toa <- function(...) {
  UseMethod("rad_sw_toa")
}

#' @rdname rad_sw_toa
#' @inheritParams sol_elevation
#' @param sol_const Solar constant in W/m$^{2}$
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
#' @method rad_sw_toa weather_station
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
#' @return Description.
#' @export
rad_diffuse_in <- function(...) {
  UseMethod("rad_diffuse_in")
}

#' @rdname rad_diffuse_in
#' @inheritParams trans_vapor
#' @inheritParams trans_ozone
#' @inheritParams rad_sw_toa
#' @inheritParams terr_sky_view
#' @inheritParams terr_terrain_angle
#' @export
#' @references p58eq3.14, p55eq3.9
rad_diffuse_in.default <- function(datetime, lon, lat, elev, temp, ...,
  p0 = 1013, ozone_column = 0.35, sol_const = 1368, valley = FALSE,
  slope = 0, exposition = 0) {
  vapor <- trans_vapor(datetime, lon, lat, elev, temp, p0 = p0)
  ozone <- trans_ozone(datetime, lon, lat, ozone_column = ozone_column)
  sw_toa <- rad_sw_toa(datetime, lon, lat, sol_const = sol_const)
  sw_in <- rad_sw_in(datetime, lon, lat, elev, temp)
  sky_view <- terr_sky_view(slope, valley = valley)
  terrain_angle <- terr_terrain_angle(datetime, lon, lat,
    slope = slope, exposition = exposition)
  terrain_angle <- deg2rad(terrain_angle)
  
  elevation <- sol_elevation(datetime, lon, lat)
  z <- 90 - elevation
  z <- deg2rad(z)
  
  0.5 * (
    (1 - (1 - vapor) - (1 - ozone)) *
    sw_toa - sw_in
  ) * sky_view * (1 + cos(terrain_angle)^2 * sin(z)^3)
}

#' Long wave radiation balance
#'
#' @param ... Additional arguments.
#' @return W/m$^{2}$.
#' @export
rad_lw_bal <- function(...) {
  UseMethod("rad_lw_bal")
}

#' @rdname rad_lw_bal
#' @inheritParams rad_lw_in
#' @inheritParams rad_lw_out
#' @export
#' @references p68eq3.25
rad_lw_bal.default <- function(name, ...) {
  rad_lw_in - rad_lw_out
}

#' Longwave radiation of the atmosphere
#'
#' Calculation of the longwave radiation of the atmosphere.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Atmospheric radiation in W/m$^{2}$.
#' @export
rad_lw_in <- function(...) {
  UseMethod("rad_lw_in")
}

#' @rdname rad_lw_in
#' @method rad_lw_in numeric
#' @param hum relative humidity in %.
#' @param t Air temperature in °C.
#' @export
#' @references p66eq3.21
rad_lw_in.default <- function(...) {
  sigma <- 5.6693e-8
  temp <- c2k(temp)
  sigma * temp^4
}
#rad_lw_in.default <- function(hum, t, ...) {
#  sigma <- 5.6693e-8
#  gs <- (0.594 + 0.0416 * sqrt(hum_vapor_pres(hum, t))) * sigma * (t + 273.15)^4
#  return(gs)
#}

#' @rdname rad_lw_in
#' @method rad_lw_in weather_station
#' @export
#' @param weather_station Object of class weather_station.
rad_lw_in.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "t2", "hum2")
  hum <- weather_station$measurements$hum2
  t <- weather_station$measurements$t2

  return(rad_lw_in(hum, t))
}

#' Longwave radiation of the surface
#'
#' Calculates emissions of a surface.
#'
#' If a weather_station object is given, the lower air temperature will be used
#' instead of the surface temperature.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Emissions in W/m$^{2}$.
#' @export
rad_lw_out <- function(...) {
  UseMethod("rad_lw_out")
}

#' @rdname rad_lw_out
#' @method rad_lw_out numeric
#' @param t_surface Surface temperature in °C.
#' @param surface_type Surface type for which a specific emissivity will be selected.
#' Default is 'field' as surface type.
#' @export
#' @references p66eq3.20
rad_lw_out.default <- function(t_surface, surface_type = "field", ...) {
  surface_properties <- surface_properties
  emissivity <- surface_properties[which(surface_properties$surface_type == surface_type), ]$emissivity
  sigma <- 5.6993e-8
  radout <- emissivity * sigma * (t_surface + 273.15)**4
  return(radout)
}

#' @rdname rad_lw_out
#' @method rad_lw_out weather_station
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

































#' Emissivity of the atmosphere
#'
#' Calculation of the emissivity of the atmosphere.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Emissivity of the atmosphere (0-1).
#' @export
rad_emissivity_air <- function(...) {
  UseMethod("rad_emissivity_air")
}

#' @rdname rad_emissivity_air
#' @method rad_emissivity_air numeric
#' @param t Air temperature in °C.
#' @param elev Meters above sea level in m.
#' @param hum humidity.
#' @param p OPTIONAL. Air pressure in hPa.
#' @export
#' @references p67eq3.23
rad_emissivity_air.default <- function(t, elev, hum, p = NULL, ...) {
  if (is.null(p)) p <- pres_p(elev, t)

  # Calculate temperature adjusted to elevation (with saturated adiabatic lapse rate)
  t_adj <- t * (0.0065 * elev)

  # Calculate saturated vapor pressure with adjusted temperature
  svp <- hum_sat_vapor_pres(t_adj)

  # Calculate vapor pressure with saturated vapor pressure and measured humidity
  e <- hum * svp

  # Calculate emissivity
  eat <- ((1.24 * e / (t_adj + 273.15))**1 / 7) * (p / 1013.25)

  return(eat)
}

#' @rdname rad_emissivity_air
#' @method rad_emissivity_air weather_station
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










#' Reflected shortwave radiation
#'
#' Calculation of the reflected shortwave radiation.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Reflected shortwave radiation in W/m$^{2}$.
#' @export
#'
rad_sw_out <- function(...) {
  UseMethod("rad_sw_out")
}

#' @rdname rad_sw_out
#' @method rad_sw_out numeric
#' @param rad_sw_in Shortwave radiation on the ground onto a horizontal area in W/m$^{2}$.
#' @param surface_type type of surface for which an albedo will be selected.
#' @param albedo if albedo measurements are performed, values in decimal can be inserted here.
#' @export
#' @references p59eq3.15
rad_sw_out.default <- function(rad_sw_in, surface_type = "field", albedo = NULL, ...) {
  if (!is.null(albedo)) {
    albedo <- albedo

    if (any(albedo > 1 | albedo < 0)) {
      warning("One or more input values for albedo argument are outside of the valid range (0-1). \n They will be set to NA.")
      albedo[albedo > 1 | albedo < 0] <- NA
    }
  } else {
    surface_properties <- surface_properties
    albedo <- surface_properties[which(surface_properties$surface_type == surface_type), ]$albedo
  }
  rad_sw_out <- rad_sw_in * albedo
  return(rad_sw_out)
}

#' @rdname rad_sw_out
#' @method rad_sw_out weather_station
#' @export
#' @param weather_station Object of class weather_station.
#' @param surface_type type of surface for which an albedo will be selected.
#' @param albedo if albedo measurements are performed, values in decimal can be inserted here.
rad_sw_out.weather_station <- function(weather_station, surface_type = "field", ...) {
  check_availability(weather_station, "sw_in")
  sw_in <- weather_station$measurements$sw_in

  if (!(is.null(weather_station$location_properties$albedo))) {
    albedo <- weather_station$location_properties$albedo
  } else {
    surface_properties <- surface_properties
    albedo <- surface_properties[which(surface_properties$surface_type == surface_type), ]$albedo
  }

  return(rad_sw_out(sw_in, albedo = albedo))
}



#' Shortwave radiation balance
#'
#' Calculation of the shortwave radiation balance.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Shortwave radiation balance in W/m$^{2}$.
#' @export
#'
rad_sw_radiation_balance <- function(...) {
  UseMethod("rad_sw_radiation_balance")
}

#' @rdname rad_sw_radiation_balance
#' @method rad_sw_radiation_balance numeric
#' @param rad_sw_ground_horizontal Shortwave radiation on the ground onto a horizontal area in W/m^2.
#' @param rad_sw_reflected Reflected shortwave radiation in W/m$^{2}$.
#' @export
#' @references p45eq3.1
rad_sw_radiation_balance.default <- function(rad_sw_ground_horizontal, rad_sw_reflected, ...) {
  rad_sw_radiation_balance <- rad_sw_ground_horizontal - rad_sw_reflected
  return(rad_sw_radiation_balance)
}

#' @rdname rad_sw_radiation_balance
#' @method rad_sw_radiation_balance weather_station
#' @export
#' @param weather_station Object of class weather_station.
rad_sw_radiation_balance.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "sw_in", "sw_out")
  rad_sw_ground_horizontal <- weather_station$measurements$sw_in
  rad_sw_reflected <- weather_station$measurements$sw_out

  return(rad_sw_radiation_balance(rad_sw_ground_horizontal, rad_sw_reflected))
}



#' Incoming shortwave radiation in dependency of topography
#'
#' Calculate shortwave radiation balance in dependency of topography.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Shortwave radiation balance in dependency of topography in W/m$^{2}$.
#' @export
#'
rad_sw_in_topo <- function(...) {
  UseMethod("rad_sw_in_topo")
}

#' @rdname rad_sw_in_topo
#' @method rad_sw_in_topo numeric
#' @param slope Slope in degrees.
#' @param sol_elevation Sun elevation in degrees.
#' @param sol_azimuth Sun azimuth in degrees.
#' @param exposition Exposition (North = 0, South = 180).
#' @param rad_sw_toa Shortwave radiation at top of atmosphere in W/m$^{2}$.
#' @param albedo Albedo of surface.
#' @param trans_total Total transmittance of the atmosphere (0-1).
#' Default is average atmospheric transmittance.
#' @param terr_sky_view Sky view factor (0-1).
#' @param trans_vapor Transmittance due to water vapor (0-1).
#' @param trans_ozone Transmittance due to ozone (0-1).
#' @export
#' @references p46eq3.3, p52eq3.8, p52eq3.7, p55eq3.9, p57eq3.11
rad_sw_in_topo.default <- function(slope,
                                   exposition = 0,
                                   terr_sky_view,
                                   sol_elevation, sol_azimuth,
                                   rad_sw_toa, albedo,
                                   trans_total = 0.8,
                                   trans_vapor = 0.5,
                                   trans_ozone = 0.5, ...) {
  sol_dir <- rad_sw_toa * 0.9751 * trans_total
  sol_dif <- 0.5 * ((1 - (1 - trans_vapor) - (1 - trans_ozone)) * rad_sw_tao - sol_dir)
  f <- (pi / 180)

  if (slope > 0) {
    terrain_angle <- (cos(slope * f) * sin(sol_elevation * f)
      + sin(slope * f) * cos(sol_elevation * f) * cos(sol_azimuth * f
        - (exposition * f)))
    rad_sw_topo_direct <- sol_dir / sin(sol_elevation * f) * terrain_angle
  } else if (slope == 0) {
    rad_sw_topo_direct <- sol_dir
  }

  rad_sw_topo_diffuse <- sol_dif * terr_sky_view
  sol_ter <- (rad_sw_topo_direct + rad_sw_topo_diffuse) * albedo * (1 - terr_sky_view)
  sol_in_topo <- rad_sw_topo_direct + rad_sw_topo_diffuse + sol_ter
  return(sol_in_topo)
}

#' @rdname rad_sw_in_topo
#' @method rad_sw_in_topo weather_station
#' @export
#' @param weather_station Object of class weather_station.
rad_sw_in_topo.weather_station <- function(weather_station, trans_total = 0.8, ...) {
  check_availability(
    weather_station, "slope", "datetime", "albedo",
    "sw_in", "sky_view", "exposition"
  )

  slope <- weather_station$location_properties$slope
  valley <- weather_station$location_properties$valley
  terr_sky_view <- weather_station$location_properties$sky_view
  exposition <- weather_station$location_properties$exposition
  rad_sw_tao <- weather_station$measurements$sw_in # need to be changed!!
  datetime <- weather_station$measurements$datetime
  albedo <- weather_station$location_properties$albedo
  sol_elevation <- sol_elevation(weather_station)
  sol_azimuth <- sol_azimuth(weather_station)

  return(rad_sw_in_topo(
    slope,
    exposition,
    terr_sky_view,
    sol_elevation, sol_azimuth,
    rad_sw_tao, albedo,
    trans_total
  ))
}







#' Incoming longwave radiation corrected by topography.
#'
#' Corrects the incoming longwave radiation using the sky view factor.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Incoming longwave radiation with topography in W/m$^{2}$.
#' @export
#'
rad_lw_in_topo <- function(...) {
  UseMethod("rad_lw_in_topo")
}

#' @rdname rad_lw_in_topo
#' @method rad_lw_in_topo numeric
#' @export
#' @param rad_lw_out Longwave surface emissions in W/m$^{2}$.
#' @param hum relative humidity in %.
#' @param t Air temperature in °C.
#' @param terr_sky_view Sky view factor from 0-1. (See [fieldClim::terr_sky_view])
#' @references p68eq3.24
rad_lw_in_topo.default <- function(rad_lw_out,
                                   hum,
                                   t,
                                   terr_sky_view, ...) {
  # Calculate incoming longwave radiation
  rad_lw_in <- rad_lw_in(hum, t)
  # Longwave component:
  rad_lw_in_topo <- rad_lw_in * terr_sky_view + rad_lw_out * (1 - terr_sky_view)
  return(rad_lw_in_topo)
}

#' @rdname rad_lw_in_topo
#' @method rad_lw_in_topo weather_station
#' @export
#' @param weather_station Object of class weather_station.
rad_lw_in_topo.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "lw_out", "lw_in", "sky_view")
  rad_lw_surface <- weather_station$measurements$lw_out
  hum <- weather_station$measurements$hum1
  t <- weather_station$measurements$t1
  terr_sky_view <- weather_station$location_properties$sky_view

  return(rad_lw_in_topo(
    rad_lw_surface,
    hum,
    t,
    terr_sky_view
  ))
}
