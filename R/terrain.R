#' Sky view factor
#'
#' Calculates the sky view factor of a position only based on slope angle and
#' position in valley or on a slope.
#'
#' Terrain view factor can be calculated by 1-terr_sky_view.
#'
#' @param ... Additional arguments.
#' @returns Sky view factor from 0-1. unitless
#' @export
terr_sky_view <- function(...) {
  UseMethod("terr_sky_view")
}

#' @rdname terr_sky_view
#' @param slope Inclination of slope in degrees.
#' @param valley If the position is in a valley (TRUE) or on a slope (FALSE).
#' @export
#' @references p57eq3.12, p57eq3.13
terr_sky_view.default <- function(slope, valley, ...) {
  slope <- deg2rad(slope)
  
  if (valley == FALSE) {
    (1 + cos(slope)) / 2
  } else if (valley == TRUE) {
    cos(slope)
  }
}

#' @rdname terr_sky_view
#' @inheritParams sol_julian_day
#' @export
terr_sky_view.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(terr_sky_view.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  terr_sky_view(slope, valley)
}

#' Terrain angle
#'
#' @param ... Additional arguments.
#' @returns degree.
#' @export
terr_terrain_angle <- function(...) {
  UseMethod("terr_terrain_angle")
}

#' @rdname terr_terrain_angle
#' @inheritParams sol_elevation
#' @param slope Slope in degree.
#' @param exposition Exposition in degree.
#' @export
#' @references p52eq3.7
terr_terrain_angle.default <- function(datetime, lon, lat, slope, exposition, ...) {
  elevation <- sol_elevation(datetime, lon, lat)
  azimuth <- sol_azimuth(datetime, lon, lat)
  
  slope <- deg2rad(slope)
  elevation <- deg2rad(elevation)
  azimuth <- deg2rad(azimuth)
  exposition <- deg2rad(exposition)
  
  out <- acos(cos(slope) * sin(elevation) +
    sin(slope) * cos(elevation) * cos(azimuth - exposition))
  rad2deg(out)
}

#' @rdname terr_terrain_angle
#' @inheritParams sol_julian_day
#' @export
terr_terrain_angle.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(terr_terrain_angle.default)
  a <- a[1:(length(a)-3)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  terr_terrain_angle(datetime, lon, lat)
}