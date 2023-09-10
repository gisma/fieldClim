#' Sky view factor
#'
#' How much sky can be seen from the point?
#'
#' Terrain view factor can be calculated by `1 - terr_sky_view()`, which means:
#' "How much sky can **not** be seen from the point?" or
#' "How much terrain can be seen from the point?"
#'
#' @inheritParams build_weather_station
#' @returns Ratio from 0 to 1, unitless.
#' @export
#' @references Bendix 2004, p. 63 eq. 3.15.
terr_sky_view <- function(...) {
  UseMethod("terr_sky_view")
}

#' @rdname terr_sky_view
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 57 eq. 3.12, p. 57 eq. 3.13.
terr_sky_view.default <- function(slope, valley, ...) {
  slope <- deg2rad(slope)
  
  if (valley == FALSE) {
    (1 + cos(slope)) / 2
  } else if (valley == TRUE) {
    cos(slope)
  }
}

#' @rdname terr_sky_view
#' @inheritParams build_weather_station
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
#' @inheritParams build_weather_station
#' @returns Degree.
#' @export
terr_terrain_angle <- function(...) {
  UseMethod("terr_terrain_angle")
}

#' @rdname terr_terrain_angle
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 52 eq. 3.7.
terr_terrain_angle.default <- function(datetime, lon, lat, slope, exposition, ...) {
  elevation <- sol_elevation(datetime, lon, lat)
  azimuth <- sol_azimuth(datetime, lon, lat)
  
  slope <- deg2rad(slope)
  elevation <- deg2rad(elevation)
  azimuth <- deg2rad(azimuth)
  exposition <- deg2rad(exposition)
  
  out <- acos(
    cos(slope) * sin(elevation) +
    sin(slope) * cos(elevation) * cos(azimuth - exposition)
  )
  rad2deg(out)
}

#' @rdname terr_terrain_angle
#' @inheritParams build_weather_station
#' @export
terr_terrain_angle.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(terr_terrain_angle.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  terr_terrain_angle(datetime, lon, lat, slope, exposition)
}