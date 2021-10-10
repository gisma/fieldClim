#' Sky view factor
#'
#' Calculates the sky view factor of a position only based on slope angle and
#' position in valley or on a slope.
#'
#' Terrain view factor can be calculated by 1-terr_sky_view.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Sky view factor from 0-1.
#' @export
#'
terr_sky_view <- function (...) {
  UseMethod("terr_sky_view")
}

#' @rdname terr_sky_view
#' @method terr_sky_view numeric
#' @param slope Inclination of slope in degrees.
#' @param valley If the position is in a valley (TRUE) or on a slope (FALSE).
#' @export
terr_sky_view.numeric <- function(slope, valley = F, ...) {
  if(valley == TRUE){
    return(1 - cos(slope * pi / 180))
  }else{
    return((1 - cos(slope * pi / 180)) / 2.0)
  }
}

#' @rdname terr_sky_view
#' @method terr_sky_view weather_station
#' @param weather_station Object of class weather_station.
#' @export
terr_sky_view.weather_station <- function (weather_station, ...) {
  check_availability(weather_station, "slope", "valley")

  slope <- weather_station$location_properties$slope
  valley <- weather_station$location_properties$valley

  return(terr_sky_view(slope, valley))
}
