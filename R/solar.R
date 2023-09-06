#' Eccentricity
#'
#' Calculates the eccentricity.
#'
#' @param ... Additional arguments.
#' @returns Eccentricity at the date. unitless
#' @export
sol_eccentricity <- function(...) {
  UseMethod("sol_eccentricity")
}

#' @rdname sol_eccentricity
#' @inheritParams sol_day_angle
#' @export
#' @references p243.
sol_eccentricity.default <- function(datetime, ...) {
#  if (!inherits(datetime, "POSIXt")) {
#    stop("datetime has to be of class POSIXt.")
#  }
  
  day_angle <- sol_day_angle(datetime)
  day_angle <- deg2rad(day_angle)
  
  1.00011 + 0.034221 * cos(day_angle) + 0.00128 * sin(day_angle) +
  0.000719 * cos(2 * day_angle) + 0.000719 * sin(2 * day_angle)
}

#' @rdname sol_eccentricity
#' @inheritParams sol_julian_day
#' @export
sol_eccentricity.weather_station <- function(weather_station, ...) {
#  check_availability(weather_station, "datetime")

  datetime <- weather_station$datetime

  sol_eccentricity(datetime)
}

#' Day angle
#'
#' @param ... Additional arguments.
#' @returns degree
#' @export
sol_day_angle <- function(...) {
  UseMethod("sol_day_angle")
}

#' @rdname sol_day_angle
#' @inheritParams sol_julian_day
#' @export
#' @references p243
sol_day_angle.default <- function(datetime, ...) {
  julian_day <- sol_julian_day(datetime)
  
  out <- 2 * pi * (julian_day - 1) / 365
  rad2deg(out)
}

#' @rdname sol_day_angle
#' @inheritParams sol_julian_day
#' @export
sol_day_angle.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  
  sol_day_angle(datetime)
}

#' Julian day
#'
#' @param ... Additional arguments.
#' @returns unitless
#' @export
sol_julian_day <- function(...) {
  UseMethod("sol_julian_day")
}

#' @rdname sol_julian_day
#' @param datetime Datetime in POSIXt
#' @export
sol_julian_day.default <- function(datetime, ...) {
  as.numeric(format(datetime, format = "%j"))
}

#' @rdname sol_julian_day
#' @param weather_station Object of class weather_station.
#' @export
sol_julian_day.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  
  sol_julian_day(datetime)
}

#' Solar elevation angle
#'
#' Calculates solar elevation angle for the given date and time.
#'
#' @param ... Additional arguments.
#' @returns Solar elevation angle in degrees.
#' @export
sol_elevation <- function(...) {
  UseMethod("sol_elevation")
}

#' @rdname sol_elevation
#' @inheritParams sol_hour_angle
#' @param lat Latitude in decimal degrees.
#' @export
#' @references p243
sol_elevation.default <- function(datetime, lon, lat, ...) {
  declination <- sol_declination(datetime)
  hour_angle <- sol_hour_angle(datetime, lon)
  
  lat <- deg2rad(lat)
  declination <- deg2rad(declination)
  hour_angle <- deg2rad(hour_angle)
  
  out <- asin(sin(lat) * sin(declination) +
    cos(lat) * cos(declination) * cos(hour_angle))
  rad2deg(out)
}

#' @rdname sol_elevation
#' @inheritParams sol_hour_angle
#' @export
sol_elevation.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  lon <- weather_station$lon
  lat <- weather_station$lat
  
  sol_elevation(datetime, lon, lat)
}

#' Solar declination
#'
#' @param ... Additional arguments.
#' @returns degree
#' @export
sol_declination <- function(...) {
  UseMethod("sol_declination")
}

#' @rdname sol_declination
#' @inheritParams sol_ecliptic_length
#' @export
#' @references p243
sol_declination.default <- function(datetime, ...) {
  ecliptic_length <- sol_ecliptic_length(datetime)
  ecliptic_length <- deg2rad(ecliptic_length)
  
  out <- asin(sin(deg2rad(23.44)) * sin(ecliptic_length))
  rad2deg(out)
}

#' @rdname sol_declination
#' @inheritParams sol_ecliptic_length
#' @export
sol_declination.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  
  sol_declination(datetime)
}

#' Solar ecliptic length
#'
#' @param ... Additional arguments.
#' @returns degree
#' @export
sol_ecliptic_length <- function(...) {
  UseMethod("sol_ecliptic_length")
}

#' @rdname sol_ecliptic_length
#' @inheritParams sol_julian_day
#' @export
#' @references p243
sol_ecliptic_length.default <- function(datetime, ...) {
  julian_day <- sol_julian_day(datetime)
  medium_anomaly <- sol_medium_anomaly(datetime)
  medium_anomaly <- deg2rad(medium_anomaly)
  
  279.3 + 0.9856 * julian_day + 1.92 * sin(medium_anomaly)
}

#' @rdname sol_ecliptic_length
#' @inheritParams sol_julian_day
#' @export
sol_ecliptic_length.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  
  sol_ecliptic_length(datetime)
}

#' Solar medium anomaly
#'
#' @param ... Additional arguments.
#' @returns degree
#' @export
sol_medium_anomaly <- function(...) {
  UseMethod("sol_medium_anomaly")
}

#' @rdname sol_medium_anomaly
#' @inheritParams sol_julian_day
#' @export
#' @references p243
sol_medium_anomaly.default <- function(datetime, ...) {
  julian_day <- sol_julian_day(datetime)
  
  356.6 + 0.9856 * julian_day
}

#' @rdname sol_medium_anomaly
#' @inheritParams sol_julian_day
#' @export
sol_medium_anomaly.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  
  sol_medium_anomaly(datetime)
}

#' Solar hour angle
#'
#' @param ... Additional arguments.
#' @returns degree
#' @export
sol_hour_angle <- function(...) {
  UseMethod("sol_hour_angle")
}

#' @rdname sol_hour_angle
#' @inheritParams sol_medium_suntime
#' @export
#' @references p243
sol_hour_angle.default <- function(datetime, lon, ...) {
  medium_suntime <- sol_medium_suntime(datetime, lon)
  time_formula <- sol_time_formula(datetime, lon)
  
  15 * (medium_suntime + time_formula - 12)
}

#' @rdname sol_hour_angle
#' @inheritParams sol_medium_suntime
#' @export
sol_hour_angle.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  lon <- weather_station$lon
  
  sol_hour_angle(datetime, lon)
}

#' Solar medium suntime
#'
#' @param ... Additional arguments.
#' @returns hour
#' @export
sol_medium_suntime <- function(...) {
  UseMethod("sol_medium_suntime")
}

#' @rdname sol_medium_suntime
#' @inheritParams sol_medium_anomaly
#' @param lon Longitude.
#' @export
#' @references p243
sol_medium_suntime.default <- function(datetime, lon, ...) {
  utc <- datetime$hour + datetime$min / 60 + datetime$sec / 3600
  
  utc + lon / 15
}

#' @rdname sol_medium_suntime
#' @inheritParams sol_medium_anomaly
#' @export
sol_medium_suntime.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  lon <- weather_station$lon
  
  sol_medium_suntime(datetime, lon)
}

#' Solar time formula
#'
#' @param ... Additional arguments.
#' @returns hour
#' @export
sol_time_formula <- function(...) {
  UseMethod("sol_time_formula")
}

#' @rdname sol_time_formula
#' @inheritParams sol_medium_anomaly
#' @param lon Longitude.
#' @export
#' @references p243
sol_time_formula.default <- function(datetime, lon, ...) {
  medium_anomaly <- sol_medium_anomaly(datetime)
  
  lon <- deg2rad(lon)
  medium_anomaly <- deg2rad(medium_anomaly)
  
  0.1644 * sin(2 * lon) - 0.1277 * sin(medium_anomaly)
}

#' @rdname sol_time_formula
#' @inheritParams sol_medium_anomaly
#' @export
sol_time_formula.weather_station <- function(weather_station, ...) {
  datetime <- weather_station$datetime
  lon <- weather_station$lon
  
  sol_time_formula(datetime, lon)
}

#' Solar azimuth angle
#'
#' Calculates solar azimuth angle for the given date and time.
#'
#' @param ... Additional arguments.
#' @returns Solar azimuth angle in degrees.
#' @export
sol_azimuth <- function(...) {
  UseMethod("sol_azimuth")
}

#' @rdname sol_azimuth
#' @inheritParams sol_elevation
#' @export
#' @references p243
sol_azimuth.default <- function(datetime, lon, lat, ...) {
  declination <- sol_declination(datetime)
  hour_angle <- sol_hour_angle(datetime, lon)
  elevation <- sol_elevation(datetime, lon, lat)
  medium_suntime <- sol_medium_suntime(datetime, lon)
  
  declination <- deg2rad(declination)
  lat <- deg2rad(lat)
  hour_angle <- deg2rad(hour_angle)
  elevation <- deg2rad(elevation)
  
  out <- acos(
    (sin(declination) * cos(lat) -
     cos(declination) * sin(lat) * cos(hour_angle)) / cos(elevation)
  )
  out <- rad2deg(out)
  
  if(medium_suntime < 12) {
    out
  } else if(medium_suntime >= 12) {
    360 - out
  }
}

#' @rdname sol_azimuth
#' @inheritParams sol_julian_day
#' @export
sol_azimuth.weather_station <- function(weather_station, ...) {
  a <- formalArgs(sol_azimuth.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_azimuth(datetime, lon, lat)
}