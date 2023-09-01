#' Eccentricity
#'
#' Calculates the eccentricity.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Eccentricity at the date. unitless
#' @export
sol_eccentricity <- function(...) {
  UseMethod("sol_eccentricity")
}

#' @rdname sol_eccentricity
#' @inheritParams sol_day_angle
#' @export
#' @references p243.
sol_eccentricity.default <- function(datetime, ...) {
  day_angle <- sol_day_angle(datetime)
  day_angle <- deg2rad(day_angle)
  
  1.00011 + 0.034221 * cos(day_angle) + 0.00128 * sin(day_angle) +
  0.000719 * cos(2 * day_angle) + 0.000719 * sin(2 * day_angle)
}
#sol_eccentricity.default <- function(datetime, ...) {
#  if (!inherits(datetime, "POSIXt")) {
#    stop("datetime has to be of class POSIXt.")
#  }

#   day of year
#  doy <- as.numeric(strftime(datetime, format = "%j"))

#  x <- 2.0 * pi * (doy - 1) / 365.0
#  exz <- 1.00011 + 0.034221 * cos(x) + 0.00128 * sin(x) + 0.000719 * cos(2 * x) + 0.000719 * sin(2. * x)
#  return(exz)
#}

#' @rdname sol_eccentricity
#' @param weather_station Object of class weather_station.
#' @export
sol_eccentricity.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime")

  datetime <- weather_station$measurements$datetime

  sol_eccentricity(datetime)
}

#' Day angle
#'
#' @param ... Additional arguments.
#' @return degree
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

#' Julian day
#'
#' @param ... Additional parameters passed to later functions.
#' @return unitless
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

#' Solar elevation angle
#'
#' Calculates solar elevation angle for the given date and time.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Solar elevation angle in degrees.
#' @export
sol_elevation <- function(...) {
  UseMethod("sol_elevation")
}

#' @rdname sol_elevation
#' @inheritParams sol_hour_angle
#' @param lon Longitude in decimal degrees.
#' @export
#' @references p243
sol_elevation.default <- function(datetime, lon, lat, ...) {
  lat <- deg2rad(lat)
  declination <- sol_declination(datetime)
  declination <- deg2rad(declination)
  hour_angle <- sol_hour_angle(datetime, lon)
  hour_angle <- deg2rad(hour_angle)
  
  out <- sin(lat) * sin(declination) +
    cos(lat) * cos(declination) * cos(hour_angle)
  rad2deg(out)
}
#sol_elevation.default <- function(datetime, lat, lon, ...) {
#  angles <- sol_angles(datetime, lat, lon)
#  return(angles$sol_elevation)
#}
#' @return degree

#' @rdname sol_elevation
#' @method sol_elevation weather_station
#' @param weather_station Object of class weather_station.
#' @export
sol_elevation.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(sol_elevation(datetime, lat, lon))
}

#' Solar declination
#'
#' @param ... Additional arguments.
#' @return degree
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

#' Solar ecliptic length
#'
#' @param ... Additional arguments.
#' @return degree
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

#' Solar medium anomaly
#'
#' @param ... Additional arguments.
#' @return degree
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

#' Solar hour angle
#'
#' @param ... Additional arguments.
#' @return degree
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

#' Solar medium suntime
#'
#' @param ... Additional arguments.
#' @return hour
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
  datetime <- as.POSIXlt(datetime, tz = "GMT")
  utc <- datetime$hour + datetime$min / 60 + datetime$sec / 3600
  
  utc + lon / 15
}

#' Solar time formula
#'
#' @param ... Additional arguments.
#' @return hour
#' @export
sol_time_formula <- function(...) {
  UseMethod("sol_time_formula")
}

#' @rdname sol_medium_anomaly
#' @inheritParams sol_medium_anomaly
#' @param lon Longitude.
#' @export
#' @references p243
sol_time_formula.default <- function(datetime, lon, ...) {
  lon <- deg2rad(lon)
  medium_anomaly <- sol_medium_anomaly(datetime)
  medium_anomaly <- deg2rad(medium_anomaly)
  
  0.1644 * sin(2 * lon) - 0.1277 * sin(medium_anomaly)
}

#' Solar azimuth angle
#'
#' Calculates solar azimuth angle for the given date and time.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Solar azimuth angle in degrees.
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
  declination <- deg2rad(declination)
  hour_angle <- sol_hour_angle(datetime, lon)
  hour_angle <- deg2rad(hour_angle)
  elevation <- sol_elevation(datetime, lon, lat)
  elevation <- deg2rad(elevation)
  medium_suntime <- sol_medium_suntime(datetime, lon)
  lat <- deg2rad(lat)
  
  x <- acos((sin(declination) * cos(lat) - cos(declination) * sin(lat) * cos(hour_angle)) / cos(elevation))
  x <- rad2deg(x)
  
  if(medium_suntime < 12) {
    x
  } else if(medium_suntime >= 12) {
    360 - x
  }
}
#sol_azimuth.default <- function(datetime, lat, lon, ...) {
#  angles <- sol_angles(datetime, lat, lon)
#  return(angles$sol_azimuth)
#}

#' @rdname sol_azimuth
#' @param weather_station Object of class weather_station.
#' @export
sol_azimuth.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(sol_azimuth(datetime, lat, lon))
}



















# Solar azimuth and elevation angles
#
# Calculates solar azimuth and solar elevation angle.
#
# @param ... Additional parameters passed to later functions.
# @return data.frame with two columns: sol_azimuth and sol_elevation.
# @export
#
#sol_angles <- function(...) {
#  UseMethod("sol_angles")
#}

# @rdname sol_angles
# @method sol_angles POSIXt
# @param datetime POSIXt object (POSIXct, POSIXlt).
# See [base::as.POSIXlt] and [base::strptime] for conversion.
# @param lat Latitude in decimal degrees.
# @param lon Longitude in decimal degrees.
# @export
# @references p243
#sol_angles.default <- function(datetime, lat, lon, ...) {
#  if (!inherits(datetime, "POSIXt")) {
#    stop("datetime has to be of class POSIXt.")
#  }
#  day of year
#  doy <- as.numeric(strftime(datetime, format = "%j"))

#   decimal hour
#  lt <- as.POSIXlt(datetime)
#  ut <- lt$hour + lt$min / 60 + lt$sec / 3600

#  f <- pi / 180 # angle to radian

#   conversion latitude to degrees
#  gbr <- lat * f
#  glr <- lon * f

#   medium sun time
#  t <- ut + lon / 15. # in hours

#   angle in radian
#  m <- 356.6 + 0.9856 * doy # in degrees
#  m <- m * f # in radian

#   time formula
#  zt <- 0.1644 * sin(2. * glr) - 0.1277 * sin(m) # in hours

#   hour-angle of the sun
#  h <- (15. * f) * (t + zt - 12.) # in radian

#   geocentric apparent ecliptic longitude of the sun (in radian)
#  del <- 279.3 * f + 0.9856 * f * doy + 1.92 * f * sin(356.6 * f + 0.9856 * f * doy)

#   Sine declination of the sun (in radian)
#  sde <- sin(23.44 * f) * sin(del)

#   sun height
#  shh <- sin(gbr) * sde + cos(gbr) * cos(asin(sde)) * cos(h)
#  sh <- asin(shh) / f # (in degrees)

#   Sun azimuth
#  saz <- (sde * cos(gbr) - cos(asin(sde)) * sin(gbr) * cos(h)) / cos((sh * f))

#  saz_2 <- rep(NA, length(saz))
#  for (i in 1:length(saz_2)) {
#    if (t[i] < 12) {
#      saz_2[i] <- acos(saz[i])
#    }
#    if (t[i] >= 12) {
#      saz_2[i] <- 360 * f - acos(saz[i])
#    }
#  }
#  saz_deg <- saz_2 / f
#  results <- data.frame(
#    sol_azimuth = saz_deg,
#    sol_elevation = sh
#  )
#  return(results)
#}

# @rdname sol_angles
# @method sol_angles weather_station
# @param weather_station Object of class weather_station.
# @export
#sol_angles.weather_station <- function(weather_station, ...) {
#  check_availability(weather_station, "datetime", "latitude", "longitude")

#  datetime <- weather_station$measurements$datetime
#  lat <- weather_station$location_properties$latitude
#  lon <- weather_station$location_properties$longitude

#  return(sol_angles(datetime, lat, lon))
#}