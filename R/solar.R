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
sol_eccentricity.POSIXt <- function(datetime, ...) {
  day_angle <- sol_day_angle(datetime)
  day_angle <- deg2rad(day_angle)
  
  1.00011 + 0.034221 * cos(day_angle) + 0.00128 * sin(day_angle) +
  0.000719 * cos(2 * day_angle) + 0.000719 * sin(2 * day_angle)
}
#sol_eccentricity.POSIXt <- function(datetime, ...) {
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
#' @references
sol_day_angle.numeric <- function(datetime) {
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
#' @param datetime datetime in POSIXt
#' @export
#' @references p
sol_julian_day.POSIXt <- function(datetime, ...) {
  as.numeric(format(datetime, format = "%j"))
}


#' Solar azimuth and elevation angles
#'
#' Calculates solar azimuth and solar elevation angle.
#'
#' @param ... Additional parameters passed to later functions.
#' @return data.frame with two columns: sol_azimuth and sol_elevation.
#' @export
#'
sol_angles <- function(...) {
  UseMethod("sol_angles")
}

#' @rdname sol_angles
#' @method sol_angles POSIXt
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @export
#' @references p243
sol_angles.POSIXt <- function(datetime, lat, lon, ...) {
  if (!inherits(datetime, "POSIXt")) {
    stop("datetime has to be of class POSIXt.")
  }
  # day of year
  doy <- as.numeric(strftime(datetime, format = "%j"))

  # decimal hour
  lt <- as.POSIXlt(datetime)
  ut <- lt$hour + lt$min / 60 + lt$sec / 3600

  f <- pi / 180 # angle to radian

  # conversion latitude to degrees
  gbr <- lat * f
  glr <- lon * f

  # medium sun time
  t <- ut + lon / 15. # in hours

  # angle in radian
  m <- 356.6 + 0.9856 * doy # in degrees
  m <- m * f # in radian

  # time formula
  zt <- 0.1644 * sin(2. * glr) - 0.1277 * sin(m) # in hours

  # hour-angle of the sun
  h <- (15. * f) * (t + zt - 12.) # in radian

  # geocentric apparent ecliptic longitude of the sun (in radian)
  del <- 279.3 * f + 0.9856 * f * doy + 1.92 * f * sin(356.6 * f + 0.9856 * f * doy)

  # Sine declination of the sun (in radian)
  sde <- sin(23.44 * f) * sin(del)

  # sun height
  shh <- sin(gbr) * sde + cos(gbr) * cos(asin(sde)) * cos(h)
  sh <- asin(shh) / f # (in degrees)

  # Sun azimuth
  saz <- (sde * cos(gbr) - cos(asin(sde)) * sin(gbr) * cos(h)) / cos((sh * f))

  saz_2 <- rep(NA, length(saz))
  for (i in 1:length(saz_2)) {
    if (t[i] < 12) {
      saz_2[i] <- acos(saz[i])
    }
    if (t[i] >= 12) {
      saz_2[i] <- 360 * f - acos(saz[i])
    }
  }
  saz_deg <- saz_2 / f
  results <- data.frame(
    sol_azimuth = saz_deg,
    sol_elevation = sh
  )
  return(results)
}

#' @rdname sol_angles
#' @method sol_angles weather_station
#' @param weather_station Object of class weather_station.
#' @export
sol_angles.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(sol_angles(datetime, lat, lon))
}


#' Solar elevation angle
#'
#' Calculates solar elevation angle for the given date and time.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Solar elevation angle in degrees.
#' @export
#'
sol_elevation <- function(...) {
  UseMethod("sol_elevation")
}

#' @rdname sol_elevation
#' @method sol_elevation POSIXt
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @export
#sol_elevation.POSIXt <- function(datetime, lat, lon, ...) {
#  angles <- sol_angles(datetime, lat, lon)
#  return(angles$sol_elevation)
#}
#' @return degree
sol_elevation.POSIXt <- function(lat, datetime, lon) {
  lat <- deg2rad(lat)
  declination <- sol_declination(datetime)
  declination <- deg2rad(declination)
  hour_angle <- sol_hour_angle(datetime, lon)
  hour_angle <- deg2rad(hour_angle)
  
  out <- sin(lat) * sin(declination) +
    cos(lat) * cos(declination) * cos(hour_angle)
  rad2deg(out)
}

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

#' @inheritParams sol_ecliptic_length
#' @return degree
sol_declination <- function(datetime) {
  ecliptic_length <- sol_ecliptic_length(datetime)
  ecliptic_length <- deg2rad(ecliptic_length)
  
  out <- asin(sin(deg2rad(23.44)) * sin(ecliptic_length))
  rad2deg(out)
}

#' @inheritParams sol_julian_day
#' @return degree
sol_ecliptic_length <- function(datetime) {
  julian_day <- sol_julian_day(datetime)
  
  x <- deg2rad(356.6 + 0.9856 * julian_day)
  
  279.3 + 0.9856 * julian_day + 1.92 * sin(x)
}

#' @return degree
sol_hour_angle <- function(datetime, lon) {
  medium_suntime <- sol_medium_suntime(datetime, lon)
  time_formula <- sol_time_formula(lon, datetime)
  
  15 * (medium_suntime + time_formula - 12)
}

#' @return hour
sol_medium_suntime <- function(datetime, lon) {
  utc <- datetime$hour + datetime$min / 60 + datetime$sec / 3600
  
  utc + lon / 15
}

#' @inheritParams sol_julian_day
#' @return hour
sol_time_formula <- function(lon, datetime) {
  lon <- deg2rad(lon)
  medium_anomaly <- sol_medium_anomaly(datetime)
  medium_anomaly <- deg2rad(medium_anomaly)
  
  0.1644 * sin(2 * lon) - 0.1277 * sin(medium_anomaly)
}

#' @inheritParams sol_julian_day
#' @return degree
sol_medium_anomaly <- function(datetime) {
  julian_day <- sol_julian_day(datetime)
  
  356.6 + 0.9856 * julian_day
}


#' Solar azimuth angle
#'
#' Calculates solar azimuth angle for the given date and time.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Solar azimuth angle in degrees.
#' @export
#'
sol_azimuth <- function(...) {
  UseMethod("sol_azimuth")
}

#' @rdname sol_azimuth
#' @method sol_azimuth POSIXt
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @export
#sol_azimuth.POSIXt <- function(datetime, lat, lon, ...) {
#  angles <- sol_angles(datetime, lat, lon)
#  return(angles$sol_azimuth)
#}
#' @return degree
sol_azimuth.POSIXt <- function(lat, datetime, lon) {
  lat <- deg2rad(lat)
  declination <- sol_declination(datetime)
  declination <- deg2rad(declination)
  hour_angle <- sol_hour_angle(datetime, lon)
  hour_angle <- deg2rad(hour_angle)
  elevation <- sol_elevation(lat, datetime, lon)
  elevation <- deg2rad(elevation)
  medium_suntime <- sol_medium_suntime(datetime, lon)
  
  x <- acos((sin(declination) * cos(lat) - cos(declination) * sin(lat) * cos(hour_angle)) / cos(elevation))
  x <- rad2deg(x)
  
  if(medium_suntime < 12) {
    x
  } else if(medium_suntime >= 12) {
    360 - x
  }
}

#' @rdname sol_azimuth
#' @method sol_azimuth weather_station
#' @param weather_station Object of class weather_station.
#' @export
sol_azimuth.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "datetime", "latitude", "longitude")

  datetime <- weather_station$measurements$datetime
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude

  return(sol_azimuth(datetime, lat, lon))
}
