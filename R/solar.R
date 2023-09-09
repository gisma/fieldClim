#' Eccentricity factor
#'
#' The track of Earth around Sun is not a circle, but more like an ellipse.
#'
#' @inheritParams build_weather_station
#' @returns Unitless.
#' @export
sol_eccentricity <- function(...) {
  UseMethod("sol_eccentricity")
}

#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
sol_eccentricity.default <- function(datetime, ...) {
  day_angle <- sol_day_angle(datetime)
  day_angle <- deg2rad(day_angle)
  
  1.00011 + 0.034221 * cos(day_angle) + 0.00128 * sin(day_angle) +
  0.000719 * cos(2 * day_angle) + 0.000719 * sin(2 * day_angle)
}

#' @rdname sol_eccentricity
#' @inheritParams build_weather_station
#' @export
sol_eccentricity.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_eccentricity.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }

  sol_eccentricity(datetime)
}

#' Day angle
#'
#' See a year as a circle. The first day of a (leap) year is 0 degree
#' and the last day 360 degree.
#'
#' @inheritParams build_weather_station
#' @returns Degree.
#' @export
sol_day_angle <- function(...) {
  UseMethod("sol_day_angle")
}

#' @rdname sol_day_angle
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
sol_day_angle.default <- function(datetime, ...) {
  julian_day <- sol_julian_day(datetime)
  
  out <- 2 * pi * (julian_day - 1) / 365
  rad2deg(out)
}

#' @rdname sol_day_angle
#' @inheritParams build_weather_station
#' @export
sol_day_angle.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_day_angle.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_day_angle(datetime)
}

#' Julian day
#'
#' Day of year as an integer from 1 to 366.
#'
#' @inheritParams build_weather_station
#' @returns Unitless.
#' @export
sol_julian_day <- function(...) {
  UseMethod("sol_julian_day")
}

#' @inheritParams build_weather_station
#' @export
sol_julian_day.default <- function(datetime, ...) {
  as.integer(format(datetime, format = "%j"))
}

#' @inheritParams build_weather_station
#' @export
sol_julian_day.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_julian_day.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_julian_day(datetime)
}

#' Solar elevation angle
#'
#' @inheritParams build_weather_station
#' @returns Degree.
#' @export
sol_elevation <- function(...) {
  UseMethod("sol_elevation")
}

#' @rdname sol_elevation
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
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
#' @inheritParams build_weather_station
#' @export
sol_elevation.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_elevation.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_elevation(datetime, lon, lat)
}

#' Solar declination
#'
#' @inheritParams build_weather_station
#' @returns Degree.
#' @export
sol_declination <- function(...) {
  UseMethod("sol_declination")
}

#' @rdname sol_declination
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
sol_declination.default <- function(datetime, ...) {
  ecliptic_length <- sol_ecliptic_length(datetime)
  ecliptic_length <- deg2rad(ecliptic_length)
  
  out <- asin(sin(deg2rad(23.44)) * sin(ecliptic_length))
  rad2deg(out)
}

#' @rdname sol_declination
#' @inheritParams build_weather_station
#' @export
sol_declination.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_declination.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_declination(datetime)
}

#' Solar ecliptic length
#'
#' @inheritParams build_weather_station
#' @returns Degree.
#' @export
sol_ecliptic_length <- function(...) {
  UseMethod("sol_ecliptic_length")
}

#' @rdname sol_ecliptic_length
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
sol_ecliptic_length.default <- function(datetime, ...) {
  julian_day <- sol_julian_day(datetime)
  medium_anomaly <- sol_medium_anomaly(datetime)
  medium_anomaly <- deg2rad(medium_anomaly)
  
  279.3 + 0.9856 * julian_day + 1.92 * sin(medium_anomaly)
}

#' @rdname sol_ecliptic_length
#' @inheritParams build_weather_station
#' @export
sol_ecliptic_length.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_ecliptic_length.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_ecliptic_length(datetime)
}

#' Solar medium anomaly
#'
#' @inheritParams build_weather_station
#' @returns Degree.
#' @export
sol_medium_anomaly <- function(...) {
  UseMethod("sol_medium_anomaly")
}

#' @rdname sol_medium_anomaly
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
sol_medium_anomaly.default <- function(datetime, ...) {
  julian_day <- sol_julian_day(datetime)
  
  356.6 + 0.9856 * julian_day
}

#' @rdname sol_medium_anomaly
#' @inheritParams build_weather_station
#' @export
sol_medium_anomaly.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_medium_anomaly.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_medium_anomaly(datetime)
}

#' Solar hour angle
#'
#' @inheritParams build_weather_station
#' @returns Degree.
#' @export
sol_hour_angle <- function(...) {
  UseMethod("sol_hour_angle")
}

#' @rdname sol_hour_angle
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
sol_hour_angle.default <- function(datetime, lon, ...) {
  medium_suntime <- sol_medium_suntime(datetime, lon)
  time_formula <- sol_time_formula(datetime, lon)
  
  15 * (medium_suntime + time_formula - 12)
}

#' @rdname sol_hour_angle
#' @inheritParams build_weather_station
#' @export
sol_hour_angle.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_hour_angle.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_hour_angle(datetime, lon)
}

#' Solar medium suntime
#'
#' @inheritParams build_weather_station
#' @returns Hour.
#' @export
sol_medium_suntime <- function(...) {
  UseMethod("sol_medium_suntime")
}

#' @rdname sol_medium_suntime
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
sol_medium_suntime.default <- function(datetime, lon, ...) {
  # change to POSIXct and then change back to POSIXlt for timezone conversion
  datetime <- as.POSIXct(datetime)
  datetime <- as.POSIXlt(datetime, tz = "UTC")
  utc <- datetime$hour + datetime$min / 60 + datetime$sec / 3600
  
  utc + lon / 15
}

#' @rdname sol_medium_suntime
#' @inheritParams build_weather_station
#' @export
sol_medium_suntime.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_medium_suntime.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_medium_suntime(datetime, lon)
}

#' Solar time formula
#'
#' @inheritParams build_weather_station
#' @returns Hour.
#' @export
sol_time_formula <- function(...) {
  UseMethod("sol_time_formula")
}

#' @rdname sol_time_formula
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
sol_time_formula.default <- function(datetime, lon, ...) {
  medium_anomaly <- sol_medium_anomaly(datetime)
  
  lon <- deg2rad(lon)
  medium_anomaly <- deg2rad(medium_anomaly)
  
  0.1644 * sin(2 * lon) - 0.1277 * sin(medium_anomaly)
}

#' @rdname sol_time_formula
#' @inheritParams build_weather_station
#' @export
sol_time_formula.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_time_formula.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_time_formula(datetime, lon)
}

#' Solar azimuth
#'
#' @inheritParams build_weather_station
#' @returns Degree.
#' @export
sol_azimuth <- function(...) {
  UseMethod("sol_azimuth")
}

#' @rdname sol_azimuth
#' @inheritParams build_weather_station
#' @export
#' @references Bendix 2004, p. 243.
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
     cos(declination) * sin(lat) * cos(hour_angle)
    ) / cos(elevation)
  )
  out <- rad2deg(out)
  
  ifelse(medium_suntime < 12, out, 360 - out)
}

#' @rdname sol_azimuth
#' @inheritParams build_weather_station
#' @export
sol_azimuth.weather_station <- function(weather_station, ...) {
  a <- methods::formalArgs(sol_azimuth.default)
  a <- a[1:(length(a)-1)]
  for(i in a) {
    assign(i, weather_station[[i]])
  }
  
  sol_azimuth(datetime, lon, lat)
}