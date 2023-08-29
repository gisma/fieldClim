#' @return W/$m^{2}$
#' @references p45eq3.1
rad_bal <- function() {
  rad_sw_bal + rad_lw_bal
}

#' @return W/$m^{2}$
rad_sw_bal <- function() {
  (rad_sw_in + rad_diffuse_in) * (1 - albedo + albedo * terrain_view - albedo^2 * terrain_view)
}

#' @inheritParams sol_eccentricity
#' @return W/$m^{2}$
rad_sw_in <- function(datetime, lat, lon, elev, t, slope, exposition,
  sol_const = 1368, ozone_column = 0.35, vis = 30) {
  eccentricity <- sol_eccentricity(datetime)
  gas <- trans_gas(lat, datetime, lon, elev, t)
  ozone <- trans_ozone(lat, datetime, lon, ozone_column)
  rayleigh <- trans_rayleigh(lat, datetime, lon, elev, t)
  vapor <- trans_vapor(lat, datetime, lon, elev, t)
  aerosol <- trans_aerosol(lat, datetime, lon, elev, t, vis)
  elevation <- sol_elevation(lat, datetime, lon)
  azimuth <- sol_azimuth(lat, datetime, lon)
  
  trans_total <- gas * ozone * rayleigh * vapor * aerosol
  cos_terrain_angle <- cos(slope) * sin(elevation) +
    sin(slope) * cos(elevation) * cos(azimuth - exposition)
  
  sol_const * eccentricity * 0.9751 * trans_total * cos_terrain_angle
}

#' @inheritParams sol_julian_day
#' @return unitless
sol_eccentricity <- function(datetime) {
  day_angle <- sol_day_angle(datetime)
  day_angle <- deg2rad(day_angle)
  
  1.00011 + 0.034221 * cos(day_angle) + 0.00128 * sin(day_angle) +
  0.000719 * cos(2 * day_angle) + 0.000719 * sin(2 * day_angle)
}

#' @return degree
sol_day_angle <- function(datetime) {
  julian_day <- sol_julian_day(datetime)
  
  out <- 2 * pi * (julian_day - 1) / 365
  rad2deg(out)
}

#' @return unitless
sol_julian_day <- function(datetime) {
  as.numeric(format(datetime, format = "%j"))
}

#' @inheritParams trans_air_mass_abs
#' @return unitless
trans_gas <- function(lat, datetime, lon, elev, t) {
  air_mass_abs <- trans_air_mass_abs(lat, datetime, lon, elev, t)
  
  exp(-0.0127 * air_mass_abs^0.26)
}

#' @inheritParams trans_air_mass_rel
#' @return unitless
trans_ozone <- function(lat, datetime, lon, ozone_column = 0.35) {
  air_mass_rel <- trans_air_mass_rel(lat, datetime, lon)
  x <- ozone_column * air_mass_rel
  
  1 - (0.1611 * x * (1 + 139.48 * x)^(-0.3035) - 0.002715 * x * (1 + 0.044 * x + 0.0003 * x^2)^(-1))
}

#' @inheritParams trans_air_mass_abs
#' @return unitless
trans_rayleigh <- function(lat, datetime, lon, elev, t) {
  air_mass_abs <- trans_air_mass_abs(lat, datetime, lon, elev, t)
  
  exp(-0.0903 * air_mass_abs^0.84 * (1 + air_mass_abs - air_mass_abs^1.01))
}

#' @inheritParams trans_air_mass_rel
#' @inheritParams trans_precipitable_water
#' @return unitless
trans_vapor <- function(lat, datetime, lon, elev, t) {
  precipitable_water <- hum_precipitable_water(elev, t)
  air_mass_rel <- trans_air_mass_rel(lat, datetime, lon)
  x <- precipitable_water * air_mass_rel
  
  1 - 2.4959 * x * ((1 + 79.034 * x)^0.6828 + 6.385 * x)^-1
}

#' @inheritParams trans_air_mass_abs
#' @return unitless
trans_aerosol <- function(lat, datetime, lon, elev, t, vis = 30) {
  air_mass_abs <- trans_air_mass_abs(lat, datetime, lon, elev, t)
  tau38 <- 3.6536 * vis^(-0.7111)
  tau50 <- 2.4087 * vis^(-0.719)
  
  x <- 0.2758 * tau38 + 0.35 * tau50
  
  exp(-x^0.873 * (1 + x - x^0.7088) * air_mass_abs^0.9108)
}

#' @inheritParams trans_air_mass_rel
#' @return unitless
trans_air_mass_abs <- function(lat, datetime, lon, elev, t, p0 = 1013) {
  air_mass_rel <- trans_air_mass_rel(lat, datetime, lon)
  p <- pres_p(elev, t, p0 = p0)
  
  air_mass_rel * (p / p0)
}

#' @inheritParams sol_elevation
#' @return unitless
trans_air_mass_rel <- function(lat, datetime, lon) {
  elevation <- sol_elevation(lat, datetime, lon)
  
  1 / (sin(deg2rad(elevation)) + 1.5 * elevation^-0.72)
}

#' @return hPa
pres_p <- function(elev, t, p0 = 1013, g = 9.81, rl = 287.05) {
  t <- c2k(t)
  
  p0 * exp(-(g * elev) / (rl * t))
}

#' @return degree
sol_elevation <- function(lat, datetime, lon) {
  lat <- deg2rad(lat)
  declination <- sol_declination(datetime)
  declination <- deg2rad(declination)
  hour_angle <- sol_hour_angle(datetime, lon)
  hour_angle <- deg2rad(hour_angle)
  
  out <- sin(lat) * sin(declination) +
    cos(lat) * cos(declination) * cos(hour_angle)
  rad2deg(out)
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
rad2deg <- function(rad) {
  rad * 180 / pi
}

#' @return radian
deg2rad <- function(deg) {
  deg * pi / 180
}

#' @return degree Celcius
c2k <- function(celcius) {
  celcius + 273.15
}

#' @return Kelvin
k2c <- function(kelvin) {
  kelvin - 273.15
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

hum_precipitable_water <- function(elev, t, p0 = 1013) {
  pw_standard <- 4.1167
  p <- pres_p(elev, t, p0)
  temp_standard <- 300
  pw_standard * (p / p0) * (temp_standard / t)^0.5
}

#' @return degree
sol_azimuth <- function(lat, datetime, lon) {
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

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}

 <- function() {
  
}




rad_lw_bal <- function() {
  rad_lw_in - rad_lw_out
}














1. bound_

bound_mech_low <- function(dist) {
  mib <- 0.3 * dist**0.5
  return(mib)
}

#it works!
#a <- function(za=10, z0=0.1*zh, zh=1.2) {
#  return(za/z0)  
}

bound_thermal_avg <- function(
  c = 1,
  ustar = turb_ustar.numeric(va = v),
  v = 1,
  temp_change_dist,
  t_pot_upwind = temp_pot_temp.numeric(t1),
  t_pot = temp_pot_temp.numeric(t2),
  t1,
  t2,
  lapse_rate
) {
  tib <- c * ustar/v * (temp_change_dist*(t_pot_upwind-t_pot)/lapse_rate)**0.5
  return(tib)
}

test:
bound_thermal_avg(temp_change_dist=2500, t1=30, t2=25, lapse_rate=1.2)
thermisch interne Grenzschicht (Morgen)
bound_thermal_avg(
  c = 1,
  ustar = turb_ustar.numeric(
    va = v,
    za = 2,
    z0 = turb_roughness_length.default(
      obs_height = 1.2
    )
  ),
  v = 1,
  temp_change_dist = 250,
  t_pot_upwind = temp_pot_temp.numeric(t1),
  t_pot = temp_pot_temp.numeric(t2),
  t1=15,
  t2=10,
  lapse_rate = 0.6/100
)
did not get v, t1, t2...

turb_ustar.numeric <- function(
  va,
  k = 0.4,
  za = 10,
  z0 = turb_roughness_length.default()
) {
  ustar <- va * k / log(za / z0)
  return(ustar)
}

turb_roughness_length.default <- function(
  surface_type = "field",
  obs_height = NULL
) {
  surface_properties <- surface_properties
  if(!is.null(obs_height)){
    z0 <- 0.1 * obs_height
  } else if(!is.null(surface_type)) {
    z0 <- surface_properties[which(surface_properties$surface_type==surface_type),]$roughness_length
  }
  return(z0)
}

temp_pot_temp.numeric <- function(
  t,
  p0 = 1013.25,
  p = pres_p.numeric(t = t)
) {
  pot_temp <- t * (p0/p)**0.286
  return(pot_temp)
}

pres_p.numeric <- function(elev=270, t, ...){
  t <- t + 273.15  # to Kelvin
  p0 <- 1013.25    # standard pressure in hPa
  g <- 9.81        # gravity acceleration
  rl <- 287.05     # specific gas constant
  p <- p0 * exp(- (g * elev) / (rl * t))
  return(p)
}

Aufgabe 5 passed