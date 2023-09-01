#datetime <- as.POSIXlt("2020-01-01 ")
datetime <- as.POSIXlt("2020-01-01 12:00")
lat <- 57
lon <- 5
elev <- 200
t <- 15
slope <- 30
exposition <- 180

sol_eccentricity(datetime)
sol_day_angle(datetime)
sol_julian_day(datetime)
sol_elevation(datetime, lon, lat)
sol_declination(datetime)
sol_ecliptic_length(datetime)
sol_medium_anomaly(datetime)
sol_hour_angle(datetime, lon)
sol_medium_suntime(datetime, lon)
sol_time_formula(datetime, lon)

trans_gas(lat, datetime, lon, elev, t)
trans_air_mass_abs(lat, datetime, lon, elev, t)
trans_air_mass_rel(lat, datetime, lon)
pres_p(elev, t)
trans_ozone(lat, datetime, lon)

sol_azimuth(lat, datetime, lon)
rad_sw_in(datetime, lat, lon, elev, t, slope, exposition)

a <- c()
for(day_angle in seq(0, 359)) {
  a[day_angle+1] = 1.00011 + 0.034221 * cos(day_angle) + 0.00128 * sin(day_angle) + 0.000719 * cos(2 * day_angle) + 0.000719 * sin(2 * day_angle)
}
plot(a, type = "l")
# matches with p243, +-3%

a <- c()
for(elevation in seq(0, 359)) {
  a[elevation+1] = 1 / (sin(deg2rad(elevation)) + 1.5 * elevation^-0.72)
}
plot(a, type = "l")
