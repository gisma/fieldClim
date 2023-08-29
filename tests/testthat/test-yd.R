datetime <- as.POSIXlt("2020-01-01 ")
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
sol_elevation(lat, datetime, lon)
sol_declination(datetime)
sol_ecliptic_length(datetime)
sol_hour_angle(datetime, lon)
sol_medium_suntime(datetime, lon)
sol_time_formula(lon, datetime)
sol_medium_anomaly(datetime)


trans_gas(lat, datetime, lon, elev, t)
trans_air_mass_abs(lat, datetime, lon, elev, t)
trans_air_mass_rel(lat, datetime, lon)
pres_p(elev, t)
trans_ozone(lat, datetime, lon)

sol_azimuth(lat, datetime, lon)
rad_sw_in(datetime, lat, lon, elev, t, slope, exposition)
