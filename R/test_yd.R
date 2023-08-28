datetime <- as.POSIXlt("2020-01-01 ")
datetime <- as.POSIXlt("2020-01-01 12:00")
lat <- 57
lon <- 5
elev <- 200
t <- 15

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
trans_air_mass_abs(p0 = 1013, lat, datetime, lon, elev, t)
trans_air_mass_rel(lat, datetime, lon)
pres_p(p0 = 1013, g = 9.81, elev, rl = 287.05, t)
