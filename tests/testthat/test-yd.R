#datetime <- as.POSIXlt("2020-01-01 ")
datetime <- as.POSIXlt("2020-01-01 12:00")
lat <- 57
lon <- 5
elev <- 200
temp <- 15
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

trans_gas(datetime, lon, lat, elev, temp)
trans_air_mass_abs(datetime, lon, lat, elev, temp)
trans_air_mass_rel(datetime, lon, lat)
pres_p(elev, temp)
trans_ozone.default(datetime, lon, lat)
trans_rayleigh.default(datetime, lon, lat, elev, temp)
trans_vapor.default(datetime, lon, lat, elev, temp)
hum_precipitable_water(elev, temp)
trans_aerosol.default(datetime, lon, lat, elev, temp)

rad_sw_in(datetime, lon, lat, elev, temp)
sol_azimuth(datetime, lon, lat)

rad_sw_toa(datetime, lon, lat)
rad_diffuse_in(datetime, lon, lat, elev, temp)
terr_terrain_angle(datetime, lon, lat)
terr_sky_view(slope)

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

T0 <- c(300, 294, 272.2, 287, 257.1)
pwst <- c(4.1167, 2.9243, 0.8539, 2.0852, 0.4176)
plot(T0, pwst)

vis <- seq(10, 60, 10)
tau38 <- c(0.71, 0.43, 0.33, 0.27, 0.22, 0.20)
tau50 <- c(0.46, 0.28, 0.21, 0.17, 0.14, 0.13)
plot(vis, tau38, ylim = c(min(tau50), max(tau38)), ylab = "tau")
points(vis, tau50, pch = 0)

vis_cal <- seq(10, 60, 1)
mod38 <- lm(log(tau38)~log(vis))
tau38_cal <- c()
for (i in seq(length(vis_cal))) {
  tau38_cal[i] <- exp(mod38$coefficients[1]) * vis_cal[i]^mod38$coefficients[2]
}
lines(vis_cal, tau38_cal)

mod50 <- lm(log(tau50)~log(vis))
tau50_cal <- c()
for (i in seq(length(vis_cal))) {
  tau50_cal[i] <- exp(mod50$coefficients[1]) * vis_cal[i]^mod50$coefficients[2]
}
lines(vis_cal, tau50_cal, lty = "dashed")

legend("topright", c("tau38", "tau50", "tau38_cal", "tau50_cal"), lty = c(0, 0, 1, 2), pch = c(1, 0, NA, NA))
