#https://www.r-bloggers.com/2016/07/timezone-conversion-in-r/
#datetime <- as.POSIXct(strptime("2018-08-19 13:15:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET"), tz = "CET")
#datetime <- format(datetime, tz = "GMT", usetz = TRUE)

#datetime <- as.POSIXct("2018-08-19 13:15:00", tz = "CET")
#datetime <- format(datetime, tz = "GMT", usetz = TRUE)
#datetime
#as.POSIXlt(datetime, tz = "GMT")
#datetime <- as.POSIXlt("2018-08-19 13:15:00", tz = "CET")
#datetime <- format(datetime, tz = "GMT", usetz = TRUE)
#datetime

#https://stackoverflow.com/questions/28996355/differences-between-subsetting-posixlt-and-posixct-in-r
#    POSIXlt is a 'list type' with components you can access as you do
#    POSIXct is a 'compact type' that is essentially just a number


datetime <- as.POSIXlt("2018-08-19 13:15:00", tz = "GMT")
lat <- 57
lon <- 5
elev <- 200
temp <- 15
slope <- 30
exposition <- 180
rh <- 50
surface_temp <- 10
ts1 <-
ts2 <- 
depth1 <- 
depth2 <- 

# structure
## 1 bound
bound_mech_low
bound_mech_avg
bound_thermal_avg
  turb_ustar
  turb_roughness_length
  surface_properties
  temp_pot_temp
  pres_p

## 2 rad
rad_bal(datetime, lon, lat, elev, temp, rh, surface_temp)
  rad_sw_bal(datetime, lon, lat, elev, temp)
    rad_sw_in(datetime, lon, lat, elev, temp)
      rad_sw_toa(datetime, lon, lat)
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
      trans_ozone(datetime, lon, lat)
      trans_rayleigh(datetime, lon, lat, elev, temp)
      trans_vapor(datetime, lon, lat, elev, temp)
        hum_precipitable_water(datetime, lat, elev, temp)
      trans_aerosol(datetime, lon, lat, elev, temp)
      terr_terrain_angle(datetime, lon, lat)
        sol_azimuth(datetime, lon, lat)
    rad_diffuse_in(datetime, lon, lat, elev, temp)
      terr_sky_view()
    rad_sw_out(datetime, lon, lat, elev, temp)
    rad_diffuse_out(datetime, lon, lat, elev, temp)
  rad_lw_bal(temp, rh, surface_temp)
    rad_lw_in(temp, rh)
      rad_emissivity_air(temp, rh)
        pres_vapor_p(temp, rh)
          pres_sat_vapor_p(temp)
    rad_lw_out(surface_temp)


## 3 Wärmestrom
turb_flux_imp_exchange
  turb_flux_ex_quotient_imp
  grad_rich_no
    temp_pot_temp
      pres_p
  ustar
    turb_roughness_length.default
    surface_properties
  monin
  air_density
  turb_flux_ex_quotient_temp

  turb_flux_stability

sensible_priestley_taylor
  rad_bal
  sc
  gam
  soil_flux
  priestley_taylor_coefficient

sensible_monin

sensible_bowen

latent

## 4. soil
soil_heat_flux(ts1, ts2, depth1, depth2)
  soil_thermal_cond()

out <- c()
a <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1)
for (i in seq(length(a))) {
  out[i] <- sol_julian_day(a[i])
}
plot(out, type = "l")

out <- c()
a <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1)
for (i in seq(length(a))) {
  out[i] <- sol_day_angle(a[i])
}
plot(out, type = "l")


out <- c()
a <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 1)
for (i in seq(length(a))) {
  out[i] <- sol_eccentricity(a[i])
}
plot(out, type = "l")

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


y <- c(0.269, 1.46, 1.98, 2.18, 2.31, 2.49, 2.58)
x <- c(0, 5, 10, 15, 20, 30, 43)
plot(x, y)
x_cal <- seq(min(x), max(x), 1)
y_cal <- c()
for (i in seq(length(x_cal))) {
  y_cal[i] <- approx(x, y, xout = x_cal[i], yleft = NA, yright = y[7])$y
}
lines(x_cal, y_cal)

y <- c(0.276, 0.586, 1.1, 1.43, 1.57, 1.74, 1.95)
x <- c(0, 5, 10, 15, 20, 30, 43)
plot(x, y)

y <- c(0.033, 0.042, 0.130, 0.276, 0.421, 0.478, 0.528)
x <- c(0, 10, 30, 50, 70, 80, 90)
plot(x, y)



weather_station <- weather_station_example_data
soil_thermal_cond(weather_station)