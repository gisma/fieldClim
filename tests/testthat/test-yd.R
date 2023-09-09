datetime <- as.POSIXlt("2018-02-19 13:15:00", tz = "GMT")
lon = 8.683300
lat = 50.840503
elev = 270
temp = 20
slope = 30
exposition = 20
surface_type = "field"
valley = FALSE
surface_temp = 20
rh = 50
texture = "sand"
moisture = 0.2
soil_temp1 = 20
soil_temp2 = 30
soil_depth1 = 1
soil_depth2 = 0

# structure
## soil
soil_heat_flux(texture, moisture, soil_temp1, soil_temp2, soil_depth1, soil_depth2)
  soil_thermal_cond(texture, moisture)

## rad
#*1 means there are other optional arguments
rad_bal(datetime, lon, lat, elev, temp,
    surface_type, slope, exposition, valley,
    rh, surface_temp)*1
  rad_sw_bal(datetime, lon, lat, elev, temp,
    surface_type, slope, exposition, valley)*1
    rad_sw_in(datetime, lon, lat, elev, temp,
    surface_type, slope, exposition, valley)*1
      rad_sw_toa(datetime, lon, lat)*1
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
          pres_p(elev, temp)*1
      trans_ozone(datetime, lon, lat)*1
      trans_rayleigh(datetime, lon, lat, elev, temp)
      trans_vapor(datetime, lon, lat, elev, temp)
        hum_precipitable_water(datetime, lat, elev, temp)
      trans_aerosol(datetime, lon, lat, elev, temp)*1
      terr_terrain_angle(datetime, lon, lat, slope, exposition)
        sol_azimuth(datetime, lon, lat)
      terr_sky_view(slope, valley)
    rad_diffuse_in(datetime, lon, lat, elev, temp,
    surface_type, slope, valley)*1
    rad_sw_out(datetime, lon, lat, elev, temp,
    slope, exposition, valley, surface_type)*1
    rad_diffuse_out(datetime, lon, lat, elev, temp,
    surface_type, slope, valley)*1
  rad_lw_bal(temp, rh, surface_temp, surface_type)*1
    rad_lw_in(temp, rh)*1
      rad_emissivity_air(temp, rh)*1
        pres_vapor_p(temp, rh)*1
          pres_sat_vapor_p(temp)*1
    rad_lw_out(surface_temp, surface_type)*1



path <- file.path("inst", "extdata", "caldern_weather_station.csv")
input <- read.csv(path)
#input <- input[c(1:3, 133:135), ]
#names(input)
weather_station <- build_weather_station(
  datetime = strptime(input$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Atlantic/Reykjavik"),
  lon = 8.683300,
  lat = 50.840503,
  elev = 270,
  temp = input$Ta_2m,
  slope = 30,
  exposition = 20,
  surface_type = "field",
  valley = FALSE,
  surface_temp = input$Ta_2m,
  rh = input$Huma_2m,
  texture = "sand",
  moisture = input$water_vol_soil,
  soil_temp1 = input$Ts,
  soil_temp2 = input$Ta_2m,
  soil_depth1 = 1,
  soil_depth2 = 0
)

soil_heat_flux(weather_station)
  soil_thermal_cond(weather_station)

rad_bal(weather_station)
  rad_sw_bal(weather_station)
    rad_sw_in(weather_station)
      rad_sw_toa(weather_station)
        sol_eccentricity(weather_station)
          sol_day_angle(weather_station)
            sol_julian_day(weather_station)
        sol_elevation(weather_station)
          sol_declination(weather_station)
            sol_ecliptic_length(weather_station)
              sol_medium_anomaly(weather_station)
          sol_hour_angle(weather_station)
            sol_medium_suntime(weather_station)
            sol_time_formula(weather_station)
      trans_gas(weather_station)
        trans_air_mass_abs(weather_station)
          trans_air_mass_rel(weather_station)
          pres_p(weather_station)
      trans_ozone(weather_station)
      trans_rayleigh(weather_station)
      trans_vapor(weather_station)
        hum_precipitable_water(weather_station)
      trans_aerosol(weather_station)
      terr_terrain_angle(weather_station)
        sol_azimuth(weather_station)
    rad_diffuse_in(weather_station)
      terr_sky_view(weather_station)
    rad_sw_out(weather_station)
    rad_diffuse_out(weather_station)
  rad_lw_bal(weather_station)
    rad_lw_in(weather_station)
      rad_emissivity_air(weather_station)
        pres_vapor_p(weather_station)
          pres_sat_vapor_p(weather_station)
    rad_lw_out(weather_station)

Caldern_Wald <- build_weather_station(
    lat = 50.8411,
    lon = 8.68477,
    elev = 263,
    surface_type = "Laubwald",
    obs_height = 2,
    z1 = 2,
    z2 = 10,
    datetime = strptime(ws$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Atlantic/Reykjavik"),
    t1 = ws$Ta_2m,
    t2 = ws$Ta_10m,
    v1 = ws$Windspeed_2m,
    v2 = ws$Windspeed_10m,
    hum1 = ws$Huma_2m,
    hum2 = ws$Huma_10m,
    sw_in = ws$rad_sw_in,
    sw_out = ws$rad_sw_out,
    lw_in = ws$rad_lw_in,
    lw_out = ws$rad_lw_out,
    soil_flux = ws$heatflux_soil
)

a <- c()
t <- format(seq(as.POSIXlt("2020-01-01 00:00:00"), as.POSIXlt("2020-01-01 23:00:00"), 60*60))
for(i in t) {
  a <- c(a, fun(as.POSIXlt(i), 0, 0))
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





## bound
bound_mech_low
bound_mech_avg
bound_thermal_avg
  turb_ustar
  turb_roughness_length
  surface_properties
  temp_pot_temp
  pres_p

## turb
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
