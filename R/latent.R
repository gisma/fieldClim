#' Latent Heat Priestley-Taylor Method
#'
#' Calculates the latent heat flux using the Priestley-Taylor method. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param ... Additional arguments.
#' @returns Latent heat flux in W/m\eqn{^2}.
#' @export
latent_priestley_taylor <- function(...) {
  UseMethod("latent_priestley_taylor")
}

#' @rdname latent_priestley_taylor
#' @export
#' @param t Air temperature in °C.
#' @param rad_bal Radiation balance in W/m\eqn{^2}.
#' @param soil_flux Soil flux in W/m\eqn{^2}.
#' @param surface_type Surface type, for which a Priestley-Taylor coefficient will be selected. Default is for short grass.
#' @references Foken 2016, p. 220, eq. 5.7.
latent_priestley_taylor.default <- function(t, rad_bal, soil_flux, surface_type = "field", ...) {
  priestley_taylor_coefficient <- priestley_taylor_coefficient

  if (!surface_type %in% priestley_taylor_coefficient$surface_type) {
    values_surface <- paste(priestley_taylor_coefficient$surface_type, collapse = " , ")
    stop("'surface_type' must be one of the following: ", values_surface)
  } else if (!is.null(surface_type)) {
    alpha_pt <- priestley_taylor_coefficient[which(priestley_taylor_coefficient$surface_type == surface_type), ]$alpha
  }

  sc <- sc(t)
  gam <- gam(t)

  out <- alpha_pt * sc * (-rad_bal - soil_flux) / (sc + gam)

  # values of sensible bowen will be checked whether they exceed the valid data range.
  if (max(out) > 600) {
    warning("There are values above 600 W/m^2!")
  }
  if (min(out) < -600) {
    warning("There are values below -600 W/m^2!")
  }
  out
}

#' @rdname latent_priestley_taylor
#' @param weather_station Object of class weather_station
#' @export
latent_priestley_taylor.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "t1", "rad_bal", "soil_flux")
  t1 <- weather_station$measurements$t1
  rad_bal <- weather_station$measurements$rad_bal
  soil_flux <- weather_station$measurements$soil_flux
  return(latent_priestley_taylor(t1, rad_bal, soil_flux))
}


#' Latent Heat Penman Method
#'
#' Calculates the latent heat flux using the Penman-Monteith equation. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param ... Additional arguments.
#' @returns Latent heat flux in W/m\eqn{^2}.
#' @export
#'
latent_penman <- function(...) {
  UseMethod("latent_penman")
}

#' @rdname latent_penman
#' @export
#' @param datetime POSIXt object (POSIXct, POSIXlt).
#' See [base::as.POSIXlt] and [base::strptime] for conversion.
#' @param v Wind velocity in m/s.
#' @param t Temperature in °C
#' @param hum Relative humidity in %.
#' @param z Height of measurement for t, v in m.
#' @param rad_bal Radiation balance in W/m\eqn{^2}.
#' @param elev Elevation above sea level in m.
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
latent_penman.default <- function(datetime,
                                 v,
                                 t,
                                 hum,
                                 z = 2,
                                 rad_bal,
                                 elev,
                                 lat,
                                 lon, ...) {
  if (!inherits(datetime, "POSIXt")) {
    stop("datetime has to be of class POSIXt.")
  }

  if (!requireNamespace("water", quietly = TRUE)) {
    stop("Package 'water' required for latent_penman() to work.")
  }

  # day of year
  doy <- as.numeric(strftime(datetime, format = "%j"))
  # decimal hour
  lt <- as.POSIXlt(datetime)
  ut <- lt$hour + lt$min / 60 + lt$sec / 3600

  WeatherStation <- data.frame(
    wind = v,
    RH = hum,
    temp = t,
    radiation = rad_bal,
    height = z,
    lat = lat,
    long = lon,
    elev = elev
  )

  lv <- hum_evap_heat(t) # specific evaporation heat
  out <- lv * (water::hourlyET(WeatherStation, hours = ut, DOY = doy) / 3600) * (-1)
  # values of sensible bowen will be checked whether they exceed the valid data range.
  if (max(out) > 600) {
    warning("There are values above 600 W/m^2!")
  }
  if (min(out) < -600) {
    warning("There are values below -600 W/m^2!")
  }
  out
}

#' @rdname latent_penman
#' @param weather_station Object of class weather_station.
#' @export
latent_penman.weather_station <- function(weather_station, ...) {
  check_availability(
    weather_station, "datetime",
    "v1", "t1", "hum1", "z1", "rad_bal",
    "elevation", "latitude", "longitude"
  )
  datetime <- weather_station$measurements$datetime
  v <- weather_station$measurements$v1
  t <- weather_station$measurements$t1
  hum <- weather_station$measurements$hum1
  z <- weather_station$properties$z1
  rad_bal <- weather_station$measurements$rad_bal
  elev <- weather_station$location_properties$elevation
  lat <- weather_station$location_properties$latitude
  lon <- weather_station$location_properties$longitude
  return(latent_penman(
    datetime, v, t, hum, z, rad_bal,
    elev, lat, lon
  ))
}


#' Latent Heat using Monin-Obukhov length
#'
#' Calculates the latent heat flux using the Monin-Obukhov length. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param ... Additional arguments.
#' @returns Latent heat flux in W/m\eqn{^2}.
#' @export
latent_monin <- function(...) {
  UseMethod("latent_monin")
}

#' @rdname latent_monin
#' @export
#' @param hum1 Relative humidity at lower height in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param t1 Air temperature at lower height in °C.
#' @param t2 Air temperature at upper height in °C.
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param z1 Lower height of measurement in m.
#' @param z2 Upper height of measurement in m.
#' @param elev Elevation above sea level in m.
#' @inheritParams turb_roughness_length
#' @references Bendix 2004, p. 77, eq.4.6
#' @references Foken 2016, p. 61, Tab. 2.10
latent_monin.default <- function(hum1, hum2, t1, t2, v1, v2, z1 = 2, z2 = 10, elev, surface_type = NULL, obs_height = NULL, ...) {
  # calculate ustar
  if (!is.null(obs_height)) {
    ustar <- turb_ustar(v=v1, z=z1, obs_height=obs_height)
  } else if (!is.null(surface_type)) {
    ustar <- turb_ustar(v=v1, z=z1, surface_type=surface_type)
  } else {
    print("The input is not valid. Either obs_height or surface_type has to be defined.")
  }

  # calculate Monin-Obhukov-Length
  if (!is.null(obs_height)) {
    monin <- turb_flux_monin(z1=z1, z2=z2, v1=v1, v2=v2, t1=t1, t2=t2, elev=elev, obs_height=obs_height)
  } else if (!is.null(surface_type)) {
    monin <- turb_flux_monin(z1=z1, z2=z2, v1=v1, v2=v2, t1=t1, t2=t2, elev=elev, surface_type=surface_type)
  } else {
    print("The input is not valid. Either obs_height or surface_type has to be defined.")
  }
  grad_rich_no <- turb_flux_grad_rich_no(t1, t2, z1, z2, v1, v2, elev)
  moist_gradient <- hum_moisture_gradient(hum1, hum2, t1, t2, z1, z2, elev)
  air_density <- pres_air_density(elev, t1)
  lv <- hum_evap_heat(t1)
  k <- 0.4 # Karman constant
  s1 <- z2 / monin # s1 = variant of the greek letter sigma
  schmidt <- 1
  busi <- rep(NA, length(grad_rich_no))
  for (i in 1:length(busi)) {
    if (is.na(grad_rich_no[i])) {
      busi[i] <- NA
    } else if (grad_rich_no[i] <= 0) {
      busi[i] <- 0.95 * (1 - (11.6 * s1[i]))^-0.5
    } else if (grad_rich_no[i] > 0) {
      busi[i] <- 0.95 + (7.8 * s1[i])
    }
  }
  out <- (-1) * air_density * lv * ((k * ustar) / busi) * schmidt * moist_gradient

  # values of sensible bowen will be checked whether they exceed the valid data range.
  if (max(out) > 600) {
    warning("There are values above 600 W/m^2!")
  }
  if (min(out) < -600) {
    warning("There are values below -600 W/m^2!")
  }
  out
}

#' @rdname latent_monin
#' @param weather_station Object of class weather_station.
#' @export
latent_monin.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "z1", "z2", "t1", "t2", "hum1", "hum2", "v1", "v2", "elevation", "surface_type")
  hum1 <- weather_station$measurements$hum1
  hum2 <- weather_station$measurements$hum2
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  v1 <- weather_station$measurements$v1
  v2 <- weather_station$measurements$v2
  elev <- weather_station$location_properties$elevation
  surface_type <- weather_station$location_properties$surface_type
  return(latent_monin(hum1, hum2, t1, t2, v1, v2, z1, z2, elev, surface_type))
}


#' Latent Heat using Bowen Method
#'
#' Calculates the latent heat flux using the Bowen Method. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#' Values above 600 W/m\eqn{^2} and below -600 W/m\eqn{^2} will be recognized
#' as measurement mistakes and smoothed respectively.
#'
#' @param ... Additional arguments.
#' @returns Latent heat flux in W/m\eqn{^2}.
#' @export
#'
latent_bowen <- function(...) {
  UseMethod("latent_bowen")
}

#' @rdname latent_bowen
#' @export
#' @param t1 Temperature at lower height in °C.
#' @param t2 Temperature at upper height in °C.
#' @param hum1 Relative humidity at lower height in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param z1 Lower height of measurement in m.
#' @param z2 Upper height of measurement in m.
#' @param elev Elevation above sea level in m.
#' @param rad_bal Radiation balance in W/m\eqn{^2}.
#' @param soil_flux Soil flux in W/m\eqn{^2}.
#' @references Bendix 2004, p. 221, eq. 9.21
latent_bowen.default <- function(t1, t2, hum1, hum2, z1 = 2, z2 = 10, elev,
                                 rad_bal, soil_flux, ...) {
  # Calculating potential temperature delta
  t1_pot <- temp_pot_temp(t1, elev)
  t2_pot <- temp_pot_temp(t2, elev)
  dpot <- (t2_pot - t1_pot) / (z2 - z1)

  # Calculating absolute humidity delta
  af1 <- hum_absolute(hum1, t1)
  af2 <- hum_absolute(hum2, t2)
  dah <- (af2 - af1) / (z2 - z1)

  # Calculate bowen ratio
  bowen_ratio <- bowen_ratio(t1, dpot, dah)
  out <- (-1 * rad_bal - soil_flux) / (1 + bowen_ratio)

  # values of latent bowen will be checked whether they exceed the valid data range.
  if (max(out) > 600) {
    warning("There are values above 600 W/m^2!")
  }

  if (min(out) < -600) {
    warning("There are values below -600 W/m^2!")
  }
  out
}

#' @rdname latent_bowen
#' @param weather_station Object of class weather_station.
#' @export
latent_bowen.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "z1", "z2", "t1", "t2", "hum1", "hum2", "elevation", "rad_bal", "soil_flux")
  hum1 <- weather_station$measurements$hum1
  hum2 <- weather_station$measurements$hum2
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  elev <- weather_station$location_properties$elevation
  rad_bal <- weather_station$measurements$rad_bal
  soil_flux <- weather_station$measurements$soil_flux
  return(latent_bowen(t1, t2, hum1, hum2, z1, z2, elev, rad_bal, soil_flux))
}
