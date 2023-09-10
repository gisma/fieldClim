#' Sensible Heat Priestley-Taylor Method
#'
#' Calculates the Sensible heat flux using the Priestley-Taylor method. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param ... Additional arguments.
#' @returns Sensible heat flux in W/m\eqn{^2}.
#' @export
sensible_priestley_taylor <- function(...) {
  UseMethod("sensible_priestley_taylor")
}

#' @rdname sensible_priestley_taylor
#' @export
#' @param temp Air temperature in °C.
#' @param rad_bal Radiation balance in W/m\eqn{^2}.
#' @param soil_flux Soil flux in W/m\eqn{^2}.
#' @param surface_type Surface type, for which a Priestley-Taylor coefficient will be selected. Options: `r priestley_taylor_coefficient$surface_type`
#' @references Foken 2016, p. 220, eq. 5.6
sensible_priestley_taylor.default <- function(temp, rad_bal, soil_flux, surface_type, ...) {
  sc <- sc(temp)
  gam <- gam(temp)

  priestley_taylor_coefficient <- priestley_taylor_coefficient
  if (!surface_type %in% priestley_taylor_coefficient$surface_type) {
    values_surface <- paste(priestley_taylor_coefficient$surface_type, collapse = " , ")
    stop("'surface_type' must be one of the following: ", values_surface)
  } else if (!is.null(surface_type)) {
    alpha_pt <- priestley_taylor_coefficient[which(priestley_taylor_coefficient$surface_type == surface_type), ]$alpha
  }

  out <- ((1 - alpha_pt) * sc + gam) * (-1 * rad_bal - soil_flux) / (sc + gam)

  # values of sensible bowen will be checked whether they exceed the valid data range.
  if (max(out) > 600) {
    warning("There are values above 600 W/m^2!")
  }
  if (min(out) < -600) {
    warning("There are values below -600 W/m^2!")
  }
  out
}

#' @rdname sensible_priestley_taylor
#' @inheritParams sol_julian_day
#' @export
sensible_priestley_taylor.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "temp", "rad_bal", "soil_flux", "surface_type")
  temp <- weather_station$temp
  rad_bal <- weather_station$rad_bal
  soil_flux <- weather_station$soil_flux
  surface_type <- weather_station$surface_type
  return(sensible_priestley_taylor(temp, rad_bal, soil_flux, surface_type = surface_type))
}


#' Sensible Heat using Monin-Obukhov length
#'
#' Calculates the sensible heat flux using the Monin-Obukhov length. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param ... Additional arguments.
#' @returns Sensible heat flux in W/m\eqn{^2}.
#' @export
sensible_monin <- function(...) {
  UseMethod("sensible_monin")
}

#' @rdname sensible_monin
#' @export
#' @param t1 Air temperature at lower height in °C.
#' @param t2 Air temperature at upper height in °C.
#' @param z1 Lower height of measurement in m.
#' @param z2 Upper height of measurement in m (Use highest point of measurement as values are less disturbed).
#' @param v1 Windspeed at lower height (e.g. height of anemometer) in m/s.
#' @param v2 Windspeed at upper height in m/s.
#' @param elev Elevation above sea level in m.
#' @inheritParams turb_roughness_length
#' @references Bendix 2004, p. 77, eq. 4.6,
#' @references Foken 2016, p. 362: Businger
sensible_monin.default <- function(t1, t2, z1 = 2, z2 = 10, v1, v2, elev, surface_type = NULL, obs_height = NULL, ...) {
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
  cp <- 1004.834
  k <- 0.35
  s1 <- z2 / monin

  # temperature gradient
  t_gradient <- (temp_pot_temp(t2, elev) - temp_pot_temp(t1, elev)) / log(z2 - z1)

  air_density <- pres_air_density(elev, t1)
  busi <- rep(NA, length(grad_rich_no))
  for (i in 1:length(busi)) {
    if (is.na(grad_rich_no[i])) {
      busi[i] <- NA
    } else if (grad_rich_no[i] <= 0) {
      busi[i] <- 0.74 * (1 - 9 * s1[i])^(-0.5)
    } else if (grad_rich_no[i] > 0) {
      busi[i] <- 0.74 + 4.7 * s1[i]
    }
  }

  out <- (-1) * air_density * cp * (k * ustar * z2 / busi) * t_gradient

  # values of sensible bowen will be checked whether they exceed the valid data range.
  if (max(out) > 600) {
    warning("There are values above 600 W/m^2!")
  }
  if (min(out) < -600) {
    warning("There are values below -600 W/m^2!")
  }
  out
}

#' @rdname sensible_monin
#' @inheritParams sol_julian_day
#' @export
sensible_monin.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "t1", "t2", "z1", "z2", "v1", "v2", "elev")
  t1 <- weather_station$t1
  t2 <- weather_station$t2
  z1 <- weather_station$z1
  z2 <- weather_station$z2
  v1 <- weather_station$v1
  v2 <- weather_station$v2
  elev <- weather_station$elev
  obs_height <- weather_station$obs_height
  if (!is.null(obs_height)) {
    return(sensible_monin(t1, t2, z1, z2, v1, v2, elev, obs_height = obs_height))
  } else {
    check_availability(weather_station, "surface_type")
    surface_type <- weather_station$surface_type
    return(sensible_monin(t1, t2, z1, z2, v1, v2, elev, surface_type = surface_type))
  }
}



#' Sensible Heat using Bowen Method
#'
#' Calculates the sensible heat flux using the Bowen Method. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param ... Additional arguments.
#' @returns Sensible heat flux in W/m\eqn{^2}.
#' @export
#'
sensible_bowen <- function(...) {
  UseMethod("sensible_bowen")
}

#' @rdname sensible_bowen
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
sensible_bowen.default <- function(t1, t2, hum1, hum2, z1 = 2, z2 = 10, elev, rad_bal, soil_flux, ...) {
  # Calculating potential temperature delta
  t1_pot <- temp_pot_temp(t1, elev)
  t2_pot <- temp_pot_temp(t2, elev)
  dpot <- (t2_pot - t1_pot) / (z2 - z1)

  # Calculating absolute humidity
  af1 <- hum_absolute(hum1, t1)
  af2 <- hum_absolute(hum2, t2)
  dah <- (af2 - af1) / (z2 - z1)

  # Calculate bowen ratio
  bowen_ratio <- bowen_ratio(t1, dpot, dah)
  out <- (-rad_bal - soil_flux) * bowen_ratio / (1 + bowen_ratio)

  # values of sensible bowen will be checked whether they exceed the valid data range.
  if (max(out) > 600) {
    warning("There are values above 600 W/m^2!")
  }
  if (min(out) < -600) {
    warning("There are values below -600 W/m^2!")
  }
  out
}

#' @rdname sensible_bowen
#' @param weather_station Object of class weather_station
#' @export
sensible_bowen.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "z1", "z2", "t1", "t2", "hum1", "hum2", "elev", "rad_bal", "soil_flux")
  hum1 <- weather_station$hum1
  hum2 <- weather_station$hum2
  t1 <- weather_station$t1
  t2 <- weather_station$t2
  z1 <- weather_station$z1
  z2 <- weather_station$z2
  elev <- weather_station$elev
  rad_bal <- weather_station$rad_bal
  soil_flux <- weather_station$soil_flux
  return(sensible_bowen(t1, t2, hum1, hum2, z1, z2, elev, rad_bal, soil_flux))
}
