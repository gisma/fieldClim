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
#' @param t Air temperature in °C.
#' @param rad_bal Radiation balance in W/m\eqn{^2}.
#' @param soil_flux Soil flux in W/m\eqn{^2}.
#' @param surface_type Surface type, for which a Priestley-Taylor coefficient will be selected. Default is for short grass.
#' @references Foken p220eq5.6.
sensible_priestley_taylor.default <- function(t, rad_bal, soil_flux, surface_type = "field", ...) {
  sc <- sc(t)
  gam <- gam(t)

  priestley_taylor_coefficient <- priestley_taylor_coefficient
  if (!surface_type %in% priestley_taylor_coefficient$surface_type) {
    values_surface <- paste(priestley_taylor_coefficient$surface_type, collapse = " , ")
    stop("'surface_type' must be one of the following: ", values_surface)
  } else if (!is.null(surface_type)) {
    alpha_pt <- priestley_taylor_coefficient[which(priestley_taylor_coefficient$surface_type == surface_type), ]$alpha
  }

  ((1 - alpha_pt) * sc + gam) * (-1 * rad_bal - soil_flux) / (sc + gam)
}

#' @rdname sensible_priestley_taylor
#' @param weather_station Object of class weather_station.
#' @export
sensible_priestley_taylor.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "t1", "rad_bal", "soil_flux")
  t1 <- weather_station$measurements$t1
  rad_bal <- weather_station$measurements$rad_bal
  soil_flux <- weather_station$measurements$soil_flux
  return(sensible_priestley_taylor(t1, rad_bal, soil_flux))
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
#' @param surface_type Type of surface.
#' @references p77eq4.6, Foken p362 Businger.
sensible_monin.default <- function(t1, t2, z1 = 2, z2 = 10, v1, v2, elev, surface_type = "field", ...) {
  p1 <- pres_p(elev, t1)
  p2 <- pres_p(elev, t2)
  monin <- turb_flux_monin(z1, z2, v1, v2, t1, t2, elev, surface_type)
  ustar <- turb_ustar(v1, z1, surface_type)
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
  (-1) * air_density * cp * (k * ustar * z2 / busi) * t_gradient
}

#' @rdname sensible_monin
#' @param weather_station Object of class weather_station.
#' @export
sensible_monin.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "t1", "t2", "z1", "z2", "v1", "v2", "elevation", "surface_type")
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  v1 <- weather_station$measurements$v1
  v2 <- weather_station$measurements$v2
  elev <- weather_station$location_properties$elevation
  surface_type <- weather_station$location_properties$surface_type
  return(sensible_monin(t1, t2, z1, z2, v1, v2, elev, surface_type))
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
#' @references p221eq9.21.
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
    out[out > 600] <- 600
  }
  if (min(out) < -600) {
    warning("There are values below -600 W/m^2!")
    out[out < -600] <- -600
  }

  return(out)
}

#' @rdname sensible_bowen
#' @param weather_station Object of class weather_station
#' @export
sensible_bowen.weather_station <- function(weather_station, ...) {
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
  return(sensible_bowen(t1, t2, hum1, hum2, z1, z2, elev, rad_bal, soil_flux))
}
