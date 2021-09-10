#' Sensible Heat Priestley-Taylor Method
#'
#' Calculates the Sensible heat flux using the Priestley-Taylor method. Negative
#' heat flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Sensible heat flux in W/m^2.
#' @export
#'
sensible_priestley_taylor <- function (...) {
  UseMethod("sensible_priestley_taylor")
}

#' @rdname sensible_priestley_taylor
#' @method sensible_priestley_taylor numeric
#' @export
#' @param t Air temperature in °C.
#' @param rad_bal Radiation balance in W/m^2.
#' @param soil_flux Soil flux in W/m^2.
#' @param surface_type Surface type, for which a Priestley-Taylor coefficient will be selected. Default is for short grass.
sensible_priestley_taylor.numeric <- function(t, rad_bal, soil_flux, surface_type = "field", ...){
  sc <- sc(t)
  lamb <- lamb(t)

  priestley_taylor_coefficient <- priestley_taylor_coefficient
  if(!surface_type %in% priestley_taylor_coefficient$surface_type){
    values_surface <- paste(priestley_taylor_coefficient$surface_type, collapse = " , ")
    stop("'surface_type' must be one of the following: ", values_surface)
  } else if(!is.null(surface_type)){
    alpha_pt <- priestley_taylor_coefficient[which(priestley_taylor_coefficient$surface_type == surface_type),]$alpha
  }

  alpt <- coefficient
  QH_TP <- ((1-alpt)*sc+lamb)*(-1*rad_bal-soil_flux)/(sc+lamb)
  return(QH_TP)
}

#' @rdname sensible_priestley_taylor
#' @method sensible_priestley_taylor weather_station
#' @param weather_station Object of class weather_station
#' @export
sensible_priestley_taylor.weather_station <- function(weather_station, ...){
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
#' @param ... Additional parameters passed to later functions.
#' @return Sensible heat flux in W/m^2.
#' @export
sensible_monin <- function (...) {
  UseMethod("sensible_monin")
}

#' @rdname sensible_monin
#' @method sensible_monin numeric
#' @export
#' @param t1 Air temperature at lower height in °C.
#' @param t2 Air temperature at upper height in °C.
#' @param p1 Pressure at lower height in hPa.
#' @param p2 Pressure at upper height in hPa.
#' @param z1 Lower height of measurement in m.
#' @param z2 Upper height of measurement in m.
#' @param monin Monin-Obukhov-Length in m.
#' @param ustar Friction velocity in m/s.
#' @param grad_rich_no Gradient-Richardson-Number.
sensible_monin.numeric <- function(t1, t2, p1, p2, z1 = 2, z2 = 10,
                           monin, ustar, grad_rich_no, ...) {
  cp <- 1004.834
  k <- 0.4
  s1 <- z2/monin

  # temperature gradient
  t_gradient <- (temp_pot_temp(t2, p2)-temp_pot_temp(t1, p1)) / log(z2-z1)

  air_density <- pres_air_density(p1, t1)
  busi <- rep(NA, length(grad_rich_no))
  for(i in 1:length(busi)){
    if(is.na(grad_rich_no[i]))   {busi[i] <- NA}
    else if(grad_rich_no[i] <= 0){busi[i] <- 0.74 * (1 - 9 * s1[i])^(-0.5)}
    else if(grad_rich_no[i] >  0){busi[i] <- 0.74 + 4.7 * s1[i]}
  }
  # hier noch mit Herrn Bendix drüberschauen (z2 oder z1?) geklärt, z2 ok (z2 ungestörter)
  QH <- (-1) * air_density * cp * (k * ustar * z2 / busi) * t_gradient
  return(QH)
}

#' @rdname sensible_monin
#' @method sensible_monin weather_station
#' @param weather_station Object of class weather_station.
#' @export
sensible_monin.weather_station <- function(weather_station, ...){
  check_availability(weather_station, "z1", "z2", "t1", "t2", "p1", "p2")
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  p1 <- weather_station$measurements$p1
  p2 <- weather_station$measurements$p2
  monin <- turb_flux_monin(weather_station)
  ustar <- turb_ustar(weather_station)
  grad_rich_no <- turb_flux_grad_rich_no(weather_station)
  return(sensible_monin(t1, t2, p1, p2, z1, z2,
                      monin, ustar, grad_rich_no))
}



#' Sensible Heat using Bowen Method
#'
#' Calculates the sensible heat flux using the Bowen Method. Negative
#' flux signifies flux away from the surface, positive values signify flux
#' towards the surface.
#'
#' @param ... Additional parameters passed to later functions.
#' @return Sensible heat flux in W/m^2
#' @export
#'
sensible_bowen <- function (...) {
  UseMethod("sensible_bowen")
}

#' @rdname sensible_bowen
#' @method sensible_bowen numeric
#' @export
#' @param t1 Temperature at lower height (e.g. height of anemometer) in °C.
#' @param t2 Temperature at upper height in °C.
#' @param hum1 Relative humidity at lower height (e.g. height of anemometer) in %.
#' @param hum2 Relative humidity at upper height in %.
#' @param p1 Air pressure at lower height in hPa.
#' @param p2 Air pressure at upper height in hPa.
#' @param z1 Lower height of measurement (e.g. height of anemometer) in m.
#' @param z2 Upper height of measurement in m.
#' @param rad_bal Radiation balance in W/m^2.
#' @param soil_flux Soil flux in W/m^2.
sensible_bowen.numeric <- function(t1, t2, hum1, hum2, p1, p2, z1 = 2, z2 = 10,
                           rad_bal, soil_flux, ...){

  # Calculating potential temperature delta
  t1_pot <- temp_pot_temp(t1, p1)
  t2_pot <- temp_pot_temp(t2, p2)
  dpot <- (t2_pot - t1_pot) / (z2 - z1)

  # Calculating absolute humidity
  af1 <- hum_absolute(hum_vapor_pres(hum1, t1), t1_pot)
  af2 <- hum_absolute(hum_vapor_pres(hum2, t2), t2_pot)
  dah <- (af2 - af1) / (z2 - z1)

  # Calculate bowen ratio
  bowen_ratio <- bowen_ratio(t1, dpot, dah)
  out <- ((-1) * rad_bal - soil_flux) * (bowen_ratio / (1 + bowen_ratio))
  return(out)
}

#' @rdname sensible_bowen
#' @method sensible_bowen weather_station
#' @param weather_station Object of class weather_station
#' @export
sensible_bowen.weather_station <- function(weather_station, ...){
  check_availability(weather_station, "z1", "z2", "t1", "t2", "p1", "p2",
                     "hum1", "hum2", "rad_bal", "soil_flux")
  hum1 <- weather_station$measurements$hum1
  hum2 <- weather_station$measurements$hum2
  t1 <- weather_station$measurements$t1
  t2 <- weather_station$measurements$t2
  z1 <- weather_station$properties$z1
  z2 <- weather_station$properties$z2
  p1 <- weather_station$measurements$p1
  p2 <- weather_station$measurements$p2
  rad_bal <- weather_station$measurements$rad_bal
  soil_flux <- weather_station$measurements$soil_flux
  return(sensible_bowen(t1, t2, hum1, hum2, p1, p2, z1, z2,
                      rad_bal, soil_flux))
}







