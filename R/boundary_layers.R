#' Mechanical internal boundary layer; lowest height.
#'
#' Calculation of the lowest height of the mechanical internal boundary layer.
#'
#' @param dist Distance to point of roughness change in m.
#'
#' @returns Height of boundary layer in m.
#' @references Bendix 2004, p. 242
#' @export
#'
bound_mech_low <- function(dist) {
  0.3 * sqrt(dist)
}

#' Mechanical internal boundary layer; average height.
#'
#' Calculation of the average height of the mechanical internal boundary layer.
#'
#' @param dist Distance to point of roughness change in m.
#'
#' @returns medium height of boundary layer in m.
#' @references Bendix 2004, p. 242
#' @export
#'
bound_mech_avg <- function(dist) {
  0.43 * dist**0.5
}

#' Thermal internal boundary layer.
#'
#' Calculation of the average height of the thermal internal boundary layer.
#'
#' @param v Windspeed in height of anemometer in m/s.
#' @param z Height of anemometer in m.
#' @param surface_type Type of surface.
#' @param temp_change_dist Distance to point of temperature change in m.
#' @param t_pot_upwind Potential temperature in upwind direction in °C.
#' @param t_pot Potential temperature at site in °C.
#' @param lapse_rate Lapse rate in °C/m.
#' @inheritParams turb_roughness_length
#' @returns Average height of the thermal boundary layer in m.
#' @references Bendix 2004, p. 242
#' @export
#'
bound_thermal_avg <- function(v, z, temp_change_dist, t_pot_upwind, t_pot, lapse_rate,
                              surface_type = NULL, obs_height = NULL) {
  # Calculate ustar
  if (!is.null(obs_height)) {
    ustar <- turb_ustar(v1=v, z1=z, obs_height=obs_height)
  } else if (!is.null(surface_type)) {
    ustar <- turb_ustar(v1=v, z1=z, surface_type=surface_type)
  } else {
    print("The input is not valid. Either obs_height or surface_type has to be defined.")
  }
  (ustar / v) * ((temp_change_dist * abs(t_pot_upwind - t_pot)) / abs(lapse_rate))**0.5
}
