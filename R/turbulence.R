#' Roughness length
#'
#' Calculate the roughness length of a surface on the basis of the obstacle height or the type of the surface.
#' Possible surface types are:
#' "field", "acre", "lawn", "street", "agriculture", "settlement", "coniferous forest", "deciduous forest", "mixed forest", "city"
#' You need to specify only one, "surface_type" OR "obs_height".
#'
#' @rdname turb_roughness_length
#' @param ... Additional arguments.
#' @returns roughness length in m.
#' @references Bendix 2004, p. 239
#' @export
#'
turb_roughness_length <- function(...) {
  UseMethod("turb_roughness_length")
}

#' @rdname turb_roughness_length
#' @param surface_type Type of surface.
#' @param obs_height Height of obstacle in m.
#' @export
turb_roughness_length.default <- function(surface_type = NULL, obs_height = NULL, ...) {
  surface_properties <- surface_properties
  if (!is.null(obs_height)) {
    obs_height * 0.1
  } else if (!is.null(surface_type)) {
    surface_properties[which(surface_properties$surface_type == surface_type), ]$roughness_length
  } else {
    print("The input is not valid. Please check the input values.")
  }
}

#' @rdname turb_roughness_length
#' @param weather_station Object of class weather_station.
#' @export
turb_roughness_length.weather_station <- function(weather_station, ...) {
  check_availability(weather_station, "obs_height", "surface_type")
  obs_height <- weather_station$location_properties$obs_height
  surface_type <- weather_station$location_properties$surface_type
  if (is.null(obs_height) & is.null(surface_type)) {
    stop("Either surface_type or obs_height must be set.")
  }
  return(turb_roughness_length(surface_type, obs_height))
}


#' Displacement height
#'
#' Calculate the displacement height, caused by an obstacle (e.g. a crop field).
#' Works for vegetation only.
#'
#' @rdname turb_displacement
#' @param ... Additional arguments.
#' @returns Displacement height in m.
#' @export
#'
turb_displacement <- function(...) {
  UseMethod("turb_displacement")
}

#' @rdname turb_displacement
#' @param obs_height Height of vegetation in m.
#' @param surroundings Choose either 'vegetation' or 'city'.
#' @export
#' @references Bendix 2004, p. 241
turb_displacement.default <- function(obs_height, surroundings = "vegetation", ...) {
  if (surroundings == "vegetation") {
    (2 / 3) * obs_height # for vegetation
  } else if (surroundings == "city") {
    0.8 * obs_height # for dense housing
  } else {
    stop("Please set 'surroundings' to either 'vegetation' or 'city'.")
  }
}

#' @rdname turb_displacement
#' @param weather_station Object of class weather_station
#' @export
turb_displacement.weather_station <- function(weather_station, surroundings = "vegetation", ...) {
  check_availability(weather_station, "obs_height")
  obs_height <- weather_station$location_properties$obs_height
  return(turb_displacement(obs_height, surroundings))
}


#' Friction velocity
#'
#' Calculate the friction velocity of the surface.
#'
#' @rdname turb_ustar
#' @param ... Additional arguments.
#' @returns Friction velocity in m/s.
#' @references Bendix 2004, p. 239
#' @export
#'
turb_ustar <- function(...) {
  UseMethod("turb_ustar")
}

#' @rdname turb_ustar
#' @param v Windspeed in height of anemometer in m/s.
#' @param z Height of anemometer in m.
#' @inheritParams turb_roughness_length
#' @export
turb_ustar.default <- function(v, z, surface_type = NULL, obs_height = NULL, ...) {
  if (!is.null(obs_height)) {
    z0 <- turb_roughness_length(obs_height=obs_height)
  } else if (!is.null(surface_type)) {
    z0 <- turb_roughness_length(surface_type=surface_type)
  } else {
    print("The input is not valid. Either obs_height or surface_type has to be defined.")
  }
  ustar <- (v * 0.4) / log(z / z0)
  if (any(is.infinite(ustar))) {
    print("One or more ustar values are infinite. They are set to NA.")
    ustar[is.infinite(ustar)] <- NA
  }
  ustar
}

#' @rdname turb_ustar
#' @param weather_station Object of class weather_station.
#' @param obs_height Height of obstacle in m.
#' @export
turb_ustar.weather_station <- function(weather_station, obs_height = NULL, ...) {
  check_availability(weather_station, "v1", "z1")
  v <- weather_station$measurements$v1
  z <- weather_station$properties$z1
  if (!is.null(obs_height)) {
    return(turb_ustar(v, z, obs_height = obs_height))
  } else {
    check_availability(weather_station, "surface_type")
    surface_type <- weather_station$location_properties$surface_type
    return(turb_ustar(v, z, surface_type = surface_type))
  }
}
