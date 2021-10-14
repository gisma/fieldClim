#' Example data of a weather station.
#'
#' A dataset containing different weather station measurements over the course of one day (2018-07-28).
#' The weather station is located near Caldern, Hesse, Germany.
#' Latitude: 50.840503
#' Longitude: 8.683300
#' Meters above sea level (elevation): 270
#' Heights of measurements: 2m (lower level) & 10m (upper level)
#' Surface type: "field"
#' Ground texture: "clay"
#'
#' @format A data frame with 288 rows and 21 variables:
#' \describe{
#'   \item{datetime}{Date and time of measurement in POSIXt-format.}
#'   \item{t1}{Temperature in °C at 2m height (lower level).}
#'   \item{hum1}{Humidity in % at 2m height (lower level).}
#'   \item{t2}{Temperature in °C at 10m height (upper level).}
#'   \item{hum2}{Humidity in % at 10m height (upper level).}
#'   \item{rad_sw_in}{Incoming shortwave radiation in W/m².}
#'   \item{rad_sw_out}{Outgoing shortwave radiation in W/².}
#'   \item{rad_lw_in}{Incoming longwave radiation in W/m².}
#'   \item{rad_lw_out}{Outgoing longwave radiation in W/m².}
#'   \item{rad_sw_bal}{Shortwave radiation balance in W/m².}
#'   \item{rad_lw_bal}{Longwave radiation balance in W/m².}
#'   \item{albedo}{Albedo of ground.}
#'   \item{rad_bal}{Total radiation balance in W/m².}
#'   \item{water_vol_soil}{Moisture of ground in %.}
#'   \item{t_surface}{Surface temperature in °C.}
#'   \item{ts1}{Temperature soil, 30 cm depth.}
#'   \item{heatflux_soil}{Soil heat flux in W/m².}
#'   \item{v1}{Wind velocity in m/s at 2m height (lower level).}
#'   \item{v2}{Wind velocity in m/s at 10m height (upper level).}
#'   \item{p1}{Air pressure in hPa (lower level).}
#'   \item{p2}{Air pressure in hPa (upper level).}
#' }
#' @source Provided by Prof. Dr. Jörg Bendix, Laboratory of Climatology and Remote Sensing, Philipps-University of Marburg.
"weather_station_example_data"

#' Emissivity and roughness length for different surfaces.
#'
#' A dataset containing values for emissivity and roughness length for different surface types.
#'
#' @format A data frame with 12 rows and 4 variables:
#' \describe{
#'   \item{surface_type}{Type of surface as character}
#'   \item{emissivity}{Emissivity of the chosen surface}
#'   \item{roughness_length}{Roughness length of surface in m}
#'   \item{albedo}{Albedo of the certain surface type as decimal number}
#' }
"surface_properties"

#' Priestley-Taylor coefficient for different surface types.
#'
#' A dataset containing values for priestley-taylor coefficiencts for different surface types.
#'
#' @format A data frame with 6 rows and 2 variables:
#' \describe{
#'   \item{surface_type}{Type of surface as character}
#'   \item{alpha}{coefficient value as numeric}
#' }
"priestley_taylor_coefficient"
