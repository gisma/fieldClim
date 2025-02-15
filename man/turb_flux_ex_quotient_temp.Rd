% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/turbulent_flux.R
\name{turb_flux_ex_quotient_temp}
\alias{turb_flux_ex_quotient_temp}
\alias{turb_flux_ex_quotient_temp.default}
\alias{turb_flux_ex_quotient_temp.weather_station}
\title{Exchange quotient for heat transmission}
\usage{
turb_flux_ex_quotient_temp(...)

\method{turb_flux_ex_quotient_temp}{default}(
  t1,
  t2,
  z1 = 2,
  z2 = 10,
  v1,
  v2,
  elev,
  surface_type = NULL,
  obs_height = NULL,
  ...
)

\method{turb_flux_ex_quotient_temp}{weather_station}(weather_station, ...)
}
\arguments{
\item{...}{Additional arguments.}

\item{t1}{Temperature at lower height (e.g. height of anemometer) in °C.}

\item{t2}{Temperature at upper height in degrees C.}

\item{z1}{Lower height of measurement (e.g. height of anemometer) in m.}

\item{z2}{Upper height of measurement in m.}

\item{v1}{Windspeed at lower height (e.g. height of anemometer) in m/s.}

\item{v2}{Windspeed at upper height in m/s.}

\item{elev}{Elevation above sea level in m.}

\item{surface_type}{Type of surface. Options: field, acre, lawn, street, agriculture, settlement, coniferous forest, deciduous forest, mixed forest, city, water, shrub}

\item{obs_height}{Height of obstacle in m.}

\item{weather_station}{Object of class weather_station}
}
\value{
Exchange quotient for heat transmission in kg/(m*s).
}
\description{
Calculation of the exchange quotient of the turbulent heat transmission.
}
\references{
Foken 2016, p. 362: Businger.
}
