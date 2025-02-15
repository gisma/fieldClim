% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensible.R
\name{sensible_monin}
\alias{sensible_monin}
\alias{sensible_monin.default}
\alias{sensible_monin.weather_station}
\title{Sensible Heat using Monin-Obukhov length}
\usage{
sensible_monin(...)

\method{sensible_monin}{default}(
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

\method{sensible_monin}{weather_station}(weather_station, ...)
}
\arguments{
\item{...}{Additional arguments.}

\item{t1}{Air temperature at lower height in °C.}

\item{t2}{Air temperature at upper height in °C.}

\item{z1}{Lower height of measurement in m.}

\item{z2}{Upper height of measurement in m (Use highest point of measurement as values are less disturbed).}

\item{v1}{Windspeed at lower height (e.g. height of anemometer) in m/s.}

\item{v2}{Windspeed at upper height in m/s.}

\item{elev}{Elevation above sea level in m.}

\item{surface_type}{Type of surface. Options: field, acre, lawn, street, agriculture, settlement, coniferous forest, deciduous forest, mixed forest, city, water, shrub}

\item{obs_height}{Height of obstacle in m.}

\item{weather_station}{Object of class weather_station}
}
\value{
Sensible heat flux in W/m\eqn{^2}.
}
\description{
Calculates the sensible heat flux using the Monin-Obukhov length. Negative
flux signifies flux away from the surface, positive values signify flux
towards the surface.
}
\references{
Bendix 2004, p. 77, eq. 4.6,

Foken 2016, p. 362: Businger
}
