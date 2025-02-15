% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radiation.R
\name{rad_sw_out}
\alias{rad_sw_out}
\alias{rad_sw_out.default}
\alias{rad_sw_out.weather_station}
\title{Shortwave outgoing radiation}
\usage{
rad_sw_out(...)

\method{rad_sw_out}{default}(
  datetime,
  lon,
  lat,
  elev,
  temp,
  slope,
  exposition,
  surface_type,
  ...
)

\method{rad_sw_out}{weather_station}(weather_station, ...)
}
\arguments{
\item{...}{Additional arguments.}

\item{datetime}{Datetime of class \code{POSIXlt}. See \code{\link[base:as.POSIXlt]{base::as.POSIXlt()}}.
Make sure to provide the correct timezone information!}

\item{lon}{Longitude in degree.}

\item{lat}{Latitude in degree.}

\item{elev}{Elevation above sea level in m.}

\item{temp}{Air temperature in degree Celcius.}

\item{slope}{Slope in degree.}

\item{exposition}{Exposition in degree.}

\item{surface_type}{Surface type.
Allowed values are: field, acre, lawn, street, agriculture, settlement, coniferous forest, deciduous forest, mixed forest, city, water, shrub.
EXCEPTION: for functions related to Priestley-Taylor methods,
allowed values are: field, bare soil, coniferous forest, water, wetland, spruce forest.}

\item{weather_station}{Object of class \code{weather_station}.}
}
\value{
W/m\eqn{^2}.
}
\description{
Reflected shortwave incoming radiation.
}
\references{
Bendix 2004, p. 45 eq. 3.1.
}
