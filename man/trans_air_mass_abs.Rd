% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transmittance.R
\name{trans_air_mass_abs}
\alias{trans_air_mass_abs}
\alias{trans_air_mass_abs.default}
\alias{trans_air_mass_abs.weather_station}
\title{Absolute optical air mass}
\usage{
trans_air_mass_abs(...)

\method{trans_air_mass_abs}{default}(datetime, lon, lat, elev, temp, ...)

\method{trans_air_mass_abs}{weather_station}(weather_station, ...)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[=pres_p.default]{pres_p.default}}
  \describe{
    \item{\code{g}}{Gravitational acceleration in \eqn{m \cdot s^{-2}}, default 9.81.}
    \item{\code{rl}}{Specific gas constant for air in \eqn{m^2 \cdot s^{-2} \cdot K^{-1}}, default 287.05.}
  }}

\item{datetime}{Datetime of class \code{POSIXlt}. See \code{\link[base:as.POSIXlt]{base::as.POSIXlt()}}.
Make sure to provide the correct timezone information!}

\item{lon}{Longitude in degree.}

\item{lat}{Latitude in degree.}

\item{elev}{Elevation above sea level in m.}

\item{temp}{Air temperature in degree Celcius.}

\item{weather_station}{Object of class \code{weather_station}.}
}
\value{
Unitless.
}
\description{
Absolute optical air mass
}
\references{
Bendix 2004, p. 247.
}
