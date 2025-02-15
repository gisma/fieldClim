% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/humidity.R
\name{hum_specific}
\alias{hum_specific}
\alias{hum_specific.default}
\alias{hum_specific.weather_station}
\title{Specific humidity}
\usage{
hum_specific(...)

\method{hum_specific}{default}(rh, temp, elev, ...)

\method{hum_specific}{weather_station}(weather_station, ...)
}
\arguments{
\item{...}{Additional arguments.}

\item{rh}{Relative humidity in \%.}

\item{temp}{Temperature in °C.}

\item{elev}{Elevation above sea level in m.}

\item{weather_station}{Object of class \code{weather_station}.}
}
\value{
Specific humidity in kg/kg.
}
\description{
Calculates specific humidity from vapor pressure and air pressure.
}
\references{
Bendix 2004, p. 262.
}
