% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soil.R
\name{soil_thermal_cond}
\alias{soil_thermal_cond}
\alias{soil_thermal_cond.default}
\alias{soil_thermal_cond.weather_station}
\title{Soil thermal conductivity}
\usage{
soil_thermal_cond(...)

\method{soil_thermal_cond}{default}(texture, moisture, ...)

\method{soil_thermal_cond}{weather_station}(weather_station, ...)
}
\arguments{
\item{...}{Additional arguments.}

\item{texture}{Soil texture. Either "sand", "clay", or "peat".}

\item{moisture}{Soil moisture in cubic meter/cubic meter.}

\item{weather_station}{Object of class \code{weather_station}.}
}
\value{
Soil thermal conductivity in W/m/K.
}
\description{
Calculates soil thermal conductivity from soil texture and soil moisture.
}
\details{
Works by linearly interpolating thermal conductivity based on measured data.
}
\references{
Bendix 2004, p. 254.
}
