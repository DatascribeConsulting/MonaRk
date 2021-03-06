% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StatesParsing.R
\name{mapFlightgeo}
\alias{mapFlightgeo}
\title{Map flight}
\usage{
mapFlightgeo(flightgeo)
}
\arguments{
\item{flightgeo}{A dataframe of all the flights information within routes of interest including geo data.}
}
\value{
an interactive leaflet map
}
\description{
Use leaflet to show flights on top of a basemap. When a flight is clicked
additional information is shown in a tooltip.
}
