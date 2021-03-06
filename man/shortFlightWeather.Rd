% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddCovars.R
\name{shortFlightWeather}
\alias{shortFlightWeather}
\title{Shorest flight in a route sigmet overlap}
\usage{
shortFlightWeather(flightgeo, sigmetgeo)
}
\arguments{
\item{flightgeo}{A sf dataframe containing flight data and geodata}

\item{sigmetgeo}{A df with rows as sigmets, and columns as time, location, etc}
}
\value{
A T/F vector to show if a sigmet overlapped the shorest flight in the route.
}
\description{
Loop through the routes to find the shorest flight for each. For the time of 
each flight, loop through to find whether a sigmet overlapped the shortest flight's path.
Would be interesting if many longer routes have a sigmet intersecting the short path,
providing a probable cause for the deviation
}
