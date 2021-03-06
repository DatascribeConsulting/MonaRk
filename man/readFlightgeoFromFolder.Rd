% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StatesParsing.R
\name{readFlightgeoFromFolder}
\alias{readFlightgeoFromFolder}
\title{Read RDSs from folders}
\usage{
readFlightgeoFromFolder(geoFolder, dfFolder = NULL, drop)
}
\arguments{
\item{geoFolder}{Location of geo.RDS}

\item{dfFolder}{Location of df.RDS}
}
\value{
A sf dataframe, flightgeo, that contains flight data and geodata.
}
\description{
When storing multiple geo.RDS and df.RDS in two folders, pass those folders
to this function to create one object containing all the data.
If dfFolder = NULL, read geo matrices only from folder and return 1 object
}
