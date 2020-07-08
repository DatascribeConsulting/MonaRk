# Needed by app AND parsing functions

#' Return merged flight + geo object
#'
#' This function takes a list of geomatrices (geo.RDS) and flight data (df.RDS) and 
#' returns essentially the same flight data df with a geo column of class sfc_LINESTRING. 
#' Since there are 5 columns within the geomatrix, and only 4 can be within a sfc column, one must be dropped. 
#' The resulting sf geo dataframe can be used to map routes and further analysis.
#' 
#' @param geomatrix A list of matrices containing 10-second data for lat, long, alt, velocity; "geo.RDS"
#' @param flights A dataframe of all the flights information within routes of interest; "df.RDS"
#' @param simplify A T/F option of whether to simplify the geo data after calculating the distance
#' @param drop User must choose to drop velocity, elevation, or time to create the sf geo dataframe
#' @return A df with rows representing a flight, with many rows containing flight data and geodata.
#' @export

createFlightgeo <- function (geomatrix, flights, simplify = F, 
                             drop = c("velocity", "altitude", "time")){
  
  drop <- match.arg(drop)
  
  if(ncol(geomatrix[[1]]) > 3){
    geomatrix <- lapply(geomatrix, function(x){
      if (drop == "altitude") {
        return(x[ ,c(1,2,4,5)])
      } else if (drop == "time") {
        return(x[ ,c(1,2,3,5)])
      } else if (drop == "velocity"){
        return(x[ ,c(1,2,3,4)])
      } else {
        return(x[ ,c(1,2,3,4)])
      }
    })
  }
  
  geo <- st_sf(flights, g = st_sfc(lapply(geomatrix, st_linestring)), crs = 4326)
  
  # calculate geodistance with a crs that has smaller biases, before any simplification
  geo$distance <- as.vector(sf::st_length(st_cast(geo,"LINESTRING")))
  
  if(simplify){
    geo <- sf::st_simplify(st_zm(geo), preserveTopology = TRUE, dTolerance = .01)
  }
  
  return(geo)
}
