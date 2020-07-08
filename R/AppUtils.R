### Functions used only by the app

#' Load and Organize in-flight data for a route. 
#'
#' This function takes the name of a route, the flight data and geo data for this route,
#' and returns a df used by leaflet and plotly within the app to display in-flight information.
#'
#' @param route A string like "SAN SFO" describing the start and end of a route
#' @param flightgeoCovar A dataframe of all the flights information only within the given route; "df.RDS"
#' @param geomatrix A list of matrices containing 10-second data for lat, long, alt, velocity; "geo.RDS"
#' @return A df with rows representing time, repeated for each flight, columns containing location, weather, delay, and other information shown in tooltips.
#' @export
makeInFlightDf <- function(route, flightgeoCovar, geomatrix){
  #routeIndex <- which(flightgeoCovar$route == route)
  if(nrow(flightgeoCovar) > 0){
    inFlightDf <- do.call(rbind, lapply(1:nrow(flightgeoCovar), function(x){
      mat <- geomatrix[[as.numeric(row.names(flightgeoCovar[x,]))]] 
      data.frame(id = rep(flightgeoCovar$id[x], nrow(mat)),
                 lon = mat[,1],
                 lat = mat[,2],
                 elev = mat[,3],
                 time = mat[,4],
                 thuman = as.POSIXct(mat[,4], origin = "1970-01-01"), 
                 minfromstart = (mat[,4] - mat[1,4])/60,
                 speed = mat[,5],
                 thisWeather = rep(flightgeoCovar$thisWeather[x], nrow(mat)),
                 regionWeather = rep(flightgeoCovar$regionWeather[x], nrow(mat)),
                 stormEvent    = rep(flightgeoCovar$stormEvent[x], nrow(mat)),
                 anyDelay      = rep((flightgeoCovar$weatherDelay[x] > 0 |
                                        flightgeoCovar$nasDelay[x] > 0 |
                                        flightgeoCovar$carrierDelay[x] > 0 |
                                        flightgeoCovar$lateAircraftDelay[x] > 0 |
                                        flightgeoCovar$securityDelay[x] > 0), 
                                     nrow(mat)),
                 weatherDelay  = rep(flightgeoCovar$weatherDelay[x] > 0, nrow(mat)),
                 nasDelay      = rep(flightgeoCovar$nasDelay[x] > 0, nrow(mat)),
                 kmincreaseFlight = rep(flightgeoCovar$kmincreaseFlight[x], nrow(mat)),
                 minincrease = rep(flightgeoCovar$minincrease[x], nrow(mat)),
                 cost = rep(flightgeoCovar$cost[x], nrow(mat)),
                 date = rep(flightgeoCovar$date[x], nrow(mat)),
                 airline = rep(flightgeoCovar$airline[x], nrow(mat)),
                 manufacturer = rep(flightgeoCovar$manufacturer[x], nrow(mat))
      )  
    }))
  }
  return(inFlightDf)
}


#' Compress numeric values to redable dollar values
#'
#' Round numbers and display digits as human-readable terms like "K" for thousand,
#' "M" for million, and so on.
#'
#' @param tx a number
#' @return A string with a rounded number and a suffix describing its magnitude
#' @export
comprss <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                      c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), digits = 1), 
        c("","K","M","B","T")[div] )}