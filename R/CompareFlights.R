################### Distance comparison calculations ###################

#' Calculate extra distance and duration for flights 
#'
#' After calculating the distance and duration for each flight, pass to addDistanceCovars to find
#' the shortest distance and duration for a route and the direct distance of the route, then
#' calculate the diffence to find "extra" distance and duration
#'
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @return A sf dataframe with additional columns containing extra distance and duration
#' @export
compareFlights <- function(flightgeo){
  print("Starting distance")
  distanceCovars <- addDistanceCovars(flightgeo)
  flightgeo <- cbind(flightgeo, distanceCovars)
  
  # distance comparison
  flightgeo$kmincrease <- (flightgeo$distance - flightgeo$shortestDist)/1000
  flightgeo$kmpercincrease <- 100*(flightgeo$distance - flightgeo$shortestDist)/flightgeo$shortestDist
  flightgeo$kmincreaseFlight <- (flightgeo$distance - flightgeo$shortestDistFlight)/1000
  flightgeo$kmpercincreaseFlight <- 100*(flightgeo$distance - flightgeo$shortestDistFlight)/flightgeo$shortestDistFlight
  
  # duration comparison
  flightgeo$minincrease <- flightgeo$duration - flightgeo$shortestDur
  flightgeo$minpercincrease <- 100*(flightgeo$duration - flightgeo$shortestDur)/flightgeo$shortestDur
  print("Distances done")
  
  return(flightgeo)
}



#' Find shortest distance and duration for routes
#'
#' Loop through the routes in flightgeo, find the
#' existing flight path with the shortest duration, shortest distance("shortestDistFlight"),
#' as well as the direct geo distance between airports ("shortestDist"), the number of 
#' flights within this route ("routeCount"). Also returns starting and ending airports 
#' latitude for data quality analyses.
#' This function needs to be run after parsing flight data and adding covars,
#' called within compareFlights() for a shorter pipeline. 
#'
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @return A sf dataframe with additional columns containing routeCount, shortest dist and dur
#' @export
addDistanceCovars <- function(flightgeo){
  
  airports <- fread("inst/extdata/helpers/airports.csv")
  
  # initialize
  routeCount <- rep(NA, nrow(flightgeo))
  shortestDist <- rep(NA, nrow(flightgeo))
  shortestDistFlight <- rep(NA, nrow(flightgeo))
  shortestDur <- rep(NA, nrow(flightgeo))
  startLat <- rep(NA, nrow(flightgeo))
  endLat <- rep(NA, nrow(flightgeo))
  
  # gather shortest distances and durations per route
  for(i in unique(flightgeo$route)) {
    
    index <- which(flightgeo$route == i)
    
    routeCount[index] <- length(index)
    
    dist <- flightgeo$distance[index]
    dur <- flightgeo$duration[index]
    
    shortestDistFlight[index] <- min(dist)
    shortestDur[index] <- min(dur)
    
    ## Change it to this after rerun
    start <- as.numeric(airports[which(airports$name == flightgeo$airportStartName[index[1]] &
                                         airports$iata == flightgeo$airportStart[index[1]])[1], c("X", "Y")])
    end   <- as.numeric(airports[which(airports$name == flightgeo$airportEndName[index[1]] &
                                         airports$iata == flightgeo$airportEnd[index[1]])[1], c("X", "Y")])
    
    shortestDist[index] <- Imap::gdist(start[1], start[2], end[1], end[2], units = "m")
    
    # exactly the same distance from st_distance, using great circle. gdist is faster 
    #st <- st_sfc(st_point(c(start[1], start[2])), st_point(c(end[1], end[2])), crs = 4326)
    #st_distance(st, st, by_element = TRUE)
    #st_is_longlat(st)
    
    startLat[index] <- start[2]
    endLat[index] <- end[2]
  }
  
  return(cbind(routeCount, shortestDist, shortestDistFlight, shortestDur, startLat, endLat))
}


################### Summarize flightgeo into route data #######################

#' Format route data to be read into MonaRk shiny app
#'
#' All the flight data is too big to be read into the shiny app. Therefore, the app
#' only reads in data for routes the user selected. This function creates the folder structure
#' so each route has a folder containing a df.RDS and a geo.RDS that can be read into the app.
#'
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @param geomatrix A list of matrices containing 10-second data for lat, long, alt, velocity; "geo.RDS"
#' @param appDataDir The folder where all the route subfolders should be located
#' @return No objects returns. Folders are created locally and data is written to them.
#' @export
createRouteFolders <- function(flightgeo, geomatrix, appDataDir)
  lapply(unique(flightgeo$route), function(x){
    index <- which(flightgeo$route == x)
    
    geo_index <- geomatrix[index]
    flights_index <- flightgeo[index,]
    
    subfolder <- paste0(appDataDir, "/", gsub(" ", "_", x))
    dir.create(subfolder, showWarnings = FALSE)
    save(geo_index, file = file.path(subfolder,"geo.RDS"))
    save(flights_index, file = file.path(subfolder,"df.RDS"))
  })


#' Summarize route data
#'
#' Create a df where each row is a route and columns contain summary data.
#' Used to compare routes and select routes of interest.
#' This returned df is shown on the first tab of the MonaRk app.
#'
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @return A df summarizing each route's data
#' @export
createRouteLevelData <- function(flightgeo){
  flightgeo$g <- NULL
  flightgeo <- data.table(flightgeo)
  routeSummary <- flightgeo[,.( 
    routeCount = .N,
    country = country[1],
    continent = continent[1],
    startLat = startLat[1],
    endLat = endLat[1],
    
    # basic distance summaries 
    shortestDist = shortestDist[1], 
    shortestDistFlight = min(distance), 
    longestDist = max(distance), 
    meanDist = mean(distance, na.rm = T),
    medianDist = median(distance, na.rm = T),
    rangeDist = max(distance) - min(distance),
    varDist = var(distance),
    
    # basic duration summaries
    shortestDur = min(duration), 
    longestDur = max(duration), 
    meanDur = mean(duration, na.rm = T),
    medianDur = median(duration, na.rm = T),
    rangeDur = max(duration) - min(duration),
    varDur = var(duration),
    
    # distance increase summaries based on direct path
    meanKmIncrease = mean(kmincrease),
    maxKmIncrease = max(kmincrease),
    varKmIncrease = var(kmincrease),
    meanKmPercIncrease = mean(kmpercincrease),
    maxKmPercIncrease = max(kmpercincrease),
    varKmPercIncrease = var(kmpercincrease),
    
    # distance increase summaries based shortest flight
    meanKmIncreaseFlight = mean(kmincreaseFlight),
    maxKmIncreaseFlight = max(kmincreaseFlight),
    varKmIncreaseFlight = var(kmincreaseFlight),
    meanKmPercIncreaseFlight = mean(kmpercincreaseFlight),
    maxKmPercIncreaseFlight = max(kmpercincreaseFlight),
    varKmPercIncreaseFlight = var(kmpercincreaseFlight),
    
    # duration increase summaries
    meanMinIncrease = mean(minincrease),
    maxMinIncrease = max(minincrease),
    varMinIncrease = var(minincrease),
    meanMinPercIncrease = mean(minpercincrease),
    maxMinPercIncrease = max(minpercincrease),
    varMinPercIncrease = var(minpercincrease),
    
    # weather incidence
    thisWeather = sum(thisWeather, na.rm = T), 
    percThisWeather = round(mean(thisWeather, na.rm = T)*100), 
    stormEvent = sum(stormEvent, na.rm = T),
    percStormEvent = round(mean(stormEvent, na.rm = T)*100),
    regionWeather = sum(regionWeather, na.rm = T),
    percRegionWeather = round(mean(regionWeather, na.rm = T)*100),
    
    # total extra
    sum_kmincrease = sum(kmincrease),
    sum_kmincreaseFlight = sum(kmincreaseFlight),
    sum_minincrease = sum(minincrease),
    sum_hourincrease = sum(minincrease) / 60,
    
    # extra by weather
    sum_thisWeather_kmincreaseFlight = sum(kmincreaseFlight[which(thisWeather)]),
    sum_regionWeather_kmincreaseFlight = sum(kmincreaseFlight[which(regionWeather)]),
    sum_stormEvent_kmincreaseFlight = sum(kmincreaseFlight[which(stormEvent)]),
    sum_weatherDelay_kmincreaseFlight = sum(kmincreaseFlight[which(weatherDelay > 0)]),
    sum_nasDelay_kmincreaseFlight = sum(kmincreaseFlight[which(nasDelay > 0)]),
    sum_thisWeather_hourincrease = sum(minincrease[which(thisWeather)]) / 60,
    sum_regionWeather_hourincrease = sum(minincrease[which(regionWeather)]) / 60,
    sum_stormEvent_hourincrease = sum(minincrease[which(stormEvent)]) / 60,
    sum_weatherDelay_hourincrease = sum(minincrease[which(weatherDelay > 0)]) / 60,
    sum_nasDelay_hourincrease = sum(minincrease[which(nasDelay > 0)]) / 60,
    
    # weather stations
    meanRaddens = mean(radarDensity),
    meanBaldens = mean(balloonDensity),
    meanGrounddens = mean(groundDensity),
    shortestRaddens = radarDensity[which.min(distance)],
    shortestBaldens = balloonDensity[which.min(distance)],
    shortestGrounddens = groundDensity[which.min(distance)],
    
    # delays
    anyDelay = sum(c(weatherDelay, nasDelay, carrierDelay,
                     securityDelay, lateAircraftDelay), na.rm = T),
    weatherDelay = sum(weatherDelay, na.rm = T),
    nasDelay = sum(nasDelay, na.rm = T),
    percWeatherDelay = sum(weatherDelay > 0, na.rm = T),
    percNasDelay = sum(nasDelay > 0, na.rm = T),
    
    # airbus count
    airbusCount = sum(grepl("Airbus", manufacturer))
  ), 
  by=route]
  return(routeSummary)
}
