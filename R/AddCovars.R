############################################################################
############# ADD EXTRA COLUMNS TO STATES AFTER TRANSFORMATION #############
############################################################################
#' Add covariates to flight data
#' 
#' Given basic parsed flight geo data, add columns including "route",
#' "country", "continent", sigmet overlaps, weather station density,
#' U.S. storm event overlap and U.S. delay data for use in analyses and 
#' MonaRk shiny app. 
#' 
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @param sigmetgeo A df with rows as sigmets, and columns as time, location, etc
#' @return A df based on flightgeo, with extra columns for all covariates.
#' 
addCovars <- function(flightgeo, sigmetgeo){
  
  ######## route #######
  flightgeo$route <- paste(flightgeo$airportStart, flightgeo$airportEnd)
  print("Route added")
  
  ######## update country if 2 letter code #######
  flightgeo$country <- getCountries(flightgeo)
  print("Country added")

  ######## continent #######
  flightgeo$continent <- getContinents(flightgeo)
  print("Continent added")
  
  ####### weather #######
  print("Starting weather")
  
  flightgeo$shortWeather <- shortFlightWeather(flightgeo, sigmetgeo)
  print("Short weather added")
  flightgeo$thisWeather <- thisFlightWeather(flightgeo, sigmetgeo)
  print("This weather added")
  flightgeo$eitherWeather <- flightgeo$shortWeather | flightgeo$thisWeather
  flightgeo$regionWeatherID <- thisRegionWeather(flightgeo, sigmetgeo)
  flightgeo$regionWeather <- !(flightgeo$regionWeatherID == "0")
  print("Region weather added")
  
  # weather station density: count per km2
  stationDensities <- countStations(flightgeo)
  
  flightgeo$radarDensity <- stationDensities$radarDensity
  flightgeo$balloonDensity <- stationDensities$balloonDensity
  flightgeo$groundDensity <- stationDensities$groundDensity
  #flightgeo <- cbind(flightgeo, stationDensities)
  print("Station density done")
  
  ######################################
  # STORM
  flightgeo$stormEventID <- thisStormEvent(flightgeo)
  flightgeo$stormEvent <- !(flightgeo$stormEventID == "0")
  print("Storm events done")

  ######################################
  # DELAY
  delayCovars <- thisDelay(flightgeo)
  flightgeo$weatherDelay      <- delayCovars[,1]
  flightgeo$nasDelay          <- delayCovars[,2]
  flightgeo$carrierDelay      <- delayCovars[,3]
  flightgeo$securityDelay     <- delayCovars[,4]
  flightgeo$lateAircraftDelay <- delayCovars[,5]
  print("delay done")
  
  return(flightgeo)
}


############################################################################
###################### Continents for addCovars() ##########################
############################################################################
getCountries <- function(flightgeo){
  continents <- fread("inst/extdata/helpers/continents.csv")
  countries <- unlist(lapply(1:nrow(flightgeo) , function(x){
    if(nchar(flightgeo$country[x]) == 2) {
      country <- continents$Country_Name[which(continents$Two_Letter_Country_Code == flightgeo$country[x])][1]
    } else {
      country <- flightgeo$country[x]
    }
    return(country)
  }))
  return(countries)
}

getContinents <- function(flightgeo){
  continents <- fread("inst/extdata/helpers/continents.csv")
  conts <- unlist(lapply(flightgeo$country, function(x){
      return(continents$Continent_Name[which(grepl(x, continents$Country_Name))[1]])
  }))
  return(conts)
}

############################################################################
################## Weather functions for addCovars() #######################
############################################################################

#' Shorest flight in a route sigmet overlap
#'
#' Loop through the routes to find the shorest flight for each. For the time of 
#' each flight, loop through to find whether a sigmet overlapped the shortest flight's path.
#' Would be interesting if many longer routes have a sigmet intersecting the short path,
#' providing a probable cause for the deviation
#' 
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @param sigmetgeo A df with rows as sigmets, and columns as time, location, etc
#' @return A T/F vector to show if a sigmet overlapped the shorest flight in the route.
#' @export
shortFlightWeather <- function(flightgeo, sigmetgeo){
  
  routes <- unique(flightgeo$route)
  sigmetgeo <- st_zm(st_make_valid(sigmetgeo))

  shortestWeatherAffected <- rep(NA, nrow(flightgeo))
  # loop through list of routes of interest
  for(i in 1:length(routes)){
    print(i)
    
    index <- flightgeo$route == routes[i]
    route <- flightgeo[index,]
    
    # shortest route
    minRoute <- st_zm(route$g[which.min(route$distance)])
    
    intersects <- suppressMessages(st_intersects(minRoute, sigmetgeo))
    intersectSig <- NA
    intersectSig <- sigmetgeo[unique(unlist(intersects)),]
    
    # loop through routes to see if any geo intercepts are time intercepts
    if(nrow(intersectSig) > 0){
      shortestWeatherAffected[index] <- unlist(lapply(1:nrow(route), 
                                                      function(y) {
                                                        any(unlist(
                                                          lapply(1:nrow(intersectSig), 
                                                                 function(z){
                                                                   any(intersectSig$start[z]:intersectSig$end[z] %in% route$start[y]:route$end[y])  
                                                                 })))
                                                      }))
    } else {
      shortestWeatherAffected[index] <- rep(FALSE, nrow(route))
    }
    
  }
  return(shortestWeatherAffected)
}


#' Flight sigmet overlap
#'
#' Loop through the flights if any sigmet overlapped the flight in space and time
#' 
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @param sigmetgeo A df with rows as sigmets, and columns as time, location, etc
#' @return A T/F vector to show if sigmets overlapped each flight.
#' @export
thisFlightWeather <- function(flightgeo, sigmetgeo){
  
  sigmetgeo <- st_zm(st_make_valid(sigmetgeo))
  flightgeo$start <- as.numeric(flightgeo$start)
  flightgeo$end <- as.numeric(flightgeo$end)
  
  unlist(lapply(1:nrow(flightgeo), function(x){
    print(x)
    # remove ones that can't be time overlapped
    sigmetTime <- sigmetgeo[which(!(sigmetgeo$end < flightgeo$start[x] | sigmetgeo$start > flightgeo$end[x])),]

    ##### Geo intersects is actually faster
    intersects <- NA
    intersects <- suppressMessages(st_intersects(st_zm(flightgeo[x,]), sigmetTime))
    intersects <- sigmetTime[unique(unlist(intersects)),]
    
    thisWeatherAffected <- FALSE
    if(nrow(intersects) > 0){
      # loop through geo intersections to find time intersections
      thisWeatherAffected <- any(unlist(lapply(1:nrow(intersects), function(y){
        any(intersects$start[y]:intersects$end[y] %in% flightgeo$start[x]:flightgeo$end[x])
      })))
    }
    
    return(thisWeatherAffected)
  }))
}



#' Flight region sigmet overlap ID
#'
#' Loop through the routes and find the midpoint between the two airports, and transform
#' the flight data to the UTM zone of the midpoint. Draw a 1000km buffer around midpoint.
#' Then loop through each flight to see if there was a sigmet overlapping the buffer 
#' at the same time as the flight.
#' 
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @param sigmetgeo A df with rows as sigmets, and columns as time, location, etc
#' @return A numeric vector to show ids of sigmets within ~1000km of a flight.
#' @export
thisRegionWeather <- function(flightgeo, sigmetgeo){
  
  routes <- unique(flightgeo$route)
  regionWeatherAffected <- rep(NA, nrow(flightgeo))
  
  for(i in 1:length(routes)){
    print(paste("route:", i))
    index <- flightgeo$route == routes[i]
    route <- flightgeo[index,]
    
    coords <- as.matrix(route$g)[[1]]
    airport1 <- c(coords[1,1], coords[1,2])
    airport2 <- c(coords[nrow(coords),1], coords[nrow(coords),2])
    
    midpoint <- c(abs(airport1[1] - airport2[1])/2 + min(airport1[1], airport2[1]),
                  abs(airport1[2] - airport2[2])/2 + min(airport1[2], airport2[2]))
    
    crs <- lonlat2UTM(midpoint)
    route$g <- st_transform(route$g, crs = crs)
    
    routeOverlaps <- unlist(lapply(1:nrow(route), function(x){
      print(x)
      
      # remove ones that can't be time overlapped
      sigmetTime <- sigmetgeo[which(!(as.numeric(sigmetgeo$end) < as.numeric(route$start[x]) | as.numeric(sigmetgeo$start) > as.numeric(route$end[x]))),]
      
      timeOverlap <- 0
      
      if(nrow(sigmetTime) > 0){
        
        ##### Geo intersects is faster than finding time overlap
        intersectBuf <- NA
        # buffer by 1000km
        buffer <- NA
        buffer <- try(st_buffer(st_zm(route$g[x]), dist = 1000*1000, endCapStyle = 'ROUND'),
                      silent = TRUE)
        if(!is.na(buffer) && class(buffer) != "try-error"){
          buffer <- st_transform(buffer, crs = 4326)
          
          if (any(unlist(lapply(sigmetTime$g, function(sig) {
            nrow(as.matrix(sig)) == 3
          })))) {
            sigmetTime <- st_zm(st_make_valid(sigmetTime))
          }
          
          intersectBuf <- suppressMessages(st_intersects(buffer, sigmetTime))
          intersectBuf <- sigmetTime[unique(unlist(intersectBuf)),]
        }
        
        if(!is.na(intersectBuf) && nrow(intersectBuf) > 0){
          
          # loop through geo intersections to find time intersections
          timeOverlap <- unlist(lapply(1:nrow(intersectBuf), function(y){
            if(any(intersectBuf$start[y]:intersectBuf$end[y] %in% route$start[x]:route$end[x])){
              return(intersectBuf$id[y])
            }
          }))
          
          timeOverlap <- paste(timeOverlap, collapse = " ")
        }
      }
      
      timeOverlap <- as.character(timeOverlap)
      return(timeOverlap)
    }))
    regionWeatherAffected[index] <- routeOverlaps
  }
  return(regionWeatherAffected)
}



############################################################################
############## Weather station functions for addCovars() ###################
############################################################################

#' Count weather stations per KM^2
#'
#' Read in 3 types of weather station locations. Then for each flight, loop through 
#' to find the midpoint between the two airports, and transform the flight data to the 
#' UTM zone of the midpoint. Draw a 230km buffer around the entire flight and calculate
#' the area of this buffer region in km^2. Then count the number of each station type 
#' within the buffer to calculate the station density.
#' 
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @return A df of weather station densities, with rows corresponding to flights in flightgeo
#' @export
countStations <- function(flightgeo){

  # three types of stations
  radar <- readRadarLocations()
  balloon <- readBalloonLocations()
  ground <- readGroundLocations()

  # area of influence for a stations data
  radius <- 230000 #230km - sourced, accurate for radar, not for balloon and ground

  # initialize
  #radarDensity <- rep(NA, nrow(flightgeo))
  #balloonDensity <- rep(NA, nrow(flightgeo))
  #groundDensity <- rep(NA, nrow(flightgeo))

  densities <- lapply(1:nrow(flightgeo), function(i) {
    # find midpoint of flight so can transform into correct UTM for meters
    print(i)
    coords <- as.matrix(flightgeo$g[i][[1]])
    airport1 <- c(coords[1,1], coords[1,2])
    airport2 <- c(coords[nrow(coords),1], coords[nrow(coords),2])
    midpoint <- c(abs(airport1[1] - airport2[1])/2 + min(airport1[1], airport2[1]),
                  abs(airport1[2] - airport2[2])/2 + min(airport1[2], airport2[2]))
    # find correct UTM and transform
    crs <- lonlat2UTM(midpoint)
    flightgeo_trans <- st_transform(flightgeo[i,], crs = crs)

    # make buffer area, transform back to lat lon
    flightgeo_buf <- try(st_buffer(st_zm(flightgeo_trans), dist = radius, endCapStyle = "ROUND"), silent=TRUE)
    if(class(flightgeo_buf)[1] != "try-error"){
      flightgeo_buf <- st_transform(flightgeo_buf, crs = 4326)
      #st_area returns meters, calc by km2
      area <- round(as.numeric(st_area(flightgeo_buf) / 1000000))
      
      # calculate densities
      radarDensity <- suppressMessages(lengths(st_intersects(flightgeo_buf, radar))/area)
      balloonDensity <- suppressMessages(lengths(st_intersects(flightgeo_buf, balloon))/area)
      groundDensity <- suppressMessages(lengths(st_intersects(flightgeo_buf, ground))/area)

      return(c(radarDensity, balloonDensity, groundDensity))
    } else {
      return(c(NA,NA,NA))
    }
    
  })

  densities <- as.data.frame(do.call(rbind, densities))
  names(densities) <- c("radarDensity", "balloonDensity", "groundDensity")

  # station count per km2
  return(densities)
}



readRadarLocations <- function(radarFile = "inst/extdata/helpers/radarLocations.txt"){

  radar <- readtext::readtext(radarFile, dvsep = "\\n")
  radar <- unlist(strsplit(radar$text, fixed=TRUE, split = "\\n"))
  radar <- gsub('\"', "", radar)
  
  radar <- lapply(radar, function(x){
    unlist(strsplit(x, split = ","))
  })
  
  radar <- as.data.frame(do.call(rbind, radar))
  names(radar) <- c("id", "city", "lat", "lon")
  
  radar$lon <- as.numeric(as.character(radar$lon))
  radar$lat <- as.numeric(as.character(radar$lat))
  
  radar <- st_as_sf(radar, coords = c("lon", "lat"), crs = 4326)
  return(radar)
}

readGroundLocations <- function(file = "inst/extdata/helpers/isd-history.csv"){
  isd <- data.table::fread(file)
  
  isd <- isd[substr(isd$END, 1, 4) == "2020",]
  isd <- isd[!is.na(isd$LAT) & !is.na(isd$LON) &
               isd$LAT != 0 & isd$LON != 0, ]
  
  isd <- st_as_sf(isd, coords = c("LON", "LAT"), crs = 4326)
  return(isd)
}


readBalloonLocations <- function(file = "inst/extdata/helpers/balloonLocations.txt"){
  bal <- readr::read_fwf(file, 
                         readr::fwf_widths(c(11, 9, 10, 7, 35, 5, 5, 6)))
  
  names(bal) <- c("id1", "lat", "lon", "elev_m", "name", "start_y", "end_y", "id2")
  
  bal <- bal[bal$end_y == "2020",] 
  
  bal <- st_as_sf(bal, coords = c("lon", "lat"), crs = 4326)
  
  return(bal)    
}


############################################################################
############## Storm events overlap for addCovars() ########################
############################################################################

#' Storm Events Overlap
#' 
#' Read in and polygonize storm data. Then loop through US flights to find
#' if any storm events overlapped the flight in time and space.
#' 
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @return A T/F vector to show if a storm event overlapped each flight.
#' 
thisStormEvent <- function(flightgeo){
  #storms <- fread("data/preprocessing/cleanStorms.csv")
  storms <- get(load("inst/extdata/cleaned/stormsGeo.RDS"))
  #storms <- polygonizeStorms(storms, distance = 10000)
  
  stormOverlap <- unlist(lapply(1:nrow(flightgeo), function(x){
    
    ##### stormEvents should be NA if not in the US or after latest storm
    if (flightgeo$country[x] != "USA") {
      timeOverlap <- NA
    } else if (flightgeo$start[x] > max(storms$END_UNIX_UTC)){
      timeOverlap <- NA
    }
     else {
      print(x)
      # remove ones that can't be time overlapped
      # remove storms that ended before flight started or started after flight ended
      stormsTime <- storms[which(!(storms$END_UNIX_UTC < as.numeric(flightgeo$start[x]) | storms$BEGIN_UNIX_UTC > as.numeric(flightgeo$end[x]))),]
      
      ##### Geo intersects is actually faster
      intersects <- NA
      intersects <- suppressMessages(st_intersects(st_zm(flightgeo[x,]), stormsTime))
      intersects <- stormsTime[unique(unlist(intersects)),]
      
      timeOverlap <- 0
      if(nrow(intersects) > 0){
        # loop through geo intersections to find time intersections
        # timeOverlap <- any(unlist(lapply(1:nrow(intersects), function(y){
        #   any(intersects$BEGIN_UNIX_UTC[y]:intersects$END_UNIX_UTC[y] %in% flightgeo$start[x]:flightgeo$end[x])
        # })))
        timeOverlap <- unlist(lapply(1:nrow(intersects), function(y){
          if(any(intersects$BEGIN_UNIX_UTC[y]:intersects$END_UNIX_UTC[y] %in% flightgeo$start[x]:flightgeo$end[x])){
            return(intersects$id[y])
          }
        }))
      }
      
      timeOverlap <- paste(timeOverlap, collapse = " ")
    }

    return(timeOverlap)
  }))
  
  return(stormOverlap)
}



############################################################################
################# Delay information for addCovars() ########################
############################################################################
#' Flight delay
#' 
#' Read in delay data. Then loop through US flights to find matching records 
#' in the delay data by matching origin, destination, callsign, plane, and then time.
#' 
#' @param flightgeo A sf dataframe containing flight data and geodata
#' @return A df containing columns with minute delays for weather, NAS, carrier, security, and late aircraft. 
#' 
thisDelay <- function(flightgeo){
  delay <- get(load("inst/extdata/cleaned/delayGeo.RDS"))
  aircraft <- fread("inst/extdata/helpers/aircraftDatabase.csv")

  # if just working with one date, subset to that date
  if(length(unique(flightgeo$date)) == 1){
    delay <- delay[which(delay$FlightDate == flightgeo$date[1] | delay$FlightDate == (flightgeo$date[1] - 1)),]
  }

  # find each flightgeo in delay database by matching:
  # origin, destination, callsign, plane, and then time
  match <- unlist(lapply(1:nrow(flightgeo), function(x){

    # only have data for USA flights
    if(flightgeo$country[x] != "USA"){
      return(NA)
    } else {
      print(x)
      delaysub <- delay[which(delay$Origin == flightgeo$airportStart[x] & delay$Dest == flightgeo$airportEnd[x]),]
      
      if(nrow(delaysub) > 0){
        callsign <- sub("\\D+", "", flightgeo$callsign[x])
        delaysub <- delaysub[delaysub$Flight_Number_Operating_Airline == callsign, ]
      }
      
      if(nrow(delaysub) > 0){
        plane <- aircraft$registration[aircraft$icao24 == flightgeo$plane[x]]
        delaysub <- delaysub[delaysub$Tail_Number == plane]
      }
      
      if(nrow(delaysub) > 0){
        times <- as.numeric(as.POSIXct(
          fourDigitTimeToUTC(delaysub$WheelsOff, delaysub$FlightDate),
          format="%Y-%m-%d"))
        # my shortcut of assuming all flight in CST - if flight starts
        # within 3 hours, they are probably the same flight
        delaysub <- delaysub[which(abs(times - as.numeric(flightgeo$start[x])) < 10800)]
      }
      
      if(nrow(delaysub) == 1){
        return(which(delay$id == delaysub$id))
      } else {
        return(NA)
      }
      
    }
  }))
  
  if(any(!(is.na(match)))){ 
    return(cbind(delay$WeatherDelay[match], 
                 delay$NASDelay[match],
                 delay$CarrierDelay[match], 
                 delay$SecurityDelay[match],
                 delay$LateAircraftDelay[match]))
  } else { # above returns weird format if all na
    return(data.frame(matrix(NA, nrow = nrow(flightgeo), ncol = 5)))
  }
 
}


fourDigitTimeToUTC <- function(timeCol, dateCol){
  
  # add 2 zero to 2 digit times (10 to 1000)
  timeDateCol <- unlist(lapply(timeCol, function(x) {
    ifelse(nchar(x) == 2, paste0(x, 0,0), x)
  }))
  
  # add zero to 3 digit times (930 to 0930)
  timeDateCol <- unlist(lapply(timeDateCol, function(x) {
    ifelse(nchar(x) == 3, paste0(0,x), x)
  }))
  
  timeDateCol <- paste0(substr(timeDateCol, 1, 2), ":", substr(timeDateCol, 3, 4))
  
  # says GMT but still EDT
  timeDateCol <- strptime(paste(dateCol, timeDateCol), 
                          format = "%Y-%m-%d %H:%M",
                          tz = "America/Chicago")
  
  #### CONVERT TO UTC 
  timeColUTC <- with_tz(timeDateCol, tz = "GMT")
  
  return(timeColUTC)
}