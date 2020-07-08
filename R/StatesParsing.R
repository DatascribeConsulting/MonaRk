#' Download, unzip, and read raw OpenSky States Files into R
#'
#' If states files are not downloaded, use this to download, unzip, and fread into R
#' If states files are downloaded, use this to unzip, and fread into R by download = F
#'
#' @param date One date at a time, in the format "YYYY-MM-DD"
#' @param range The hours in the day to be included. Default is all (0:23), but if download fails halfway though, can pick it up (5:23)
#' @param download T/F Need to download data from opensky and unzip it first? False if you have already downloaded and want to just read into R
#' @param fread T/F Need to read it into R? False if you want to download only
#' @param dir Where to download files or where to look for already downloaded files
#' @return "states" which is a dataframe containing a row for each plane at 10 second intervals. This will be fed to additional functions to get data into a usable format.
#' @export

readOpenSkyStates <- function (date, range = 0:23, 
                               download = TRUE, fread = TRUE,
                               dir = NULL) {
  url <- "https://opensky-network.org/datasets/states/"
  
  hours <- c("00","01","02","03","04","05","06",
             "07","08","09","10","11","12","13",
             "14","15","16","17","18","19","20",
             "21","22","23")[range + 1]
  
  fn <- paste0(url, date, "/", hours, "/states_", date, "-", hours, ".csv.tar")
  
  if(is.null(dir)){
    dir <- getwd()
  }
  
  return( rbindlist( 
    lapply(fn,  function(x){ 
      file <- file.path(dir, paste0(basename(tools::file_path_sans_ext(x)), ".gz"))
      if (download){
        download.file(x, destfile=file.path(dir,"tmp.tar.gz"))
        untar(file.path(dir,"tmp.tar.gz"), exdir = dir)
      }
      if (fread){
        states <- fread(file)
        # remove rows that have missing location data
        states <- na.omit(states, cols = c("lon", "lat", "geoaltitude")) 
        return(states)
      }
    }) 
  ))
}



#' Read RDSs from folders
#' 
#' When storing multiple geo.RDS and df.RDS in two folders, pass those folders
#' to this function to create one object containing all the data.
#' If dfFolder = NULL, read geo matrices only from folder and return 1 object 
#' 
#' @param geoFolder Location of geo.RDS
#' @param dfFolder Location of df.RDS
#' @return A sf dataframe, flightgeo, that contains flight data and geodata.
#' 
readFlightgeoFromFolder <- function(geoFolder = NULL, dfFolder = NULL, drop){
  
  if(is.null(dfFolder) && !is.null(geoFolder)){
    
    geoFiles <- list.files(geoFolder)
    
    geos <- lapply(1:length(geoFiles), function(x){
      return(get(load(file.path(geoFolder, geoFiles[x]))))
    })
    geos <- unlist(geos, recursive = F)
    return(geos)
    
  } else if (!is.null(dfFolder) && is.null(geoFolder)){
    
    dfFiles <- list.files(dfFolder)
    
    dfs <- lapply(1:length(dfFiles), function(x){
      return(get(load(file.path(dfFolder, dfFiles[x]))))
    })
    return(do.call(rbind, dfs))
  
  }  else {
    
    geoFiles <- list.files(geoFolder)
    dfFiles <- list.files(dfFolder)
    
    dfs <- lapply(1:length(geoFiles), function(x){
      geoReadRDS <- get(load(file.path(geoFolder, geoFiles[x])))
      flightsReadRDS <- get(load(file.path(dfFolder, dfFiles[x])))
      return(createFlightgeo(geoReadRDS, flightsReadRDS, drop = drop))
    })
    
    return(do.call(rbind, dfs))
  }
}

###########################################################################
#################### Main Transformation Function #########################
###########################################################################
#' Parse flight state data
#' 
#' Loop through callsigns and subset to all data with that callsign. Separate into
#' different flights if more than 30 minutes between records. For each of these flights,
#' only keep data it is a complete flight: starting and ending below 4500m elevation,
#' more than 10 minutes long, is not a Cessna or Beech, doesn't have a jump/data error of
#' more than two decimal degrees, is within 500m elevation and .1 decimal degrees lat and lon 
#' to nearest airport at start and end of flight.
#' 
#' @param states A df containing flight data, each row 10 seconds for a single aircraft
#' @param callsigns Either pass selected callsigns if only interested in a few, or leave as is to select all callsigns 
#' @return a list of "geomatrix" (list of matrices) and "flights" (a df where a flight is a row)
#' 
transformStates <- function(states, 
                            callsigns = unique(states[ , callsign])) {
  
  # read in airports & aircraft data data
  # these are used as covariates but also used to determine if flightpath is real & complete
  airports <- fread("inst/extdata/helpers/airports.csv")
  # for airports without iata, replace with icao
  airports[iata == "N/A", iata:=icao]
  airports <- st_as_sf(airports, coords = c("X", "Y"),
                       crs = 4326, agr = "constant")
  
  aircraft <- fread("inst/extdata/helpers/aircraftDatabase.csv")
  
  # initialize before loop
  starttime    <- c()
  endtime      <- c()
  callsign     <- c()
  icao         <- c()
  airportStart <- c()
  airportEnd   <- c()
  airportStartName <- c()
  airportEndName <- c()
  aveSpeed     <- c()
  country      <- c()
  id           <- c()
  alert        <- c()
  
  routeLines <- list()
  f <- 0  # initialize flight count ticker
  
  # loop through callsign
  for (i in 1:length(callsigns)) {
 
    # every 1000 callsigns, save to routeLines and flights in case error
    # causes function to quit in the middle
    if(i %% 1000 == 0){
      print(i)

      keepIndices <- sapply(routeLines, function(x) !is.null(x) && !is.na(x))
      
      if(length(keepIndices) > 0 && sum(keepIndices) > 0) {
      
        craftMatch <- unlist(lapply(icao[keepIndices], function(x){
          which(aircraft$icao24 == x)[1]
        }))

        geomatrix <<- routeLines[keepIndices]
        flights <<- data.table("start"       = starttime[keepIndices],
                              "end"          = endtime[keepIndices],
                              "date"         = lubridate::date(as.POSIXct(as.numeric(starttime[keepIndices]), origin = "1970-01-01")),
                              "duration"     = (as.numeric(endtime[keepIndices])-as.numeric(starttime[keepIndices]))/60,
                              "callsign"     = callsign[keepIndices], 
                              "plane"        = icao[keepIndices],
                              "airline"      = aircraft$owner[craftMatch],
                              "manufacturer" = aircraft$manufacturername[craftMatch],
                              "model"        = aircraft$model[craftMatch],
                              "airportStart" = airportStart[keepIndices],
                              "airportEnd"   = airportEnd[keepIndices],
                              "airportStartName" = airportStartName[keepIndices], 
                              "airportEndName" = airportEndName[keepIndices], 
                              "country"      = country[keepIndices],
                              "aveSpeed"     = aveSpeed[keepIndices],
                              "id"           = id[keepIndices],
                              "alert"        = alert[keepIndices])
      }
    }
    
    flightCallsign <- states[callsign == callsigns[i]]
    
    # we don't care about cessnas - leave out low flights
    if (any(!is.na(flightCallsign$geoaltitude)) && mean(unique(flightCallsign$geoaltitude),  na.rm = TRUE) > 1500) {
      
      # Find records with a 30 min gap
      # This will define a new flight, later we'll check endpoints to only 
      # include flights that started & ended at an airport
      # Don't want to include flights with gap in coverage over ocean
      flightCut <- c(0, which(diff(flightCallsign$time) > 1800), nrow(flightCallsign))
      flightCut <- flightCut[which(!is.na(flightCut))]
    
      # loop through flights for current callsign
      for (j in 1:(length(flightCut)-1)) {

        flight <- flightCallsign[(flightCut[j]+1):flightCut[j+1]]
 
        # ~4500m is the highest airport in the world. If higher than this at
        # first or last entry we know it's an incomplete flight
        if(nrow(flight) > 1 && 
           flight$geoaltitude[1] < 4500 && 
           flight$geoaltitude[nrow(flight)] < 4500){
          
          # only look at flights longer than 10 min
          if ((flight$time[nrow(flight)] - flight$time[1]) > 600){
            
            # check for cessnas more directly
            aircraftMaker <- aircraft[icao24 == flight$icao24[1], manufacturername]
    
            if(length(aircraftMaker) == 0 || aircraftMaker != "Cessna" && aircraftMaker != "Beech"){
              
              ## remove rows that are duplicated at beginning and end to have correct time
              firstDup <- duplicated(flight[,c(3:7, 13:15)], fromLast = T)
              lastDup <- duplicated(flight[,c(3:7, 13:15)])
              flight <- flight[min(which(!(firstDup))) : max(which(!(lastDup))), ]
        
              #making XYZ points
              locMat <- matrix(c(flight[,lon], flight[,lat],
                       flight[,geoaltitude], flight[,time],
                       flight[,velocity]),
                     nrow = nrow(flight))
              locMat <- locMat[rowSums(is.na(locMat)) == 0,]
              
              routeLine <- NA
              ## leave out flights that have a jump of 2 decimal degrees
              if(is.matrix(locMat) && all(diff(locMat[,c(1,2)]) < 2)){
                # don't need time or velocity for airport calculations
                routeLine <- st_linestring(locMat[,c(1:3)])          
              }
    
              if(!is.na(routeLine) && length(routeLine) > 0 ){
                suppressMessages( #"although coordinates are longitude/latitude, st_nearest_feature assumes that they are planar"
                  nearStart <- st_nearest_feature(st_point(routeLine[1,1:2]), airports) 
                )
                suppressMessages( 
                  nearEnd <- st_nearest_feature(st_point(routeLine[nrow(routeLine),1:2]), airports) 
                )

                if(airports$iata[nearStart] != airports$iata[nearEnd]){
                  # test to see if begining and end have close enough elevation to airport to have entire flight
                  fullStart <- NA
                  fullEnd <- NA
                  # hard to know what cutoffs should be... 
                  # .1 decimal degrees = 11.1km for lat everywhere and for lon at equator
                  # cases of complete flights being 370m above airport and .06 dd away (common to be over .01 away)
                  fullStart <- abs(flight$geoaltitude[1] - airports$alt[nearStart]) < 500  &&
                               abs(flight$lat[1] - airports$geometry[nearStart][[1]][2]) < .1  &&
                               abs(flight$lon[1] - airports$geometry[nearStart][[1]][1]) < .1
                  fullEnd   <- abs(flight$geoaltitude[nrow(flight)] - airports$alt[nearEnd]) < 500  &&
                               abs(flight$lat[nrow(flight)] - airports$geometry[nearEnd][[1]][2]) < .1  &&
                               abs(flight$lon[nrow(flight)] - airports$geometry[nearEnd][[1]][1]) < .1
                            
                  # if it is a full flight, save its data
                  if(!is.na(fullStart) && !is.na(fullEnd) 
                     && fullStart && fullEnd) {
                  
                    f <- f + 1
         
                    routeLines[[f]] <- locMat
                    starttime[f]    <- as.character(flight$time[1])
                    endtime[f]      <- as.character(flight$time[nrow(flight)])
                    callsign[f]     <- as.character(flight$callsign[1])
                    icao[f]         <- as.character(flight$icao24[1])
                    airportStart[f] <- as.character(airports$iata[nearStart])
                    airportEnd[f]   <- as.character(airports$iata[nearEnd])
                    airportStartName[f] <- as.character(airports$name[nearStart])
                    airportEndName[f]   <- as.character(airports$name[nearEnd])
                    country[f]      <- as.character(airports$country[nearStart])
                    aveSpeed[f]     <- mean(flight$velocity)
                    id[f]           <- i
                    alert[f]        <- as.character(sum(flight$alert) / nrow(flight))
                  } 
                }
              }
            }
          }
        }
      }
    }
  }

  keepIndices <- sapply(routeLines, function(x) !is.null(x) && !is.na(x))
  
  if(length(keepIndices) > 0 && sum(keepIndices) > 0) {
   
    craftMatch <- unlist(lapply(icao[keepIndices], function(x){
      which(aircraft$icao24 == x)[1]
    }))

    flights <- data.table("start"        = starttime[keepIndices],
                          "end"          = endtime[keepIndices],
                          "date"         = lubridate::date(as.POSIXct(as.numeric(starttime[keepIndices]), origin = "1970-01-01")),
                          # duration in min
                          "duration"     = (as.numeric(endtime[keepIndices]) - as.numeric(starttime[keepIndices])) / 60,  
                          "callsign"     = callsign[keepIndices], 
                          "plane"        = icao[keepIndices],
                          "airline"      = aircraft$owner[craftMatch],
                          "manufacturer" = aircraft$manufacturername[craftMatch],
                          "model"        = aircraft$model[craftMatch],
                          "operator"     = aircraft$operatorcallsign[craftMatch],
                          "airportStart" = airportStart[keepIndices],
                          "airportEnd"   = airportEnd[keepIndices],
                          "airportStartName" = airportStartName[keepIndices], 
                          "airportEndName" = airportEndName[keepIndices], 
                          "country"      = country[keepIndices],
                          "aveSpeed"     = aveSpeed[keepIndices],
                          "id"           = id[keepIndices],
                          "alert"        = alert[keepIndices]
                          )
    
    geomatrix <- routeLines[keepIndices]
    
    return(list(geomatrix, flights))
  }
}


############################# Map Geo Object ###############################
#' Map flight
#' 
#' Use leaflet to show flights on top of a basemap. When a flight is clicked
#' additional information is shown in a tooltip.
#' 
#' @param flightgeo A dataframe of all the flights information within routes of interest including geo data.
#' @return an interactive leaflet map
#' 
mapFlightgeo <- function(flightgeo) {
  states_popup <- paste("Callsign:", flightgeo$callsign,
                        "</br>", 
                        "Origin:", flightgeo$airportStart,
                        "</br>", 
                        "Destination:", flightgeo$airportEnd,
                        "</br>", 
                        "Start Time:", as.POSIXlt(as.numeric(as.character(flightgeo$start)), 
                                                  tz = "UTC", origin = "1970-01-01"),
                        "</br>", 
                        "End Time:", as.POSIXlt(as.numeric(as.character(flightgeo$end)), 
                                                tz = "UTC", origin = "1970-01-01"),
                        "</br>", 
                        "Line ID:", flightgeo$id)
  
  leaflet() %>% 
    addProviderTiles("CartoDB.Positron", group = "Carto") %>%
    addPolylines(data=st_zm(flightgeo), 
                 color = "#77aaee", opacity = .5, weight = 1,
                 popup = states_popup,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))
}





############################# Add Monday Data ###############################
#' Add multiple Mondays worth of data
#' 
#' Run this function if it has been several weeks since data has been updated.
#' It will update delay, storm, sigmet, and flight data.
#' 
#' @param statesDataDir The location where the large flight state data is downloaded and stored
#' @param geoSaveDir The location of the geo.RDS for each date
#' @param dfSaveDir The location of df.RDS for each date
#' @param appDir Where the MonaRk package files are stored (including "MonaRk")
#' @param routeCountCutoff Min number of routes to be included for app
#' @return No object returned, only data written
#'

addMondays <- function(appDir, statesDataDir, routeCountCutoff = 50){

  setwd(appDir)
  
  if(format(as.Date(lubridate::now("GMT")),'%w') == 1){
    stop("It's Monday in UTC time, so new Monday data is being created now. Please wait until tomorrow to update data.")
  }
  
  # what was most recent Monday?
  prevMonday <- function(x) {
                  for(i in 1:7){
                    if(format(as.Date(Sys.Date()-i),'%w') == 1){
                        d = Sys.Date() - i;
                      }
                   }
                return(d)
              }
  
  # is data up to date?
  recentMonday <- prevMonday(Sys.Date())

  ##### UPDATE STORMS ######
  # check if storms need to be downloaded
  threeMonthsAgo <- as.Date(recentMonday) %m-% months(3)
  
  # old storms file
  stormsGeo <- get(load("inst/extdata/cleaned/stormsGeo.RDS"))
  recentStorm <- max(format(as.Date(stormsGeo$END_YMDHMS_UTC), "%Y-%m"))
  
  if(recentStorm < format(threeMonthsAgo, "%Y-%m")){
    stormFolder <- "inst/extdata/raw/rawStorms"
    # does user need to download?
    files <- list.files(stormFolder, full.names = T)
    lastMonthDownload <- max(unlist(lapply(files, function(x){
      fread(x, select = c("BEGIN_YEARMONTH"))
    })))
    lastMonthDownload <- format(as.Date(paste0(substr(lastMonthDownload, 1,4), "-", substr(lastMonthDownload, 5,6), "-28")), "%Y-%m")
    if(lastMonthDownload < format(threeMonthsAgo, "%Y-%m")){
      stop(paste("Please download most recent storm files into", stormFolder))
    } else {
      save(stormsGeo, file = "inst/extdata/cleaned/stormsGeo_old.RDS")
      
      storms <- cleanStorms(stormFolder)
      stormsGeo <- polygonizeStorms(storms)
      save(stormsGeo, file = "inst/extdata/cleaned/stormsGeo.RDS")
    }
  }
  
  
  ##### UPDATE DELAY ######
  # check if current delay data is updated
  delay <- get(load("inst/extdata/cleaned/delayGeo.RDS"))
  recentDelay <- delay$FlightDate
  # if most recent month in the delay data 3 months ago, than delay data is updated
  
  if(format(max(as.Date(recentDelay)), "%Y-%m") < format(threeMonthsAgo, "%Y-%m")) {
    delayFolder <- "inst/extdata/raw/rawDelay"  
    
    # check if delay need to be downloaded  
    delayDownloadedMonths <- unlist(lapply(list.files(delayFolder), function(x){
      dateparts <- strsplit(strsplit(x, split = ")|\\.")[[1]][2], split="_")[[1]]
      return(paste0(dateparts[2],"-", dateparts[3], "-28"))
    }))
    allDataMonths <- format(seq(as.Date(min(delay$FlightDate)), as.Date(threeMonthsAgo), by = "month"), "%Y-%m")
    needsDownload <- allDataMonths[unlist(lapply(allDataMonths, function(x){
      !(x %in% format(as.Date(delayDownloadedMonths), "%Y-%m"))
    }))]
    # redownload most recent month also
    needsDownload <- unique(c(needsDownload, max(format(as.Date(delay$FlightDate), "%Y-%m"))))
    
    if(length(needsDownload) > 1){
      stop(paste("Please download most recent raw delay files from the date", 
                 needsDownload,
                 "into",
                 delayFolder,
                 "."))
    } else {
      save(delay, file = "inst/extdata/cleaned/delayGeo_old.RDS")
      delay <- cleanDelay(delayFolder)
      save(delay, file = "inst/extdata/cleaned/delayGeo.RDS")
    }
  }
  
  ##################################################
  ##### update sigmet date with multiple dates
  # old sigmet file
  sigmetgeo <- get(load("appData/allsigs.RDS"))
  
  # where new files are
  sigmetDataDir <- "inst/extdata/raw/rawSigmetScraping"

  dateseq <- as.Date((max(sigmetgeo$date) + 1) : recentMonday, origin = "1970-01-01")
  if(length(dateseq) >= 7){
    
    ##### Save allsigs_old as a backup
    save(sigmetgeo, file = "inst/extdata/cleaned/allsigs_old.RDS")
    
    datesUpdateSig <- dateseq[seq(7, length(dateseq), by = 7)]  
  
    # DO THEY NEED TO DOWNLOAD
    needDownload <- unlist(lapply(datesUpdateSig, function(x){
      !(any(grepl(x, list.files(sigmetDataDir))))
    }))
    
    if(any(needDownload)){
      stop(paste("Please download raw Sigmet files from the date", 
                 datesUpdateSig[which(needDownload)],
                 "into",
                 sigmetDataDir,
                 "."))
    }
    
    for(i in 1:length(datesUpdateSig)){
      updateFolder <- which(grepl(datesUpdateSig[i], list.files(sigmetDataDir)))
      rawSigmet <- readSigmetFiles(list.files(sigmetDataDir, full.names = TRUE)[updateFolder])
      sigmetgeoDay <- try(parseSigmet(rawSigmet, datesUpdateSig[i]))
      sigmetgeoDay$id <- seq(nrow(sigmetgeo) + 1, nrow(sigmetgeo) + nrow(sigmetgeoDay))
      sigmetgeo <- rbind(sigmetgeo, sigmetgeoDay)
    }
    
    ##### Add new sigmets to end, add id, and rewrite shapefile!
    save(sigmetgeo, file = "appData/allsigs.RDS")
  }
  
  
  ##### STATES DATA #####
  # look where parsed States are, find most recent
  files <- strsplit(list.files("inst/extdata/cleaned/df"), split = "_|\\.")
  stateDates <- as.Date(unlist(lapply(files, function(x){
    x[2]
  })))
  maxdateDf <- max(stateDates)

  if(maxdateDf != recentMonday){
    
    # find dates that need updating
    dateseq <- as.Date((maxdateDf + 1) : recentMonday, origin = "1970-01-01")
    
    if(length(dateseq) >= 7){
      datesUpdate <- dateseq[seq(7, length(dateseq), by = 7)]
    } else {
      datesUpdate <- NULL
    }
    
    # loop through datesUpdate
    for(i in 1:length(datesUpdate)){
      
      # are states already downloaded?
      if(length(which(grepl("states_", list.files(statesDataDir)) &
                grepl(datesUpdate[i], list.files(statesDataDir)))) == 24){
        downloadStates <- FALSE
      } else {
        downloadStates <- TRUE
      }
      
      print(paste("Parsing", datesUpdate[i]))
      states <- readOpenSkyStates(datesUpdate[i], download = downloadStates, 
                                  fread = T, dir = statesDataDir)
      
      fullstates <- transformStates(states)
      geomatrix <- fullstates[[1]]  # list of matrices with 5 cols
      flights <- fullstates[[2]]
      flightgeo <- createFlightgeo(geomatrix, flights, drop = "velocity")
      
      flightgeo_covars <- addCovars(flightgeo, sigmetgeo)
      flightgeo_covars$g <- NULL
      
      ## Save new df and geo to existing folder
      save(flightgeo_covars, file = file.path("inst/extdata/cleaned/df", paste0("df_", datesUpdate[i], ".RDS")))
      save(geomatrix, file = file.path("inst/extdata/cleaned/geo", paste0("geo_", datesUpdate[i],".RDS"))) 
    }
    
    ## RE ADD COVARS for delay and storm data that needs updating
    # subset between recentDelay and 3 months ago, send to addCovars again 
    updateCovarDate <- which(stateDates > max(recentDelay) &
                               stateDates < datesUpdate[1])
    for (i in updateCovarDate){
      updateDf <- get(load( file.path("inst/extdata/cleaned/df", paste0("df_", stateDates[i], ".RDS"))))
      updateGeo <- get(load( file.path("inst/extdata/cleaned/geo", paste0("geo_", stateDates[i], ".RDS"))))
      updateFlightGeo <- createFlightgeo(updateGeo, updateDf, drop = "velocity")
      
      updateFlightGeo$stormEventID <- thisStormEvent(updateFlightGeo)
      updateFlightGeo$stormEvent <- !(updateFlightGeo$stormEventID == "0")
      
      delayCovars <- thisDelay(updateFlightGeo)
      updateFlightGeo$weatherDelay      <- delayCovars[,1]
      updateFlightGeo$nasDelay          <- delayCovars[,2]
      updateFlightGeo$carrierDelay      <- delayCovars[,3]
      updateFlightGeo$securityDelay     <- delayCovars[,4]
      updateFlightGeo$lateAircraftDelay <- delayCovars[,5]
      
      updateFlightGeo$g <- NULL
      
      save(updateFlightGeo, file = file.path("inst/extdata/cleaned/df", paste0("df_", stateDates[i], ".RDS")))
    }
  }  
  
  # Once all dates have covars, build distance comparison df
  print("Combining with previous data")
  geomatrix_distcovars <- readFlightgeoFromFolder(geoFolder = "inst/extdata/cleaned/geo")
  flightgeo_distcovars <- readFlightgeoFromFolder(dfFolder = "inst/extdata/cleaned/df")
  flightgeo_distcovars <- compareFlights(flightgeo_distcovars)
  
  print("Removing flights longer than 100,000 Km (bad data)")
  print(paste("Subseting to routes with at least", routeCountCutoff, "flights"))
  keep <- which(flightgeo_distcovars$routeCount >= routeCountCutoff & flightgeo_distcovars$distance < 100000000)
  if(length(keep) < nrow(flightgeo_distcovars)){
    flightgeo_distcovars <- flightgeo_distcovars[keep,]
    geomatrix_distcovars <- geomatrix_distcovars[keep]
  }
  flightgeo_distcovars$g <- NULL
  
  print("Saving in route folders")
  createRouteFolders(flightgeo_distcovars, geomatrix_distcovars, 
                     file.path(appDir, "appData/appRoutes"))
  
  print("Summarizing route data")
  routes <- createRouteLevelData(flightgeo_distcovars)
  
  print("Saving route data")
  save(routes, file = file.path(appDir, "appData/routes.RDS"))
  
  print("Saving data for total cost calculations")
                                        
  costData <- flightgeo_distcovars[ , c("date", "continent", "route", "manufacturer",
                                        "minincrease", "kmincreaseFlight",
                                        "thisWeather", "regionWeather",
                                        "stormEvent", "weatherDelay", "nasDelay")]
  costData[,c(10,11)] <- as.logical(costData[,c(10,11)] > 0)
  costData$hourincrease <- costData$minincrease / 60
  save(costData, file = file.path(appDir, "appData/costData.RDS"))
}

