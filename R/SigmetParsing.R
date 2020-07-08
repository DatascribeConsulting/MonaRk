############################ READ SIGMET ###########################

#' Read from a folder containing sigmet txt files
#'
#' Raw sigmet text is stored in separate files for each hour, inside a folder for each day.
#' For each day that needs to have sigmet data processed, loop though and run this function
#' to get a character vector, and then parseSigmet() on that vector. Different sources of 
#' sigmet data contain different record separating characters, reflected in the "split" arguments.
#'
#' @param sigmetDataDir Location of sigmet TXTs
#' @return A character vector containing sigmet text
#' @export
readSigmetFiles <- function(sigmetDataDir){

  if (length(list.dirs(sigmetDataDir)) > 1) {
    stop("Please define 'sigmetDataDir' as a folder with a single day's worth of 
    hourly sigmet txt files. This folder contains subfolders, possibly indicating 
    multiple dates worth of sigmets.")
  }
  
  # loop through all txt files in directory
  rawSigmet <- unlist(lapply(list.files(sigmetDataDir), function(x){ 
    
    # read in text in each file
    path <- file.path(sigmetDataDir, x)
    sigmet <- readtext::readtext(path)$text

    ## For Sigmet Archive within 3 months
    if(grepl("\n\n=======================================================================\n",
             sigmet[1], fixed = TRUE)) {
      sigmet <- unlist(strsplit(sigmet, fixed=TRUE, split = "\n\n=======================================================================\n"))
      # remove first one with metadata
      sigmet <- sigmet[-1]
    } 
    ## For Sigmet Archive before lat 3 months
    else if (grepl("/******************************************************************/",
                     sigmet[1], fixed = TRUE)) {
      sigmet <- unlist(strsplit(sigmet, fixed=TRUE, split = "/******************************************************************/"))
      
      # remove empty entries
      sigmet <- sigmet[-which(sigmet == "")]
    }
    ## For scraped sigmets using SigmetStreamingDownload.R and cronR
    else {
      sigmet <- unlist(strsplit(sigmet, fixed=TRUE, split = "==\n\n"))
    }
    
    ## remove repeats
    sigmet <- unique(sigmet)
    
    return(sigmet)
  }))
  
  return(rawSigmet)
}



############## Utility Functions for main parsing function ################

findWtype <- function (sigmetHeader) {
  if (grepl("WS\\D{2}\\d{2}", sigmetHeader)) {
    return("WS")
  } else if (grepl("WV\\D{2}\\d{2}", sigmetHeader)){
    return("WV")
  } else if (grepl("WC\\D{2}\\d{2}", sigmetHeader)){
    return("WC")      
  }
}


findCountry <- function (sigmetHeader) {
  return(substr(gsub("WS|WV|WC", "", sigmetHeader), 1, 2))
}


findConvective <- function (rawSigmet){
  if (grepl("CONVECTIVE", rawSigmet)){
    return("CONVECTIVE")
  } else {
    return("SIGMET")
  }
}


findHazard <- function (sigmetBody){
  if (grepl("CONVECTIVE", sigmetBody)){
    return("CONVECTIVE")
  } else if (grepl("EMBD TS", sigmetBody)){
    return("Embedded Thunderstorms")
  } else if (grepl("OBSC TS", sigmetBody)){
    return("Obscured thunderstorms")
  } else if (grepl("FRQ TS", sigmetBody)){
    return("Frequent thunderstorms")
  } else if (grepl("SQL TS", sigmetBody)){
    return("Squall line thunderstorms")
  } else if (grepl("OBSC TSGR", sigmetBody)){
    return("Obscured thunderstorms with hail")
  } else if (grepl("EMBD TSGR", sigmetBody)){
    return("Embedded thunderstorms with hail")
  } else if (grepl("FRQ TSGR", sigmetBody)){
    return("Frequent thunderstorms with hail")
  } else if (grepl("SQL TSGR", sigmetBody)){
    return("Squall line thunderstorms with hail")
  } else if (grepl("SEV TURB", sigmetBody)){
    return("Severe turbulence")
  } else if (grepl("SEV TS", sigmetBody)){
    return("Severe thunderstorms")
  } else if (grepl("SEV ICE", sigmetBody)){
    return("Severe icing")
  } else if (grepl("SEV ICE (FZRA)", sigmetBody)){
    return("Severe icing due to freezing rain")
  } else if (grepl("SEV MTW", sigmetBody)){
    return("Severe mountain wave")
  } else if (grepl("HVY DS", sigmetBody)){
    return("Heavy duststorm")
  } else if (grepl("HVY SS", sigmetBody)){
    return("Heavy sandstorm")
  } else if (grepl("RDOACT CLD", sigmetBody)){
    return("Radioactive cloud")
  } else if (grepl("CNL", sigmetBody)){
    return("CANCELLED")
  } else if (grepl("CNCL", sigmetBody)){
    return("CANCELLED")
  } else if (grepl("TS", sigmetBody)){
    return("Thunderstorm")
  } else {
    return("Unknown")
  }
}


getDirCoef <- function (direction) {
  
  ## direction to coefficient translation
  # lon, lat
  shortCoef <- sin(22.5 * pi/180)  # .38
  midCoef   <- sin(45.0 * pi/180)  # .70
  longCoef  <- sin(67.5 * pi/180)  # .92
  
  if (direction == "N") {
    return(c(0, 1))
  } else if (direction == "NNE") {
    return(c(shortCoef, longCoef))
  } else if (direction == "NE") {
    return(c(midCoef, midCoef))
  } else if (direction == "ENE") {
    return(c(longCoef, shortCoef))
  } else if (direction == "E") {
    return(c(1, 0))
  } else if (direction == "ESE") {
    return(c(longCoef, -shortCoef))
  } else if (direction == "SE") {
    return(c(midCoef, -midCoef))
  } else if (direction == "SSE") {
    return(c(shortCoef, -longCoef))
  } else if (direction == "S") {
    return(c(0, -1))
  } else if (direction == "SSW") {
    return(c(-shortCoef, -longCoef))
  } else if (direction == "SW") {
    return(c(-midCoef, -midCoef))
  } else if (direction == "WSW") {
    return(c(-longCoef, -shortCoef))
  } else if (direction == "W") {
    return(c(-1, 0))
  } else if (direction == "WNW") {
    return(c(-longCoef, shortCoef))
  } else if (direction == "NW") {
    return(c(-midCoef, midCoef))
  } else if (direction == "NNW") {
    return(c(-shortCoef, longCoef))
  } 
}


findTop <- function(sigmetBodySplit){
  FL <- sigmetBodySplit[which(sigmetBodySplit == "TOP")+1]
  if(length(FL)==0){
    FL <- sigmetBodySplit[which(grepl("FL", sigmetBodySplit))]
  }
  return(ifelse(length(FL)==0, NA, FL))
}


findTimes <- function(sigmetHeader, sigmetBodySplit, convective, day){
  
  start <- NA
  end <- NA
  
  if (convective == "CONVECTIVE"){
    if(!is.na(sigmetHeader[7])){
      startIndex <- 7 
    } else {
      startIndex <- 3   # transition time can be start time if valid doesn't have two times
    }
    # get the 6 digit times containing day, hour, minute
    times <- c(sigmetHeader[startIndex],  ## from 
                 gsub("Z", "", paste0(
                  # substr(sigmetHeader[startIndex],1,2),  # to day
                   sigmetBodySplit[which(sigmetBodySplit == "UNTIL") + 1]  # to time
                 ))
              )

    # convert to datetime
    if(length(times) > 0){
      # only take time
      times[1] <- substr(times[1], 3, nchar(times[1]))
      #times[2] <- substr(times[2], 3, nchar(times[2]))
    }
  } 
  
  # not convective
  else {
    times <- strsplit(sigmetBodySplit[which(sigmetBodySplit == "VALID") + 1], split = "/")

    if(length(times) > 0){
      times <- c(substr(times[[1]][1], 3, nchar(times[[1]][1])),
                 substr(times[[1]][2], 3, nchar(times[[1]][2])))
    } 
  }
  
  if(length(times) > 0){
    start <- as.numeric(lubridate::ymd_hm(paste0(day, "_",times[1])))
    end <- as.numeric(lubridate::ymd_hm(paste0(day, times[2])))
  } 
  
  return(list(start,end))
}


getSigReferencePoints <- function(){
  refPoints <- data.table::fread("inst/extdata/helpers/sigmetReferencePoints.csv")
  
  refPoints <- st_as_sf(refPoints, coords = c("Lon", "Lat"), 
                       crs = 4326, agr = "constant")
  
  refPoints_m <- st_transform(refPoints, 102003)
  
  return(refPoints_m)
}


makeRefPointPoly <- function(sigmetBody){

  refPoints_m <- getSigReferencePoints()
  
  # Sigmet pre processing
  refPointSig <- sub(".*FROM *(.*?) *AREA.*", "\\1", sigmetBody)
  refPointSig <- strsplit(refPointSig, split = "-|\n| TO ")[[1]]

  coords <- NULL
  # Loop to get points
  for (point in 1:length(refPointSig)){
    
    meters <- NA
    refPointSigSpl <- NA
    startLon <- NA
    startLat <- NA
    newLon <- NA
    newLat <- NA
    
    #print(paste("point:", point))
    meters <- as.numeric(gsub("[^[:digit:]]", "\\1", refPointSig[point])) * 1609.34  # miles to meters
    refPointSigSpl <- strsplit(refPointSig[point], " ")[[1]]
    direction <- gsub("[^[:alpha:]]", "\\1", refPointSigSpl[1])
    refPointCode <- refPointSigSpl[2]
    
    if(!is.null(direction) && !is.na(direction) && !direction == "" &&
       !is.null(refPointCode) && !is.na(refPointCode) && !refPointCode == "" &&
       !is.null(meters) && !is.na(meters) && !meters == ""){
    
      dirCoef <- getDirCoef(direction)
      
      refPointMatch <- which(refPoints_m$ID == refPointCode)
      
      if(length(refPointMatch) > 0) {
        # longitude
        startLon <- refPoints_m[refPointMatch[1],]$geometry[[1]][1]
        newLon <- startLon + meters * dirCoef[1] # convert degrees to radians
        
        # latitude
        startLat <- refPoints_m[refPointMatch[1],]$geometry[[1]][2]
        newLat <- startLat + meters * dirCoef[2]
        
        coords <- rbind(coords, cbind(newLon, newLat))
      }
    } 
    ## just code and no distance/direction
    else if (!is.null(direction) && !is.na(direction) && !direction == "" &&
             refPointSigSpl == direction && length(refPointSigSpl) == 1 &&
             is.na(meters)){
      refPointMatch <- which(refPoints_m$ID == refPointSigSpl)
      newLon <- refPoints_m[refPointMatch[1],]$geometry[[1]][1][[1]]
      newLat <- refPoints_m[refPointMatch[1],]$geometry[[1]][2][[1]]

      if(!is.null(newLon) && !is.null(newLat)){
        coords <- rbind(coords, cbind(newLon, newLat))  
      }
      
    }
  }

  if(!is.null(coords) && nrow(coords) > 1){
    
    coords <- coords[!(rowSums(is.na(coords)) > 0), ]
    if(nrow(coords) > 1 && !identical(coords[1,], coords[nrow(coords),])){
      coords <- rbind(coords, coords[1,])  
    }
    if(nrow(coords) > 1){
      # Make points into polygon
      polygon <- st_polygon(list(coords))
      polygon <- st_transform(st_sfc(polygon, crs = 102003), crs = 4326)
      polygon <- polygon[[1]]
      # need to return a polygon for the "polygon[[poly]] <-" part
      return(polygon)
    }
  }
}


checkCoord <- function(newLon, newLat){

  newPoint <- as.data.frame(cbind(newLon, newLat))
  newPoint <- st_as_sf(newPoint, coords = c("newLon", "newLat"), 
                       crs = 102003, agr = "constant")
  newPoint <- st_transform(newPoint, crs = 4326)
  
  leaflet() %>% 
    addProviderTiles("CartoDB.Positron", group = "Carto") %>%
    addMarkers(data = newPoint)
}


formatCoords <- function (oneDir) {
  # remove common extra characters
  latOrlon <- gsub(pattern = "-", "", oneDir, fixed = T)
  latOrlon <- gsub(pattern = ".", "", latOrlon, fixed = T)
  latOrlon <- gsub(pattern = "TOP", "", latOrlon)
  
  ## extract degrees
  dd <- sub("[NSEW]", "", latOrlon)
  dd <- as.numeric(gsub('\\d{2}$', '', dd))
  
  # extract minutes - take the last two str spots
  mm <- as.numeric(str_sub(latOrlon,-2,-1))
  
  # extract seconds - replace any numbers with nothing  
  ns <- sub("\\d{3,}.*", "", latOrlon)
  
  # change to decimal degrees
  latOrlon <- biogeo::dms2dd(dd=dd, mm=mm, ss=rep(0, length(latOrlon)), ns=ns)

  return(latOrlon)
  
}

## useful for projecting coords when faced with global datasets
## SOURCE: Geocomputation with R. Robin Lovelace, Jakub Nowosad, Jannes Muenchow.
## https://geocompr.robinlovelace.net/reproj-geo-data.html
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}



############################ Main parsing function ########################
#' Parse raw sigmet text into a dataframe
#'
#' Loop through each sigmet record to coalesce relevant data like 
#' time, location, and type. Contains lots of helper functions 
#' and special cases since there are many styles of sigmet record
#' 
#' @param rawSigmet A character vector containing raw sigmet records for one day
#' @param day The date in format "YYYY-MM-DD" since records have day but not month or year
#' @return A df with rows as sigmets, and columns as time, location, etc
#' @export
parseSigmet <- function(rawSigmet, day){
  # TODO: REMOVE THIS ONCE A PACKAGE 
  #setwd("/Users/kevinschweiker/Documents/Freelancing/Airbus/routeComparisonApp/")
  
  # initialize vectors to keep and intermediate values
  convective = c()
  sigIndex = c()
  polyIndex = c() 
  Wtype = c()
  country = c()
  headerNum = c()
  dissCenter = c()
  transT = NA
  transTime = c()
  
  icaoLoc = c()
  seqNum = c()
  
  times = NA
  start = c()
  end = c()
  hazard = c()
  top = c()
  
  shape = c()
  polygon = list()
  coords <- NA
  lon <- NA
  lat <- NA
  
  poly = 0
  
  airports <- data.table::fread("inst/extdata/helpers/airports.csv")
  
  for (sig in 1:length(rawSigmet)){
    print(sig)

    # break into header + sigmet entries
    multSigmetSplit <- strsplit(rawSigmet[sig], 
                                split = "CONVECTIVE SIGMET|SIGMET")[[1]]
    onSigmetHeader <- strsplit(multSigmetSplit[1], split = " |\n")[[1]]
    onSigmetHeader <- onSigmetHeader[which(onSigmetHeader != "" & 
                                             onSigmetHeader != "W" & 
                                             onSigmetHeader != "WS")]

    # sometimes random characters at the beginning
    while(!grepl("^W",onSigmetHeader[1]) && length(onSigmetHeader) >= 3){
      onSigmetHeader <- onSigmetHeader[-1]
    }

    # if the header contains reasonable information, continue
    if(length(onSigmetHeader) >= 3){
      # sometimes multiple sigmets per entry, we'll reuse the header information
      for (subSigmet in 1:length(multSigmetSplit[-1])) {
       
          #### body of subsigmet 
          sigmetBody <- multSigmetSplit[subSigmet + 1]  # skip header
          
          ### look at body of sigmet one word at a time
          sigmetBodySplit <- strsplit(sigmetBody, split = " |-|\n|\\.\\.\\.")[[1]]
          sigmetBodySplit <- sigmetBodySplit[which(sigmetBodySplit != "")]
        
          # Convective sigmets still record even if no storms, contain NONE as first word
          if(sigmetBodySplit[1] != "NONE"){  
            
            poly <- poly + 1
            
            # convective and normal sigmets are in different formats - look at whole sig
            convective[poly] <- findConvective(rawSigmet[sig])
            
            # keep track of the sigmet number
            sigIndex[poly] <- sig
            
            # keep track of the polygon number since there are multiple polys per sig
            polyIndex[poly] <- poly

            # onSigmetHeader[1] is a string like "\nWSBZ01 SBBR 100000\nSBAZ " 
            # find if it is a generic sigmet, volcanic ash, or tropical cyclone
            Wtype[poly] <- findWtype(onSigmetHeader[1])
            
            # Get country code
            country[poly] <- findCountry(onSigmetHeader[1])
            
            # get number after country code
            headerNum[poly] <- substr(onSigmetHeader[1], 
                                   nchar(onSigmetHeader[1]) - 1, 
                                   nchar(onSigmetHeader[1]))
            
            # dissemination center
            dissCenter[poly] <- onSigmetHeader[2]

            # transmission time
            transT <- substr(onSigmetHeader[3], 3, nchar(onSigmetHeader[3]))
            transTime[poly] <- as.numeric(lubridate::ymd_hm(paste0(day, transT)))
            
            # Icao location code and seq num
             if (convective[poly] == "CONVECTIVE"){
               icaoLoc[poly] <- onSigmetHeader[5]
               seqNum[poly]  <- NA
             } else {
               icaoLoc[poly] <- onSigmetHeader[4]
               seqNum[poly]  <- sigmetBodySplit[1]
             }
          
            ## Look at whole sigmet for hazard
            hazard[poly] <- findHazard(sigmetBody)
            
            ## find flight level top
            top[poly] <- findTop(sigmetBodySplit)
         
            ## find start and end times
            times <- findTimes(onSigmetHeader, sigmetBodySplit, convective[poly], day)
            start[poly] <- times[[1]]
            end[poly] <- times[[2]]
            
            ## if not start and end time under "valid", use transmission time
            if(is.na(start[poly])){
              start[poly] <- transTime[poly]
            }
            if(is.na(end[poly])){
              end[poly] <- transTime[poly] + 3600  # add two hours from transmission time
            }
           
            # initialize some things before getting to polygons
            polygon[[poly]] <- NA
            shape[poly] <- "other"
            coords <- NA
            lon <- NA
            lat <- NA
            
            ## get vector of lat and lon
            lon <- str_subset(sigmetBodySplit, "[EW].\\d{3,}")
            lat <- str_subset(sigmetBodySplit, "[NS].\\d{3,}")
            if(!is.na(lat) && !is.na(lon) && length(lon) > 0 && length(lat) > 0){
                lon <- formatCoords(lon)
                lat <- formatCoords(lat)
                coords <- unique(cbind(lon,lat))
                if(!is.null(nrow(coords)) && nrow(coords) > 0){
                  coords <- coords[!(rowSums(is.na(coords)) > 0), ]
                }
            }

            ##### CIRCLES #####
            if(!is.null(nrow(coords)) && nrow(coords) == 1){
              shape[poly] <- "circle"
              point <- st_point(coords)
              
              # recorded in nautical miles, change to meters (1852m to 1nm)
              rad <- sigmetBodySplit[which(grepl("NM",sigmetBodySplit))][1]
              rad <- gsub("[^[:digit:]]", "\\1", rad)
              rad <- as.numeric(rad) * 1852
              
              if(is.na(rad) || is.null(rad) || length(rad) == 0 || rad == 0 || rad == "") { rad <- 20 * 1852}
              
              point <- st_transform(st_sfc(point, crs = 4326), crs = lonlat2UTM(point))
              buffer <- st_buffer(point, dist = rad, endCapStyle = "ROUND")
              polygon[[poly]] <- st_transform(buffer, crs = 4326)[[1]]
            }
            
            ##### Buffered Line #####
            else if(!is.null(nrow(coords)) && nrow(coords) == 2) {
              shape[poly] <- "line"
              rad <- NA
              rad <- sigmetBodySplit[which(sigmetBodySplit == "WTN") + 1]
              rad <- gsub("[^[:digit:]]", "\\1", rad)
              rad <- as.numeric(rad) * 1852
              if(is.na(rad) || is.null(rad) || length(rad) == 0 || rad == 0 || rad == "") {
                rad <- 20 * 1852
              }
              line <- st_linestring(coords)
              # rad in nautical miles
              line <- st_transform(st_sfc(line, crs = 4326), crs = lonlat2UTM(coords[floor(nrow(coords)/2),]))
              buffer <- st_buffer(line, dist = rad, endCapStyle = "ROUND")
              polygon[[poly]] <- st_transform(buffer, crs = 4326)[[1]]
            } 
            
            ##### Polygon #####
            else if (!is.null(nrow(coords)) && nrow(coords) > 2){
              shape[poly] <- "polygon"
              if(!identical(coords[1,], coords[nrow(coords),])){
                coords <- rbind(coords, coords[1,])  
              }
              # remove sigmets that are too big to be real
              if((max(coords[,1]) - min(coords[,1]) < 60) &&
                 (max(coords[,2]) - min(coords[,2]) < 60)){
                    polygon[[poly]] <- st_polygon(list(coords))
                 }
            }
              
            ##### Polygon from reference points #####
            if ("FROM" %in% sigmetBodySplit && is.na(polygon[[poly]])){
              newPoly <- NA
              newPoly <- makeRefPointPoly(sigmetBody)
              
              if (!is.null(newPoly) && !is.na(newPoly)){
                shape[poly] <- "polygon"
                polygon[[poly]] <- newPoly
              }
            }

            ##### Circle from FIR location estimate #####
            if ( ("FIR" %in% sigmetBodySplit || "FIR/UIR" %in% sigmetBodySplit) && 
               (is.na(polygon[[poly]]) || length(polygon[[poly]]) ==0)  &&
               !("CNL" %in% sigmetBodySplit) && !("CNCL" %in% sigmetBodySplit) ) {
              shape[poly] <- "estimate"
              place <- sigmetBodySplit[which(sigmetBodySplit == "FIR" | sigmetBodySplit == "FIR/UIR")[1] - 1]
              airportMatch <- which(grepl(place, toupper(airports$name)) & grepl(pattern = substr(icaoLoc[poly], 1,2), airports$icao))[1]
              coords <- airports[airportMatch, c("X", "Y")]
              if (is.na(coords) || nrow(coords) == 0) {
                airportMatch <- which(grepl(place, toupper(airports$city)) & grepl(pattern = substr(icaoLoc[poly], 1,2), airports$icao))[1]                
                coords <- airports[airportMatch, c("X", "Y")]
              }
              if ( !(any(is.na(coords))) && nrow(coords) > 0 ) {
                point <- st_point(as.numeric(coords))
                point <- st_transform(st_sfc(point, crs = 4326), crs = lonlat2UTM(point))
                # buffer by 100km
                buffer <- st_buffer(point, dist = 100*1000, endCapStyle = "ROUND")
                polygon[[poly]] <- st_transform(buffer, crs = 4326)[[1]]
              }
            }
        }
      }
    }
  }

  df <- data.frame(
    # basic info
    "polyIndex" = polyIndex,
    "sigIndex" = sigIndex,
    "fulltext" = rawSigmet[sigIndex],
    
    # header info
    "Wtype" = Wtype,
    "A1A2" = country,
    "headerNum" = headerNum,
    "dissCenter" = dissCenter,
    "transTime" = transTime,
    
    "ICAO.code" = icaoLoc,
    "seqNum" = seqNum,
    
    # time
    "start"   = start,
    "end"     = end,
    "date"    = lubridate::date(as.POSIXct(start, origin = "1970-01-01", tz = "GMT")),
    
    # weather
    "hazard"  = hazard,
    "top"     = top,
    
    "shape" = shape
  )
  
  if(nrow(df) > 0){
    
    # merge with country code
    if(!exists("countryTrans")){
      countryTrans <- read.csv("inst/extdata/helpers/sigmetCountryIndicators.csv")
    }
    df <- dplyr::left_join(df, countryTrans, by = "A1A2")
    
    # merge with icao code
    if(!exists("icaoTrans")){
      icaoTrans <- read.csv("inst/extdata/helpers/sigmetICAOIndicators.csv")
    }
    df <- dplyr::left_join(df, icaoTrans, by = "ICAO.code")
    
    # geo functions
    keepIndices <- sapply(polygon, function(x) !is.null(x) && !is.na(x) && length(x) > 0)
    polygon <- polygon[keepIndices]
    sigmetPolys <- st_sfc(polygon, crs = 4326)
    sigmetgeo <- st_sf(df[keepIndices,], g = sigmetPolys)
    
    return(sigmetgeo)
  }
}



######################## Sigmet map function ########################

#' Map sigmet data
#'
#' Pass a parsed sigmetgeo dataframe to see the shapes of the sigmet on a map.
#' Click a sigmet to see additional data in a tooltip.
#' 
#' @param sigmetgeo A df with rows as sigmets, and columns as time, location, etc
#' @return A leaflet map showing sigmet outlines 
#' @export

plotSigmet <- function (sigmetgeo){
  storm_popup <- paste("orig ID:", as.character(sigmetgeo$sigIndex), "<br>",
                       "polygon ID:", as.character(sigmetgeo$polyIndex), "<br>" ,
                       "hazard:", sigmetgeo$hazard, "<br>" ,
                       "Valid from time:", as.POSIXct(sigmetgeo$start, origin = "1970-01-01", tz = "UTC"), "<br>",
                       "Valid until time:", as.POSIXct(sigmetgeo$end, origin = "1970-01-01", tz = "UTC"), "<br>",
                       "Full sigmet:", sigmetgeo$fulltext)
  leaflet() %>% 
    addProviderTiles("CartoDB.Positron", group = "Carto") %>%
    addPolylines(data=st_zm(sigmetgeo), 
                 color = "#77aaee", opacity = 1, weight = 1,
                 popup = storm_popup,
                 highlightOptions = highlightOptions(
                   color='#ff0000', opacity = 1, weight = 2, fillOpacity = 1,
                   bringToFront = TRUE, sendToBack = TRUE))
}

