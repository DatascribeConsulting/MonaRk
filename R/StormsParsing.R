#' Parse Storm Event data into usable format
#'
#' Storm event data is downloaded from https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/
#' Keep all storm event CSVs in a single folder and pass it to this function to create a
#' df to be used in further flight data processing. Filters out storm events that 
#' have no relevance to flights. Converts all timezone data to UTC.
#'
#' @param folder Location of storm CSVs 
#' @return A dataframe containing Storm Event information
#' @export
cleanStorms <- function(folder){
  
  files <- list.files(folder, full.names = T)
  
  storms <- lapply(files, function(x){
    fread(x)
  })
  
  storms <- do.call(rbind, storms)
  
  # take out Hawaii and storms without coordinates
  storms <- storms[which(storms$BEGIN_LON > -140),]  
  
  storms <- storms[, 
                   c("BDT_YMD", # I won't use this; have to extract
                     "BDT_HMS") := tstrsplit(BEGIN_DATE_TIME, 
                                             split = " ", 
                                             fixed = TRUE)
                   ][, 
                     c("EDT_YMD", # I won't use this; have to extract
                       "EDT_HMS") := tstrsplit(END_DATE_TIME, 
                                               split = " ", 
                                               fixed = TRUE)
                     ][, 
                       ":=" (
                         # Create BDT_YMD_HMS
                         BDT_YMD_HMS = paste(
                           paste(
                             YEAR, 
                             MONTH_NAME, 
                             BEGIN_DAY, 
                             sep = "-"
                           ), 
                           BDT_HMS, 
                           sep = " "), 
                         # Create EDT_YMD_HMS
                         EDT_YMD_HMS = paste(
                           paste(
                             YEAR, 
                             MONTH_NAME, 
                             END_DAY, 
                             sep = "-"
                           ), 
                           EDT_HMS, 
                           sep = " ")
                       )]
  
  # Now convert `BDT_YMD_HMS` and `EDT_YMD_HMS` to POSIXct var
  storms <- storms[, 
                   `:=` (
                     BDT_POSIX = ymd_hms(BDT_YMD_HMS), 
                     EDT_POSIX = ymd_hms(EDT_YMD_HMS)
                   )]
  
  storms <- storms[, CZ_TIMEZONE := toupper(CZ_TIMEZONE)]
  
  
  storms <- storms[CZ_TIMEZONE == "AST-4", 
                   BEGIN_YMDHMS_L := force_tz(BDT_POSIX, 
                                              tz = "America/Halifax")]
  storms <- storms[CZ_TIMEZONE == "AST-4", 
                   END_YMDHMS_L:= force_tz(EDT_POSIX, 
                                           tz = "America/Halifax")]
  storms <- storms[CZ_TIMEZONE == "EST-5", 
                   BEGIN_YMDHMS_L := force_tz(BDT_POSIX, 
                                              tz = "America/New_York")]
  storms <- storms[CZ_TIMEZONE == "EST-5", 
                   END_YMDHMS_L:= force_tz(EDT_POSIX, 
                                           tz = "America/New_York")]
  storms <- storms[CZ_TIMEZONE == "CST-6", 
                   BEGIN_YMDHMS_L := force_tz(BDT_POSIX, 
                                              tz = "America/Chicago")]
  storms <- storms[CZ_TIMEZONE == "CST-6", 
                   END_YMDHMS_L:= force_tz(EDT_POSIX, 
                                           tz = "America/Chicago")]
  storms <- storms[CZ_TIMEZONE == "MST-7", 
                   BEGIN_YMDHMS_L := force_tz(BDT_POSIX, 
                                              tz = "America/Denver")]
  storms <- storms[CZ_TIMEZONE == "MST-7", 
                   END_YMDHMS_L:= force_tz(EDT_POSIX, 
                                           tz = "America/Denver")]
  storms <- storms[CZ_TIMEZONE == "PST-8", 
                   BEGIN_YMDHMS_L := force_tz(BDT_POSIX, 
                                              tz = "America/Los_Angeles")]
  storms <- storms[CZ_TIMEZONE == "PST-8", 
                   END_YMDHMS_L:= force_tz(EDT_POSIX, 
                                           tz = "America/Los_Angeles")]
  storms <- storms[CZ_TIMEZONE == "AKST-9", 
                   BEGIN_YMDHMS_L := force_tz(BDT_POSIX,
                                              tz = "America/Anchorage")]
  storms <- storms[CZ_TIMEZONE == "AKST-9", 
                   END_YMDHMS_L := force_tz(EDT_POSIX, 
                                            tz = "America/Anchorage")]
  storms <- storms[CZ_TIMEZONE == "HST-10", 
                   BEGIN_YMDHMS_L := force_tz(BDT_POSIX, 
                                              tz = "Pacific/Honolulu")]
  storms <- storms[CZ_TIMEZONE == "HST-10", 
                   END_YMDHMS_L:= force_tz(EDT_POSIX, 
                                           tz = "Pacific/Honolulu")]
  storms <- storms[CZ_TIMEZONE == "SST-11", 
                   BEGIN_YMDHMS_L := force_tz(BDT_POSIX, 
                                              tz = "Pacific/Samoa")]
  storms <- storms[CZ_TIMEZONE == "SST-11", 
                   END_YMDHMS_L:= force_tz(EDT_POSIX, 
                                           tz = "Pacific/Samoa")]
  
  storms <- storms[, BEGIN_YMDHMS_UTC := with_tz(BEGIN_YMDHMS_L, tz = "UTC")]
  storms <- storms[, END_YMDHMS_UTC := with_tz(END_YMDHMS_L, tz = "UTC")]
  
  storms <- storms[ , c("EVENT_TYPE", "STATE", "CZ_NAME",  # keep state and city - some lat/lons missing
                        "BEGIN_LAT", "BEGIN_LON", "END_LAT", "END_LON",  # lat/longs
                        "BEGIN_YMDHMS_UTC", "END_YMDHMS_UTC")]  # times
  
  # keep only types of storms that are relevant to planes
  storms <- storms[which(storms$EVENT_TYPE != "Flood" &
                         storms$EVENT_TYPE != "High Surf" &
                         storms$EVENT_TYPE != "Wildfire" &
                         storms$EVENT_TYPE != "Coastal Flood" &
                         storms$EVENT_TYPE != "Lakeshore Flood" &
                         storms$EVENT_TYPE != "Seiche" &
                         storms$EVENT_TYPE != "Heat" &
                         storms$EVENT_TYPE != "Marine Strong Wind" &
                         storms$EVENT_TYPE != "Marine High Wind" &
                         storms$EVENT_TYPE != "Marine Hail" &
                         storms$EVENT_TYPE != "Debris Flow" &
                         storms$EVENT_TYPE != "Avalance" &
                         storms$EVENT_TYPE != "Flash Flood" &
                         storms$EVENT_TYPE != "Debris Flood" &
                         storms$EVENT_TYPE != "Waterspout" &
                         storms$EVENT_TYPE != "Drought" &
                         storms$EVENT_TYPE != "Rip Current" &
                         storms$EVENT_TYPE != "Frost/Freeze" &
                         storms$EVENT_TYPE != "Dust Devil" &
                         storms$EVENT_TYPE != "Funnel Cloud")
                   ]  # lots of floods recorded, care more about air events
  
  storms$EVENT_TYPE[which(storms$EVENT_TYPE == "Marine Thunderstorm Wind")] <- "Thunderstorm Wind"

  ## subset storms after 9/29, since States data starts 9/30
  storms <- subset(storms, BEGIN_YMDHMS_UTC > "2019-09-29 12:00:00 UTC")
  
  storms$id <- 1:nrow(storms)
  
  storms$BEGIN_UNIX_UTC <- as.numeric(as.POSIXct(storms$BEGIN_YMDHMS_UTC))
  storms$END_UNIX_UTC <- as.numeric(as.POSIXct(storms$END_YMDHMS_UTC))
  
  return(storms)
}


#' Create sf dataframe from storm data
#'
#' Pass return from cleanStorms() to this function to create geo data from coordinates
#'
#' @param storms A df containing cleaned storm event data. Needs BEGIN_LAT, END_LAT, BEGIN_LON, and END_LON columns
#' @return A sf dataframe containing Storm Event information PLUS a geo data column with storm polygons
#' @export
polygonizeStorms <- function(storms, distance = 1000){
  
  polygon_list = list()
  
  for (i in 1:nrow(storms)){
    smallLat <- min(storms$BEGIN_LAT[i], storms$END_LAT[i]) 
    largeLat <- max(storms$BEGIN_LAT[i], storms$END_LAT[i])
    smallLon <- min(storms$BEGIN_LON[i], storms$END_LON[i])
    largeLon <- max(storms$BEGIN_LON[i], storms$END_LON[i])
    # clockwise starting from lower left
    polygon <- list(rbind(c(smallLon, smallLat), c(smallLon, largeLat), ### long first!
                          c(largeLon, largeLat), c(largeLon, smallLat),
                          c(smallLon, smallLat)))
    polygon_list[[i]] <- st_polygon(polygon)
  }
  polygon_sfc <- st_sfc(polygon_list, crs = 4326)  # define coordinate reference system
  
  # buffer by 1km default so points are squares
  polygon_sfc <- st_transform(polygon_sfc, 102010	)
  buffered <- st_buffer(polygon_sfc, distance, endCapStyle = 'SQUARE') 
  #transform back into lat-long after buffering
  buffered <- st_transform(buffered, 4326)
  
  # make sf object 
  storms_geo <- st_sf(storms, g = buffered)
  
  return(storms_geo)
}
