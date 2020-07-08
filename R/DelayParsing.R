#' Parse delay data into usable format
#'
#' Delay data is downloaded from https://www.transtats.bts.gov/Tables.asp?DB_ID=120.
#' Keep all delay CSVs in a single folder and pass it to this function to create a
#' df to be used in further flight data processing
#'
#' @param folder Location of delay CSVs 
#' @return A dataframe containing flight delay information
#' @export
cleanDelay <- function(folder){
  
  # read in data
  delayFiles <- list.files(folder)
  
  delay <- lapply(delayFiles, function(x){
      fread(file.path(folder,x))
    })

  delay <- do.call(rbind, delay)
  
  delay <- delay[,c("FlightDate", "DayOfWeek", "IATA_Code_Marketing_Airline", "IATA_Code_Operating_Airline",
                    "Tail_Number", "Flight_Number_Operating_Airline", "Origin", "Dest",
                    "WheelsOff", "WheelsOn",
                    "CRSElapsedTime", "ActualElapsedTime", "AirTime", "Flights", "Distance",
                    "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")]
  
  # subset to sunday and monday
  delaysub <- delay[delay$DayOfWeek == 7 | delay$DayOfWeek == 1,]
  
  delaysub$id <- 1:nrow(delaysub)
  
  return(delaysub)
}  # saved as delayGeo.RDS in MonaRk folder


