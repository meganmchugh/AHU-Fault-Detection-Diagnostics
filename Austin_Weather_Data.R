# Megan K. McHugh, Masters Thesis Research, 2018
# Intelligent Environments Lab & UT Facilities Services Energy Management Optimization

Austin_Weather_Data <- function(stationID = "KTXAUSTI646", start_date = "2017-09-01", end_date = "2018-05-01") {
  
  library(urltools)
  library(lubridate)
  library(data.table)
  
  wu.url <- "https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=KTXAUSTI646&month=1&day=16&year=2018&format=1"
  
  wu.date <- seq.Date(from = ymd(start_date), to = ymd(end_date), by = "day")
  
  wu.seq <- lapply(wu.date, function(x) {
    wu.url <- param_set(wu.url, key = "ID", value = stationID)
    wu.url <- param_set(wu.url, key = "month", value = month(x))
    wu.url <- param_set(wu.url, key = "day", value = day(x))
    wu.url <- param_set(wu.url, key = "year", value = year(x))
    return(wu.url)
  })
  
  wu.dfl <- lapply(wu.seq, function(x) read.csv(x, row.names = NULL))
  
  if(ncol(wu.dfl[[1]]) == 16){
    colNames <- c("Time","TemperatureF","DewpointF","PressureIn","WindDirection","WindDirectionDegrees","WindSpeedMPH","WindSpeedGustMPH","Humidity","HourlyPrecipIn","Conditions","Clouds","DailyRainIn","SoftwareType","DateUTC")
  } else if(ncol(wu.dfl[[1]]) == 17){
    colNames <- c("Time","TemperatureF","DewpointF","PressureIn","WindDirection","WindDirectionDegrees","WindSpeedMPH","WindSpeedGustMPH","Humidity","HourlyPrecipIn","Conditions","Clouds","DailyRainIn","SolarRadiationWatts.m2","SoftwareType","DateUTC")
  }
  
  wu_fix <- function(df) {
    df <- df[,-ncol(df)]
    df <- df[c(TRUE, FALSE),]
    names(df) <- colNames
    return(df)
  }
  
  wu.dfl1 <- lapply(wu.dfl, wu_fix)
  
  wu.df <- rbindlist(wu.dfl1)
  
  wu.df$TemperatureC <- 5/9*(wu.df$TemperatureF-32)
  wu.df$DewpointC <- 5/9*(wu.df$DewpointF-32)
  wu.df$SatVaporPressure.mbar <- 6.11*10**(7.5*wu.df$TemperatureC/(237.7+wu.df$TemperatureC))
  wu.df$ActualVaporPressure.mbar <- 6.11*10**(7.5*wu.df$DewpointC/(237.7+wu.df$DewpointC))
  wu.df$PctRelativeHumidity <- (wu.df$ActualVaporPressure.mbar/wu.df$SatVaporPressure.mbar)*100
  
  # folder for outputs
  ifelse(!dir.exists("./outputs/Austin_Weather_Data"), dir.create("./outputs/Austin_Weather_Data"), FALSE)
  
  csv.name <- paste0("./outputs/Austin_Weather_Data/Austin_Weather_",start_date,"_to_",end_date,".csv")
  
  write.csv(wu.df, file = csv.name, row.names = F)
  
}

