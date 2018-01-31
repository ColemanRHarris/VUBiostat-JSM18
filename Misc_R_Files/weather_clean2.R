library(readr)
library(ggplot2)
library(ggmap)

#import data
location <- read_csv("~/Desktop/R Files/Data Expo 2018/data/locations.csv")
historical <- read_csv("~/Desktop/R Files/Data Expo 2018/data/histWeather.csv")
forecast <- read.table(file= "~/Desktop/R Files/Data Expo 2018/data/forecast.dat", header = F)
##PROB OF PRECIP // NOT IN in. for forecast

#some basic cleanings for forecast data
names(forecast) <- c("city","date_forecasted","predicted_value","forecast_variable","predicted_date")
forecast[,'airport_code'] <- location[forecast$city,]$AirPtCd
forecast$city <- paste(location[forecast$city,]$city, ",", location[forecast$city,]$state,sep = "")
forecast$date_forecasted <- as.Date(forecast$date_forecasted)
forecast$predicted_date <- as.Date(forecast$predicted_date)
forecast$forecast_variable <- as.character(forecast$forecast_variable)
forecast[which(forecast$forecast_variable == "MaxTemp"),]$forecast_variable <- "Max_TemperatureF"
forecast[which(forecast$forecast_variable == "MinTemp"),]$forecast_variable <- "Min_TemperatureF"
forecast[which(forecast$forecast_variable == "ProbPrecip"),]$forecast_variable <- "PrecipitationIn"

#cleanings for hist data
historical$Date <- as.Date(historical$Date)

saveRDS(forecast, "kinda-clean-data/forecast1.rds")
write.csv(historical, "kinda-clean-data/historical1.csv")

historical <- historical[,c(1,2,4,20,24)]

#residuals function
get.res <- function(airportcode, hist1, fore1){
  h <- subset(hist1, hist1$AirPtCd == airportcode)
  #remove NAs
  h <- h[!is.na(h),]
  #remove Ts -- WTF IS T
  h <- h[which(h$PrecipitationIn != "T"),]
  f <- subset(fore1, fore1$airport_code == airportcode)
  f <- f[!is.na(f),]
  f <- f[which(f$predicted_value != "M"),]
  
  #match forecasted values with historical values
  f <- f[!is.na(match(f$date_forecasted, h$Date)),]
  f <- f[order(f$date_forecasted),]
  
  #col1: actual hist value, #col2 predicted val, #col3 residual, 
  #col4 hist day, #col5 pred day, #col6 time res
  ret <- data.frame(matrix(NA,ncol=8,nrow=nrow(f)))
  names(ret) <- c("hist_value","pred_value","val_res","hist_day","pred_day","time_res","airport_code","variable")
  ret$hist_day <- f$date_forecasted
  ret$pred_day <- f$predicted_date
  ret$time_res <- (f$date_forecasted - f$predicted_date)
  ret$pred_value <- f$predicted_value
  ret$airport_code <- airportcode
  ret$variable <- f$forecast_variable
  
  #match historical values
  ret[which(ret$variable == "PrecipitationIn"),]$hist_value <- h[match(ret[which(ret$variable == "PrecipitationIn"),]$hist_day,h$Date),]$PrecipitationIn
  ret[which(ret$variable == "Max_TemperatureF"),]$hist_value <- h[match(ret[which(ret$variable == "Max_TemperatureF"),]$hist_day,h$Date),]$Max_TemperatureF
  ret[which(ret$variable == "Min_TemperatureF"),]$hist_value <- h[match(ret[which(ret$variable == "Min_TemperatureF"),]$hist_day,h$Date),]$Min_TemperatureF
  
  ret$hist_value <- as.numeric(ret$hist_value)
  ret$pred_value <- as.numeric(as.character(f$predicted_value))
  ret$val_res <- (ret$hist_value - ret$pred_value)
 
  
  return(ret)
}

z <- lapply(1:nrow(location),function(i){
  data.frame(get.res(location$AirPtCd[i],historical,forecast))
})
full_data <- data.table::rbindlist(z,use.names = T)

write.csv(full_data,"~/Desktop/R Files/Data Expo 2018/data/fulldata.csv")
saveRDS(full_data, "~/Desktop/R Files/Data Expo 2018/weather/data/fulldata.rds")
