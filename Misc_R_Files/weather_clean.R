library(readr)
location <- read_csv("~/Desktop/R Files/Data Expo 2018/data/locations.csv")
historical <- read_csv("~/Desktop/R Files/Data Expo 2018/data/histWeather.csv")
forecast <- read.table(file = "~/Desktop/R Files/Data Expo 2018/data/forecast.dat", header = F)

names(forecast) <- c("city","date_forecasted","predicted_value","forecast_variable","predicted_date")
forecast[,'airport_code'] <- location[forecast$city,]$AirPtCd
forecast$city <- paste(location[forecast$city,]$city, ",", location[forecast$city,]$state,sep = "")

historical <- historical[,c(1,2,4,20,24)]


# eastport <- subset(forecast, forecast$city=="Eastport,Maine")
# airport_eastport <- subset(historical, historical$AirPtCd=="KBHB")
# airport_eastport$Date <- as.Date(airport_eastport$Date)
# eastport$predicted_date <- as.Date(eastport$predicted_date)

# airport_eastport[!(eastport$predicted_date %in% airport_eastport$Date),] 
# for(i in 1:(nrow(airport_eastport)-1)){
#   if(airport_eastport$Date[i+1] - airport_eastport$Date[i] > 1){
#     print(paste("missing info: ",airport_eastport$Date[i+1],airport_eastport$Date[i]))
#   }
# }
# 
# for(j in 1:(nrow(eastport)-1)){
#   if(eastport$predicted_date[i+1] - eastport$predicted_date[i] > 1){
#     print(paste("missing forecast: ",eastport$predicted_date[i+1], eastport$predicted_date[i]))
#   }
# }
# 
# ctr <- 0
# ind <- 1; v.names <- rep(NA, 24); v.vals <- rep(NA, 24)
# for(i in 1:(nrow(historical)-1)){
#   if(historical$Date[i+1] - historical$Date[i] > 1 &&
#      historical$AirPtCd[i+1] == historical$AirPtCd[i]){
#     x <- paste(historical$AirPtCd[i], historical$Date[i+1], historical$Date[i], historical$Date[i+1] - historical$Date[i])
#     # print(x)
#     # ctr <- ctr + historical$Date[i+1] - historical$Date[i]
#     v.names[ind] <- substr(x, 1,4)
#     v.names[ind] <- substr(x, 6,100)
#     ind <- ind + 1
#   }
# }
#e.g. 26 missing dates for Eastport that were never forecasted
#what would imputing tell us about this

# missing <- c("KDET 2014-09-08 2014-09-06 2"
#              , "KBAD 2014-12-23 2014-12-16 7"
#              , "KAAO 2014-07-17 2014-07-15 2"
#              , "KAAO 2014-08-11 2014-08-08 3"
#              , "KSGU 2014-07-11 2014-07-09 2"
#              , "KSMN 2014-10-20 2014-10-17 3"
#              , "KOSU 2015-11-30 2015-11-28 2"
#              , "KCPS 2015-07-10 2015-07-08 2"
#              , "KMLF 2015-03-23 2015-03-21 2"
#              , "KSMN 2015-09-27 2015-09-25 2"
#              , "KSMN 2015-12-09 2015-09-28 72"
#              , "KEED 2015-03-05 2015-03-03 2"
#              , "KAAO 2016-11-02 2016-10-31 2"
#              , "KTAD 2016-07-27 2016-07-25 2"
#              , "KTAD 2016-08-01 2016-07-30 2"
#              , "KMLF 2016-09-11 2016-09-09 2"
#              , "KSMN 2016-06-29 2016-06-01 28"
#              , "KBHB 2017-07-28 2017-07-26 2"
#              , "KMXF 2017-07-05 2017-06-30 5"
#              , "KHOT 2017-05-02 2017-04-29 3"
#              , "KBAD 2017-04-02 2017-03-30 3"
#              , "KAAO 2017-06-20 2017-06-18 2"
#              , "KSGU 2017-05-19 2017-05-17 2"
#              , "KSMN 2017-08-01 2017-07-18 14")
# mnames <- substr(missing,1,4)
# totals <- rep(NA, length(unique(mnames))); names(totals) <- unique(mnames)
# for(i in 1:length(unique(mnames))){
#   x <- missing[which(unique(mnames)[i] == substr(missing,1,4))]
#   totals[i] <- sum(as.numeric(substr(x, nchar(x)-1,nchar(x))))
# }
# 
# #ONLY CITY WITH SIGNIFICANT MISSING IS KSMN - Salmon, ID
# 
#finding missing values by each variable ("Max_TemperatureF" to "CloudCover", e.g. 2 to 21)

missing.values <- data.frame(matrix(NA,ncol=20,nrow=length(unique(historical$AirPtCd))))
colnames(missing.values) <- colnames(historical)[2:21]

for(j in 1:length(unique(historical$AirPtCd))){
  sub <- subset(historical, historical$AirPtCd == unique(historical$AirPtCd)[j])
  rownames(missing.values)[j] <- unique(historical$AirPtCd)[j]
  for(i in 2:21){
    missing.values[j,i-1] <- nrow(sub[is.na(sub[,i]),])
  }
}

missing.values <- missing.values[,c(1,3,19)]

#relevant missing values
new.miss <- missing.values[missing.values$Max_TemperatureF %in% sort(missing.values$Max_TemperatureF[missing.values$Max_TemperatureF > 0],decreasing = T),]
new.miss <- new.miss[order(new.miss$Max_TemperatureF,decreasing = T),]
new.miss_no1 <- new.miss[new.miss$Max_TemperatureF>1,] #is one missing value relevant?


#MinTemp == Min_TemperatureF
#MaxTemp == Max_TemperatureF
#ProbPrecip == PrecipitationIn

# hist1 <- subset(historical, historical$AirPtCd %in% rownames(new.miss))
# hist1$Date <- as.Date(hist1$Date)
# fore1 <- subset(forecast, forecast$airport_code %in% rownames(new.miss))
# fore1$date_forecasted <- as.Date(fore1$date_forecasted)
# fore1$predicted_date <- as.Date(fore1$predicted_date)

get.max.res <- function(airportcode, hist1, fore1){ #maxtemp
    h <- subset(hist1, hist1$AirPtCd == airportcode)
    f <- subset(fore1, fore1$airport_code == airportcode & fore1$forecast_variable == "MaxTemp")
    
    #match forecasted values with historical values
    f <- f[match(f$date_forecasted, h$Date),]
    f <- f[order(f$date_forecasted),]
    
    #col1: actual hist value, #col2 predicted val, #col3 residual, 
    #col4 hist day, #col5 pred day, #col6 time res
    ret <- data.frame(matrix(NA,ncol=6,nrow=nrow(f)))
    names(ret) <- c("hist_value","pred_value","val_res","hist_day","pred_day","time_res")
    ret$hist_day <- f$date_forecasted; ret$pred_day <- f$predicted_date; ret$time_res <- (f$date_forecasted - f$predicted_date)
    ret$pred_value <- f$predicted_value
    
    for(i in 1:nrow(ret)){
      if(ret$hist_day[i] %in% h$Date){
        ret$hist_value[i] <- h[which(h$Date == ret$hist_day[i]),]$Max_TemperatureF
      }
      #add in else to handle missing  values. create function of above missing value code
    }
    ret$hist_value <- as.numeric(ret$hist_value)
    ret$pred_value <- as.numeric(as.character(f$predicted_value))
    ret$val_res <- abs(ret$hist_value - ret$pred_value)
    return(ret)
}

vec <- rep(NA,nrow(location))
names(vec) <- location$AirPtCd
for(i in 1:nrow(location)){
  x <- get.max.res(location$AirPtCd[i], historical, forecast)
  v <- lm(x$val_res ~ x$time_res)
  vec[i] <- summary(v)$r.squared
}


# #find mean and median
# mn <- rep(NA, length(cities))
# med <- rep(NA, length(cities))
# for(i in 1:length(cities)){
#   mn[i] <- mean(cities[[i]]$val_res)
#   med[i] <- median(cities[[i]]$val_res)
#   hist(cities[[i]]$val_res,main = location[i,]$city)
# }
# 
# location[,'mean_res'] <- mn
# location[,'med_res'] <- med/5
# 
# 
# map <- get_map(location=c(lon=mean(location$longitude), lat=mean(location$latitude)), zoom = 4)
# ggmap(map) +
#   geom_point(data=location, aes(x=longitude,y=latitude,size=med_res)) + scale_color_manual()
# 
# 
# #----GOALS
# #create full dataset using above function
# #Shiny app:
# ##--Select City (x113) and Variable (x3)
# ##--Output: 
# ###Plot -> var over time
# ###Plot -> res over time
# ###Plot -> res over time res
# ###Show: Mean, Med of var
# ###Show: Mean, Med of res

