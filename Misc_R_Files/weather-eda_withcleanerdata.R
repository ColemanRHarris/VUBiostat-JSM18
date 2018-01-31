library(readr)

l <- read_csv("~/Desktop/R Files/Data Expo 2018/data/locations.csv")
h <- read_csv("~/Desktop/R Files/Data Expo 2018/kinda-clean-data/historical1.csv"); h <- h[-1]
f <- readRDS(file="~/Desktop/R Files/Data Expo 2018/kinda-clean-data/forecast1.rds")
f[which(f$forecast_variable == "PrecipitationIn"),]$forecast_variable <- "ProbPrecip"

#ideas

#hurricane matthew
#cities: Miami, FL; Jacksonville, FL; Savannah, GA; Wilmington, NC; Charleston, SC
#source: https://water.usgs.gov/floods/events/2016/matthew/

#population -> accuracy?
#probprecip variable
unique(f[which(f$forecast_variable == "ProbPrecip"),]$predicted_value)
nrow(f[which(f$predicted_value=="M"),]) #80 M's on whole thing, not much imputation interest here

#unique(h$Max_TemperatureF)

#imputation
ts <- h[which(h$PrecipitationIn=="T"),] #11612 (about 10%)
nrow(ts)/nrow(h)
#source 1: http://www.stat.columbia.edu/~gelman/arm/missing.pdf
#source 2: https://www.omicsonline.org/open-access/a-comparison-of-six-methods-for-missing-data-imputation-2155-6180-1000224.php?aid=54590
vec <- rep(NA, length(unique(ts$AirPtCd)))
vec1 <-  rep(NA, length(unique(ts$AirPtCd)))
for(i in 1:length(unique(ts$AirPtCd))){
  vec[i] <- nrow(subset(ts, ts$AirPtCd == unique(ts$AirPtCd)[i]))
  vec1[i] <- nrow(subset(h, ts$AirPtCd == unique(ts$AirPtCd)[i]))
}

#T represents trace amounts of precipitation
#This cannot really be imputed.