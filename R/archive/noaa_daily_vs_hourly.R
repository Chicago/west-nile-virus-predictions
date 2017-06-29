
##------------------------------------------------------------------------------
## INITIALIZE AND GET DATA
##------------------------------------------------------------------------------
rm(list = ls())
library(geneorama)
sourceDir("R/functions")
noaa_files <- list.files("data/", pattern="noaa_values", full.names=TRUE)
noaa_daily <- open_noaa_files(noaa_files)
noaa_daily <- noaa_daily[ , list(AWND, PRCP, TMAX), keyby = date]

# USAF='725300'  #  2. USAF code ohare
# WBAN='94846'   #  3. WBAN code ohare

dat_ohare <- download_noaa_hourly(usaf="725300", wban="94846", year=2017)
noaa_ohare_hourly <- noaa_convert_hourly2daily(dat_ohare)
noaa_ohare_hourly

##------------------------------------------------------------------------------
## MERGE
##------------------------------------------------------------------------------
merged <- noaa_daily[noaa_ohare_hourly]
merged

##------------------------------------------------------------------------------
## TEMP COMPARISON
##------------------------------------------------------------------------------
merged[,plot(max_temp ~ date, col = "blue", type = "o")]
merged[,points(TMAX ~ date, col="orange", type="o")]
merged[,plot(max_temp ~ TMAX, xlab = "daily", ylab = "hourly aggregated")]
abline(0,1)

##------------------------------------------------------------------------------
## PRCP COMPARISON
##------------------------------------------------------------------------------
merged[,plot(PRCP ~ date, col = "orange", type = "o")]
merged[,points(precip ~ date, col = "blue", type = "o")]
# merged[,points(precip6 ~ date, col = "red", type = "o")]


merged[,plot(precip ~ PRCP)]
abline(0,1)

merged[,plot(log(precip+1) ~ log(PRCP+1) )]
abline(0,1)

## cumsum makes more sense
merged[,plot(cumsum(precip) ~ date, col = "blue", type = "o")]
merged[,points(cumsum(PRCP) ~ date, col = "orange", type = "o")]

##------------------------------------------------------------------------------
## WIND COMPARISON
##------------------------------------------------------------------------------
merged[,plot(wind_ave ~ date, col = "blue", type = "o")]
merged[,points(AWND ~ date, col = "orange", type = "o")]
merged[,plot(wind_ave ~ AWND, xlab = "daily", ylab = "hourly aggregated")]
abline(0,1)
