

##------------------------------------------------------------------------------
## INITIALIZE / IMPORT DATA
##------------------------------------------------------------------------------

library(geneorama)
sourceDir("R/functions/", trace = FALSE)
loadinstall_libraries(c("fishmethods", "ggplot2", "labeling", "caret", "sp"))

refresh_noaa()
refresh_wnv_file()

noaa_files <- list.files("data/", pattern="noaa_values", full.names=TRUE)
noaa <- open_noaa_files(noaa_files)

# noaa_files <- list.files("data/", pattern="noaa_attr", full.names=TRUE)
# noaa_attrs <- open_noaa_files(noaa_files)

wnv_original <- open_latest_wnv_file()
id_table <- readRDS("data/10_calculate_idtable.Rds")

ward_map <- readRDS("R/maps/BoundariesWards.Rds")

##------------------------------------------------------------------------------
## SUPPLEMENT THE LOCATION DATA WITH ORACLE TABLE
##------------------------------------------------------------------------------

## Merge in trap id created in previous script
wnv <- merge(x = wnv_original[i = TRUE,
                              j = .SD, 
                              .SDcols = -c("latitude", "longitude", "trap_type")], 
             y = id_table[i = TRUE,
                          j = list(trap, 
                                   trap_type, 
                                   block, 
                                   id, 
                                   latitude = OLAT,  ## ORACLE LATITUDE
                                   longitude = OLON, ## ORACLE LONGITUDE
                                   X = OX,   ## ORACLE X (BASED ON STATE PLANE)
                                   Y = OY,   ## ORACLE Y (BASED ON STATE PLANE)
                                   census_block)], 
             by = c("trap", "block"), 
             sort = F)

##------------------------------------------------------------------------------
## FILTER / CLEAN UP SPECIES
##------------------------------------------------------------------------------

dcast(wnv, species~year(date), fun.aggregate = length, value.var = "result")
other_species <- c("CULEX ERRATICUS", "CULEX SALINARIUS",
                   "CULEX TARSALIS", "UNSPECIFIED CULEX")
wnv[ , spec := species]
wnv[species %in% other_species , spec := "other"]
wnv[species == "CULEX PIPIENS", spec := "pipiens"]
wnv[species == "CULEX PIPIENS/RESTUANS", spec := "pipiens_restauns"]
wnv[species == "CULEX RESTUANS", spec := "restuans"]
wnv[species == "CULEX TERRITANS", spec := "territans"]

# dcast(wnv, spec~year(date), fun.aggregate = length, value.var = "result")
# dcast(wnv[result==TRUE], species~year(date), fun.aggregate = length, value.var = "result")
# dcast(wnv[result==FALSE], species~year(date), fun.aggregate = length, value.var = "result")
# dcast(wnv, species~year(date), fun.aggregate = length, value.var = "result")

##------------------------------------------------------------------------------
## REGULARIZE DATES IN WNV DATA 
## "week" is a better measure of the date than the date field, because there
## are often errors with multiple measures in a single week
##------------------------------------------------------------------------------

## The main collection date has shifted over time from Tuesday, to Friday, and 
## by 2016 it seemed to be Thursday
dcast(wnv[ , .N, list(season_year, wday=wday(date))], 
      season_year ~ wday, value.var = "N")
setnames(wnv, "date", "date_orig")
wnv[ , date := as.IDate(round(date_orig, "year") + (week / 52) * 365) - 6]
setcolorder(wnv, c('season_year', 'week', 'test_id', 'block', 'trap', 'trap_type', 
                   'date_orig', 'number_of_mosquitoes', 'result', 'species', 
                   'id', 'date', 'latitude', 'longitude', 'X', 'Y', 
                   'census_block', 'spec'))
wnv[ , .N, keyby = list(date - date_orig)]

##------------------------------------------------------------------------------
## CONVERT WNV SHAPE FROM LONG TO WIDE
##------------------------------------------------------------------------------

dat <- dcast(wnv, 
             date + season_year + week + id + trap + latitude + longitude + 
                 X + Y + census_block + trap_type ~ spec + result,
             value.var = "number_of_mosquitoes",
             fun.aggregate = sum, fill = 0, na.rm = T)
setcolorder(dat, c('season_year', 'week', 'date', 'trap', 'id', 'trap_type', 
                   'latitude', 'longitude', 'X', 'Y', 'census_block',
                   'pipiens_restauns_TRUE', 'pipiens_restauns_FALSE', 
                   'pipiens_TRUE', 'pipiens_FALSE',
                   'restuans_TRUE', 'restuans_FALSE', 
                   'territans_TRUE', 'territans_FALSE',
                   'other_TRUE', 'other_FALSE'))

# ## Add in row count from original WNV data
# dat <- merge(dat, 
#              wnv[ , .N, list(date, week, id, trap, trap_type)],
#              by = c("date", "week", "id", "trap", "trap_type"))
# dat

# encoding_matrix <- get_encoding_list(dat)

NAsummary(dat)

##------------------------------------------------------------------------------
## GEOCODE WARD (ONLY NEEDED FOR MAPPING, NOT USED IN MODEL)
##------------------------------------------------------------------------------

## Manual process
sp::coordinates(dat) <- c("longitude", "latitude")
ward_map@proj4string
ward_map <- sp::spTransform(ward_map, sp::CRS("+proj=longlat +datum=WGS84"))
dat@proj4string <- ward_map@proj4string
system.time(geo <- sp::over(dat, ward_map))
dat <- as.data.table(dat)
dat[ , ward := as.integer(geo$ward)]

##------------------------------------------------------------------------------
## Geocoding process using chigeocodR
## The chigeocodR package is an internal, private pacakge that uses the city's
## internal geocoding service to geocode point locations to regions (such as
## ward, census blocks, or various districts). For now chigeocodR won't work 
## because some of the locations are just outside the city limits.  So, we 
## didn't switch from the methodology above which uses sp::over.
##------------------------------------------------------------------------------
# system.time(addrs <- dat[ , chigeocodR::reverseGeocode(lat = latitude, 
#                                                        lon = longitude)])
# system.time(wards <- chigeocodR::forwardGeocode(streetAddresses = addrs$address, 
#                                                 geoTypes = "Ward"))
# setnames(addrs, c("lat", "lon"), c("latitude", "longitude"))
# wards <- as.data.table(wards)
# dat <- merge(dat, addrs, c("latitude", "longitude"))
# dat <- merge(dat, wards, c("address"))
# setnames(dat, "geovalues.Ward", "ward")


##------------------------------------------------------------------------------
## ADD TRAP OBSERVATION COUNT FOR CREDIBILITY
## IN THE MODEL WE CAN EXCLUDE TRAPS WITH VERY LOW COUNTS
##------------------------------------------------------------------------------

## Calculate how many times we see a trap
## This is no longer used, but it was an initial attempt to estimate the 
## credibility of a trap.  Some traps only appear a handful of times, and 
## these probably contribute more noise than information to the model. 
dat[ , trap_obs_count := .N, id]

##------------------------------------------------------------------------------
## REMOVE SOME MISSING VARIABLES FROM NOAA DATA (FOR SIMPLICITY)
##------------------------------------------------------------------------------

# noaa[ , WDF5 := NULL]  ## Should impute later, this is wind speed
# noaa[ , WSF5 := NULL]  ## Should impute later, this is wind speed
noaa[ , WT01 := NULL]
noaa[ , WT02 := NULL]
noaa[ , WT03 := NULL]
noaa[ , WT08 := NULL]

NAsummary(noaa)

##------------------------------------------------------------------------------
## SUPPLEMENT ANY MISSING NOAA DAILY RECORDS WITH HOURLY DATA
## Often the most recent data in the weather file contains NA values, which 
## will cause the model to fail. This part of the code downloads the hourly
## weather data and supplements the daily values that are NA.
##------------------------------------------------------------------------------
## Subset NOAA data to dates that are within the range of the wnv data
noaa <- noaa[date <= max(wnv$date_orig)]
## Find dates with missing values
## Check all the NOAA variables used TMAX, AWND, and PRCP
## Limit check to current year. There are some missing values around Dec 31 
## sometimes, but these don't affect the model because there is no WNV then.
missing_dates <- noaa[i = year(date) == year(Sys.time()) & 
                          (is.na(TMAX) | is.na(AWND) | is.na(PRCP)),
                      j = date]
## Replace missing NOAA daily data with hourly aggregates
## Note that no regristration / token is needed to access the hourly FTP site
if(length(missing_dates) > 0){
    dat_ohare <- download_noaa_hourly(usaf="725300", wban="94846", year=year(Sys.time()))
    noaa_ohare_hourly <- noaa_convert_hourly2daily(dat_ohare)
    for(i in missing_dates){
        noaa[date==i, AWND := noaa_ohare_hourly[date == i, as.integer(wind_ave)]]
        noaa[date==i, TMAX := noaa_ohare_hourly[date == i, max_temp]]
        noaa[date==i, PRCP := noaa_ohare_hourly[date == i, precip]]
    }
}

##------------------------------------------------------------------------------
## ADD Y VALUES TO WNV DATA
##------------------------------------------------------------------------------

## Early on it wasn't clear which outcome would be the "y value" that we were
## going to predict, so several things were calculated below. These values are 
## still useful for reports, diagnostics, plots, and map construction.

dat$total_true <- apply(dat[ , grep("_TRUE", colnames(dat)), with =F], 1, sum)
dat$total_false <- apply(dat[ , grep("_FALSE", colnames(dat)), with =F], 1, sum)
dat$total_mosquitoes <- dat[ , total_true + total_false]

dat[ , pct_wnv := total_true / (total_true + total_false)]
dat[ , wnv := as.integer(0!=(total_true / (total_true + total_false)))]

##------------------------------------------------------------------------------
## CREATE VARIABLES OF LAGGED VALUES
## (BASED ON HECTOR'S PREVIOUS PYTHON WORK)
##------------------------------------------------------------------------------

setkey(dat, id, date, week)
NAsummary(dat)

## The shift function is essentially a lag operator.  See ?stats::lag
## Demo of the shift function:
# dat[ , date_prev1 := shift(as.character(date), -1), by = id]
# dat[ , list(id, date, date_prev1)]
# dat[ , date_prev1 := NULL, by = id]

##------------------------------------------
## CREATE LAGGED WNV TEST RESULT VALUES
##------------------------------------------
dat[ , wnvw1 := shift(wnv, -1), by = id]
dat[ , wnvw2 := shift(wnv, -2), by = id]
## Future WNV for forecast testing
dat[ , wnv_f1 := shift(wnv, 1), by = id]

season_prev_summary <- dat[i = TRUE,
                           j = list(date,
                                    wnv_ytd = shift(cumsum(wnv), -1)),
                           keyby = list(id, year = year(date))]
dat <- merge(dat,
             season_prev_summary[,.SD,.SDcols=-"year"],
             c("id", "date"))

##------------------------------------------
## LAGGED MOSQUITO COUNTS BY SPECIES
##------------------------------------------
## First create total
dat[ , culx := other_FALSE + other_TRUE + pipiens_FALSE + pipiens_TRUE +
         pipiens_restauns_FALSE + pipiens_restauns_TRUE + restuans_FALSE +
         restuans_TRUE + territans_FALSE + territans_TRUE]
dat[ , pip := pipiens_FALSE + pipiens_TRUE]
dat[ , res := restuans_FALSE + restuans_TRUE]
dat[ , pipres := pipiens_FALSE + pipiens_TRUE + restuans_FALSE + restuans_TRUE]
dat[ , other := other_FALSE + other_TRUE + territans_FALSE + territans_TRUE]

## Then shift the totals
dat[ , culx1 := shift(culx, -1), by = id]
dat[ , culx2 := shift(culx, -2), by = id]
dat[ , pip1 := shift(pip, -1), by = id]
dat[ , pip2 := shift(pip, -2), by = id]
dat[ , res1 := shift(res, -1), by = id]
dat[ , res2 := shift(res, -2), by = id]
dat[ , pipres1 := shift(pipres, -1), by = id]
dat[ , pipres2 := shift(pipres, -2), by = id]
dat[ , other1 := shift(other, -1), by = id]
dat[ , other2 := shift(other, -2), by = id]


##------------------------------------------
## HISTORICAL WEATHER DATA
##------------------------------------------

## Calculate previous week values, then join them to the data
## Use all possible dates for flexibility
dates <- unique(dat$date)
noaa
xx <- data.table(date = dates, start = dates - 8, end = dates - 1)
yy <- noaa[ , list(AWND, PRCP, SNOW, SNWD, TMAX, TMIN, WDF2, WSF2),
            keyby = list(start = date, end = date)]
jj <- foverlaps(xx, yy)
weather_summary <- jj[i = TRUE,
                      j = list(awnd = mean(AWND),
                               prcp = mean(PRCP),
                               snow = mean(SNOW),
                               snwd = mean(SNWD),
                               tmax = mean(TMAX),
                               tmin = mean(TMIN),
                               wdf2 = mean(WDF2),
                               wsf2 = mean(WSF2)),
                      keyby = list(date)]
weather_summary
rm(xx,yy,jj)

##------------------------------------------------------------------------------
## CHECK FOR NEAR ZERO VARIANCE COLUMNS AND LINEAR COMBOS
##------------------------------------------------------------------------------

## Use the caret package to identify columns with near zero variance and
## columns that are linear combinations of other columns.  These variables
## will not contribute to the model. 
caret::nearZeroVar(weather_summary)
weather_summary <- weather_summary[,.SD,.SDcols=-c("snow", "snwd")]
## findLinearCombos causes errors if dates are missing, and filtering causes
## warnings if dates are not missing. So, run this manually, but it doesn't 
## change unless you change data sources or add new fetaures. It's just a good 
## diagnostic to keep in mind. 
# caret::findLinearCombos(weather_summary[ , list(tmin, tmax, awnd, prcp, wdf2, wsf2)])
cor(weather_summary[ , list(tmin, tmax, awnd, prcp, wdf2, wsf2)])
dat <- merge(dat, weather_summary, "date")


##------------------------------------------------------------------------------
## Diagnostics / plots
##------------------------------------------------------------------------------
if(FALSE){
    msum <- dat[i = T,
                list(pos = sum(wnv), .N), 
                list(date = round(date, "month"), 
                     month = month(date))]
    msum[ , month:=as.factor(month)]
    msum
    msum <- melt(msum, id.vars = "month", measure = c("pos", "N"))
    msum <- msum[!(variable=="pos" & value == 0)]
    
    ## Dot plot then boxplot
    # ggplot(msum, aes(x=month, y = value, colour = variable)) + geom_point()
    # ggplot(msum, aes(month, value)) + geom_boxplot(aes(colour = variable))
    mmsum <- data.frame(msum[ , list(mean = mean(value)), list(month, variable)])
    ggplot(msum, aes(month, value)) + 
        geom_boxplot(aes(colour = variable), width = .5) +
        geom_line(aes(month, mean, colour = variable, group = variable), 
                  data= mmsum, size = 2) +
        geom_point(aes(month, mean, colour = variable, group = variable), 
                   data= mmsum, size = 2, colour = "black") +
        ggtitle(paste0("Citywide count of traps collected (BLUE) compared to\n",
                       "count of traps that were WNV positive (ORANGE)\n",
                       "2008 - 2016\n"))
}

##------------------------------------------------------------------------------
## SAVE RESULTS
##------------------------------------------------------------------------------
dat
saveRDS(dat, "data/21_full_wnv_data_aggregated.Rds")
cat("created 21_full_wnv_data_aggregated.Rds\n")


