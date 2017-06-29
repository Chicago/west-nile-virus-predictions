
# wget -N 'ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv'
# USAF='725340'  #  2. USAF code midway
# WBAN='14819'   #  3. WBAN code midway
# 
# USAF='725300'  #  2. USAF code ohare
# WBAN='94846'   #  3. WBAN code ohare

download_noaa_hourly <- function(usaf, wban, year){
    require(data.table) # You don't *need* to use data.table, but you'd be foolish not to.
    url <- sprintf("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/%s/%s-%s-%s.gz",
                   year, usaf, wban, year)
    dest <- file.path(tempdir(), basename(url))
    on.exit(unlink(dest)) ## delete temp file when exiting
    download.file(url, dest)
    result <- as.data.table(read.table(dest, na.strings = "-9999"))
    setnames(result, c("year", "month", "day", "hour", "air_temp_celsius", 
                       "dew_point_temp_celsius", "sea_level_pressure", 
                       "wind_direction", "wind_speed_km_hr", 
                       "sky_condition_total_coverage_code", 
                       "liquid_precipitation_mm_one_hour",
                       "liquid_precipitation_mm_six_hours"))
    return(result)
}


noaa_convert_hourly2daily <- function(dat){
    daily <- dat[i = TRUE, 
                 j = list(max_temp = round(max(air_temp_celsius, na.rm = TRUE)),
                          max_temp_f = round(max(air_temp_celsius, na.rm = TRUE) / 10 * 9 / 5 + 32),
                          wind_ave = mean(wind_speed_km_hr, na.rm = TRUE),
                          wind_max = max(wind_speed_km_hr, na.rm = TRUE),
                          precip = round(sum(pmax(liquid_precipitation_mm_one_hour, 0), na.rm = TRUE)),
                          precip6 = round(sum(liquid_precipitation_mm_six_hours, na.rm = TRUE))),
                 keyby = list(date = as.IDate(paste(year, month, day, sep="-")))]
    return(daily)
}

if(FALSE){
    dat_midway <- download_noaa_hourly(usaf="725340", wban="14819", year=2017)
    daily_midway <- noaa_convert_hourly2daily(dat_midway)
    daily_midway
    # clipper(daily)
}

