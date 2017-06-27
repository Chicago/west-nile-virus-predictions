
add_buffer <- function(dates, buff){
    dates <- as.character(dates)
    mindates <- as.IDate(sapply(split(dates, year(dates)), min))
    dates <- as.IDate(dates)
    additional_dates <- unlist(lapply(buff, `+`, mindates))
    additional_dates <- as.IDate(additional_dates, origin = "1970-01-01")
    ret <- sort(c(dates, additional_dates))
    return(ret)
}

calc_solar <- function(buff = c(-7, -14, -21, -28, -35)){
    # dates <- refresh_wnv_file()[ , unique(date)]
    dat <- refresh_wnv_file()[!is.na(longitude) , .N, list(date, latitude, longitude)]
    dates <- as.IDate(dat[,sort(unique(date))])
    dates <- add_buffer(dates, buff)

    avg_lon <- dat[ , sum(N * longitude) / sum(N)]
    avg_lat <- dat[ , sum(N * latitude) / sum(N)]
    date_times <- data.table(expand.grid(dates, 1:24, stringsAsFactors = FALSE))
    setnames(date_times, c("date", "h"))
    date_times[ , date := as.IDate(date)]
    date_times[ , lon := avg_lon]
    date_times[ , lat := avg_lat]

    date_times[ , y := year(date)]
    date_times[ , m := month(date)]
    date_times[ , d := mday(date)]

    solar <- date_times[ , astrocalc4r(day=d, month=m, year=y, hour=h,
                                       timezone=-5, lat=lat, lon=lon,
                                       seaorland = "continental"),
                         keyby =list(date, h)]
    return(solar)
}

interpolate_solar <- function(ss){
    # ss <-copy(solar_summary)
    years <- sort(unique(year(ss$date)))
    loess_models <- lapply(years, function(x){
        ss[year(date)==x, loess(par ~ as.integer(date))]
    })
    loess_pred_data <- lapply(years, function(x)
        ss[year(date)==x, list(date = min(date):max(date))])
    loess_preds <- lapply(1:length(loess_models), function(i){
        unname(predict(loess_models[[i]], loess_pred_data[[i]]))
    })
    ss
    result <- data.table(date = unname(unlist(loess_pred_data)),
                         pred = unlist(loess_preds))
    result[ , date := as.IDate(date, origin="1970-01-01")]
    result[]
    return(result)
}

if(FALSE) {
    rm(list=ls())
    library(geneorama)
    sourceDir("R/functions/")
    geneorama::loadinstall_libraries("fishmethods")
    solar <- calc_solar()
    solar
    solar_summary <- solar[i = TRUE,
                           j = list(par = mean(PAR),
                                    daylight = mean(daylight)),
                           keyby = date]
    solar_summary
    solar_summary_full <- interpolate_solar(solar_summary)
    solar_summary_full

    # solar_summary[ , plot(par~date)]
    # solar_summary[ , plot(daylight~date)]
    #
    #
    # solar_summary[year(date)==2016]
    # lom2016 <- solar_summary[year(date)==2016, loess(par ~ as.integer(date))]
    # plot(lom2016)
    # lod2016 <- solar_summary[year(date)==2016, list(date = min(date):max(date))]
    # lod2016
    # lod2016$par <- unname(predict(lom2016, lod2016))
    # lod2016[ , date := as.IDate(date, origin="1970-01-01")]
    #
    # ## Plot actual and predicted
    # plot(lod2016)
    # points(lom2016, pch=19)
    #
    # points(solar_summary_full, col="red")

}
