get_noaa_content <- function(base_url = NULL, 
                             param_datasetid = "GHCND",
                             param_stationid = "GHCND:USW00094846",
                             start_date = NULL,
                             end_date = NULL,
                             LIMIT = 1000, 
                             token=NULL){
    result <- list()
    i <- 0
    while(length(result)==0 || length(result[[length(result)]]) >= LIMIT ){
        i <- i + 1
        # cat("httr get request number", i, "\n")
        param_limits <- paste0("limit=", sprintf("%i", LIMIT),
                               "&", "offset=", sprintf("%i", (i - 1) * LIMIT))
        url <- paste0(base_url, "?", 
                      "datasetid=", param_datasetid, "&", 
                      "stationid=", param_stationid, "&", 
                      "startdate=", start_date, "&", 
                      "enddate=", end_date, "&", 
                      param_limits)
        result[[i]] <- httr::GET(url, httr::add_headers(token=token))
        result[[i]] <- httr::content(result[[i]])
        if(length(result[[length(result)]]) == 0){
            stop(paste0("Nothing returned for the following request:\n", url))
        }
        result[[i]] <- result[[i]][[2]]
    }
    result <- Reduce(c, result)
    return(result)
}

if(FALSE){
    library(geneorama)
    library(data.table)
    source("R/functions/get_noaa_content.R")
    dat <- get_noaa_content(base_url = "http://www.ncdc.noaa.gov/cdo-web/api/v2/data",
                            param = paste0("datasetid=GHCND&stationid=GHCND:USW00094846&",
                                           "startdate=2007-01-01&enddate=2008-01-01"),
                            token = readLines("R/functions/weather_noaa_token.txt"))
    dat <- rbindlist(dat)
    dat[ , date := as.IDate(date)]
    dat_values <- dcast(dat, formula = date ~ datatype, value.var = "value",
                        fun.aggregate = c, fill=NA)
    dat_attr <- dcast(dat, formula = date ~ datatype, value.var = "attributes",
                      fun.aggregate = c, fill=NA)
    dat_values
    dat_attr
    dat_values[,plot(AWND~date)]
    dat_values[,plot(TMAX~date)]
    dat_values[,points(TMIN~date)]
}

