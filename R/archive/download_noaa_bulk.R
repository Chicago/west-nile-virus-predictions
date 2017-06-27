
##
## Download NOAA data for each year
##
## Note, this function could be easily parameterized for start date, token, etc.
##
##


download_noaa_bulk <- function(){
    require(data.table)
    # source("R/functions/get_noaa_content.R")
    # source("R/functions/parse_dated_filename.R")

    TOKEN <- readLines("R/functions/weather_noaa_token.txt")
    BASE_URL <- "http://www.ncdc.noaa.gov/cdo-web/api/v2/data"
    param <- paste0("datasetid=GHCND&stationid=GHCND:USW00094846&",
                    "startdate=%s&enddate=%s")

    ## Populate param with sequence of start and end dates
    start_dates <- seq(as.IDate("2007-01-01"), as.IDate(Sys.Date()), "year")
    end_dates <- c(start_dates[-1] - 1, Sys.Date())
    params <- sprintf(param, start_dates, end_dates)

    ## Loop over params
    for(p in params){
        m <- regexec("[[:digit:]]{4}\\-[[:digit:]]{2}\\-[[:digit:]]{2}$", p)
        cur_date <- regmatches(p, m)[[1]]
        outfile_values <- paste0("data/noaa_values_", cur_date, ".csv")
        outfile_attr <- paste0("data/noaa_attributes_", cur_date, ".csv")
        if(!file.exists(outfile_values)){
            dat <- get_noaa_content(base_url = BASE_URL, param = p, token = TOKEN)
            dat <- rbindlist(dat)
            dat[ , date := as.IDate(date)]
            dat_values <- dcast(dat, formula = date ~ datatype, value.var = "value",
                                fun.aggregate = c, fill=NA)
            dat_attr <- dcast(dat, formula = date ~ datatype, value.var = "attributes",
                              fun.aggregate = c, fill=NA)
            write.table(x = dat_values, file = outfile_values, sep = ",",
                        col.names = T, na = "", row.names = FALSE)
            write.table(x = dat_attr, file = outfile_attr, sep = ",",
                        col.names = T, na = "", row.names = FALSE)
        }
    }
    invisible(TRUE)
}

