## Download NOAA data for each year


refresh_noaa <- function(purge_old = TRUE,
                         TOKEN = readLines("untracked/weather_noaa_token.txt")[1],
                         BASE_URL = "http://www.ncdc.noaa.gov/cdo-web/api/v2/data",
                         param_datasetid = "GHCND",
                         param_stationid = "GHCND:USW00094846"){
    
    require(data.table)
    ## also requires teh following functions:
    # source("R/functions/get_noaa_content.R")
    # source("R/functions/parse_dated_filename.R")
    
    ## Populate param_base with sequence of start and end dates
    start_dates <- seq(as.IDate("2007-01-01"), as.IDate(Sys.Date()), "year")
    end_dates <- c(start_dates[-1] - 1, Sys.Date())

    ## Output filenames for the values and attributes parts of the data
    filenames_values <- paste0("data/noaa_values_", end_dates, ".csv")
    filenames_attribues <- paste0("data/noaa_attributes_", end_dates, ".csv")
    
    if(!file.exists("data")) {
        dir.create("data")
    }
    
    ## Loop over dates
    for(i in 1:length(start_dates)){
        # m <- regexec("[[:digit:]]{4}\\-[[:digit:]]{2}\\-[[:digit:]]{2}$", p)
        # cur_date <- regmatches(p, m)[[1]]
        # cur_date <- end_dates[i]
        
        ## It should be sufficient to check for just the values file:
        if(!file.exists(filenames_values[i])){
            cat(paste0("Getting NOAA data for ", param_stationid, " ",
                       start_dates[i], " to ", end_dates[i]), "\n")
            
            dat <- try(get_noaa_content(base_url = BASE_URL, 
                                        param_datasetid = param_datasetid, 
                                        param_stationid = param_stationid,
                                        start_date = start_dates[i],
                                        end_date = end_dates[i],
                                        token = TOKEN),
                       silent = FALSE)
            if(!inherits(dat, "try-error")){
                dat <- rbindlist(dat)
                dat[ , date := as.IDate(date)]
                dat_values <- dcast(dat, formula = date ~ datatype, value.var = "value",
                                    fun.aggregate = c, fill=NA)
                dat_attr <- dcast(dat, formula = date ~ datatype, value.var = "attributes",
                                  fun.aggregate = c, fill=NA)
                write.table(x = dat_values, file = filenames_values[i], sep = ",",
                            col.names = T, na = "", row.names = FALSE)
                write.table(x = dat_attr, file = filenames_attribues[i], sep = ",",
                            col.names = T, na = "", row.names = FALSE)
            } else {
                warning(paste0("Something went wrong with the request for ",
                               start_dates[i], " to ", end_dates[i]), 
                        " processing other dates now.")
            }
            
        }
    }
    if(purge_old){
        current_value_files <- list.files("data", pattern = "noaa_values_.+\\.csv$", full.names = TRUE)
        current_attr_files <- list.files("data", pattern = "noaa_attributes_.+\\.csv$", full.names = TRUE)
        cur_files <- c(current_value_files, current_attr_files)
        expected_files <- c(filenames_values, filenames_attribues)
        files_to_purge <- cur_files[!cur_files %in% expected_files]
        unlink(files_to_purge)
    }
}
