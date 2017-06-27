
##
## This function refreshes the wnv result data from the portal by
## downloading the most recent data.  It doesn't download
## a new file if you've already gotten for one today. 
##

open_latest_wnv_file <- function(echo = TRUE){
    #source("R/functions/parse_dated_filename.R")
    data_filenames <- list.files("data/", full.names = T)
    if(length(data_filenames) == 0){
        stop("no wnv file found")
    }
    parsed_files <- parse_dated_filename(data_filenames)
    parsed_files <- as.data.table(parsed_files)
    # outfile <- sprintf("data/wnv_dataportal_%s.csv", Sys.Date())
    # basefilename <- parse_dated_filename(outfile)$filename_base
    latest_date <- parsed_files[filename_base == "wnv_dataportal_", max(date)][1]
    latest_file <- parsed_files[filename_base == "wnv_dataportal_"][
        date == max(date), fullname]
    if(length(latest_date)==0){
        stop("no wnv file found")
    }
    if(echo){
        cat("opening wnv file as of", latest_date, "\n")
    }
    wnv <- fread(latest_file)
    wnv[ , date:=as.IDate(date)]
    return(wnv[])
}
