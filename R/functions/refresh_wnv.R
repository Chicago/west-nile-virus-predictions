
##
## This function refreshes the wnv result data from the portal by
## downloading the most recent data.  It doesn't download
## a new file if you've already gotten for one today. 
##

refresh_wnv <- function(){
    #source("R/functions/parse_dated_filename.R")
    parsed_files <- parse_dated_filename(list.files("data/", full.names = T))
    parsed_files <- as.data.table(parsed_files)
    outfile <- sprintf("data/wnv_dataportal_%s.csv", Sys.Date())
    basefilename <- parse_dated_filename(outfile)$filename_base
    

    candidate_files <- parsed_files[filename_base == basefilename, ]
    if(nrow(candidate_files) == 0){
        latest_date <- 0 ## No previous download
    } else {
        latest_date <- parsed_files[filename_base == basefilename, max(date)][1]
    }
    if(latest_date != Sys.Date()){
        wnv <- download_wnv()
        write.table(x = wnv, file = outfile, sep = ",",
                    col.names = T, na = "", row.names = FALSE)
    } else {
        wnv <- fread(outfile)
    }
    wnv[ , date:=as.IDate(date)]
    invisible(wnv)
}
