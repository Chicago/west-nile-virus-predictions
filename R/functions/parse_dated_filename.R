
##
## This function is used to split out the "base" part of a file name
## and the "date" part of the name.  This is used to check for recent
## downloads / to maintain a file cache.
##

parse_dated_filename <- function(files){

    ## Split filenames into components: directory / file / extension
    m <- regexec("(([^+]+)+/)?((.+)\\.(.+$))", files)
    files_split <- regmatches(files, m)
    files_split[lapply(files_split, length) == 0] <- NA ## replace any NULLS with NA
    files_split <- do.call(rbind, files_split)

    ## Further split filename within the directory into components:
    ## file root name and extension
    filenames_full <- files_split[ , 4]
    date_pattern <- "(.+)([[:digit:]]{4}\\-[[:digit:]]{2}\\-[[:digit:]]{2})"
    m <- regexec(date_pattern, filenames_full)
    filenames_split <- regmatches(filenames_full, m)
    filenames_split[lapply(filenames_split, length) == 0] <- NA ## replace any NULLS with NA
    filenames_split <- do.call(rbind, filenames_split)
    ## If there are no regex results, the result will be a vector instead of a 
    ## matrix, so just make a matrix of the correct dimension
    if(all(is.na(filenames_split))){
        filenames_split <- matrix(NA_character_, nrow(files_split), 3)
    }

    ret <- data.frame(fullname = files_split[,1],
                      dir = files_split[,3],
                      filename_full = files_split[,4],
                      filename_base = filenames_split[ , 2],
                      date = filenames_split[ , 3],
                      ext = files_split[,6],
                      stringsAsFactors = FALSE)
    return(ret)
}


if(FALSE){
    rm(list=ls())

    files <- c("data/traps_portal_2016-09-30.Rds",
               "data/traps_portal/data_portal_2016-09-30.Rds",
               "data/traps_portal/data_portal2016-09-30.Rds",
               "data/traps_portal/2016-09-30.Rds",
               "data/traps_portal_2016-10-31.Rds",
               "data/traps_oracle_2016-08-24.Rds",
               "data/traps_oracle_2016-08-26.Rds",
               "data/traps_oracle_2016-09-30.Rds",
               "data/traps_oracle_2017-10-31.Rds",
               "data/traps.oracle_2017-10-31.Rds",
               "data/wnv_results_portal_2016-08-08.Rds",
               "data/wnv_results_portal_2016-08-09.Rds",
               "data/wnv_results_portal_2016-08-10.Rds",
               "data/wnv_results_portal_2016-08-11.Rds",
               "data/traps_new_data_source.Rds",
               "data/traps_portal.Rds",
               "data/traps_oracle.Rds",
               "data/traps_oraclez.Rds",
               "data/traps_.zoracle.Rds",
               "data/traps_results.Rds",
               "data/traps_results_portal.R")
    parse_dated_filename(files)
    parse_dated_filename(basename(files))
    parse_dated_filename(files[c(20,21)])
    parse_dated_filename(files[c(20)])
}


