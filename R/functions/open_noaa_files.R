
open_noaa_files <- function(noaa_files, na_strings = c("", "NA")){

    ## don't warn while reading because the noaa attribute files trigger a
    ## warning in fread.  This is because there are so many empty values the
    ## automatically detected column classes are checking the "first 5, middle
    ## 5, and last 5" rows.
    ##
    ow <- options("warn")
    options(warn=-1)
    ## read in the list of files
    noaa_list <- lapply(noaa_files, fread, na.strings = na_strings)
    options(ow)

    ## get the column names, find unique column names,
    noaa_list_names <- lapply(noaa_list, colnames)
    all_names <- unique(unlist(noaa_list_names))
    all_in <- sapply(noaa_list_names, function(x) all_names %in% x)
    ## find column names which are common to all the files
    common_names <- all_names[which(apply(all_in, 1, all))]
    ## subset to common columns
    df_list <- lapply(noaa_list, function(x)as.data.frame(x)[,common_names])
    ## combine subset and return
    ret <- rbindlist(df_list)
    ret[ , date:=as.IDate(date)]
    return(ret)
}

