
download_wnv <- function(
    inurl = "https://data.cityofchicago.org/api/views/jqe8-8r6s/rows.csv?accessType=DOWNLOAD"){
    dat <- fread(inurl)
    setnames(dat, tolower(colnames(dat)))
    setnames(dat, gsub(" ", "_", colnames(dat)))
    setnames(dat, "test_date", "date")
    dat <- dat[order(test_id)][]
    dat <- dat[ , date := as.IDate(date, "%m/%d/%Y")][]
    dat <- dat[ , result := result == "positive"][]
    dat <- dat[ , location := NULL][]
    setkey(dat, date, trap, species, result)

    return(dat)
}

if(FALSE){
    # source("R/functions/parse_dated_filename.R")
    # parsed_files <- parse_dated_filename(list.files("data/", full.names = T))
    # parsed_files <- as.data.table(parsed_files)
    # parsed_files
    #
    # outfile <- sprintf("data/wnv_dataportal_%s.csv", Sys.Date())
    # outfile
    #
    # basefilename <- parse_dated_filename(outfile)$filename_base
    # latest_date <- parsed_files[filename_base == basefilename, max(date)][1]
    # latest_date
    # if(latest_date != Sys.Date()){
    #     wnv <- download_wnv()
    #     write.table(x = wnv, file = outfile, sep = ",",
    #                 col.names = T, na = "", row.names = FALSE)
    # } else {
    #     fread(outfile)
    # }
    #
    # parsed_files <- parse_dated_filename(list.files("data/", full.names = T))
    # parsed_files <- as.data.table(parsed_files)
    # basefilename <- parse_dated_filename(outfile)$filename_base
    # latest_date <- parsed_files[filename_base == basefilename, max(date)][1]
    # latest_date

}
