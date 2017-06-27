

## LOAD DATA.TABLE PACKAGE AND FUNCTIONS
library(data.table)
geneorama::sourceDir("R/functions/")

## CURRENT FILENAME FOR EMAIL
# frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
# curfile <- frame_files[[length(frame_files)]] ## Works, unless called from cron job
thisfile <- file.path(getwd(), "run_all.R")
url <- "https://data.cityofchicago.org/api/views/jqe8-8r6s/rows.csv?accessType=DOWNLOAD"


cat("************************************************\n")
cat("** REFRESHING WNV FILE                        **\n")
cat("************************************************\n")
res <- try({
    refresh_wnv()
    wnv <- open_latest_wnv_file()
    cur_digest <- digest::digest(wnv)
    if(file.exists("data/wnv_digest.txt")){
        prev_digest <- readLines("data/wnv_digest.txt")
    } else {
        prev_digest <- ""
    }
}, 
silent = TRUE)

## COMPARE HASH VALUES
cat("Current digest value: ", cur_digest, "\n")
cat("Previous digest value: ", prev_digest, "\n")

if(cur_digest == prev_digest) {
    cat("Nothing to do. No change in WNV portal data\n")
    msg <- paste0("'Email from ", thisfile, "\n",
                  "No change in ", url, "\n",
                  "Predictions not updated'")
    if(inherits(res, "try-error")){
        subj <- "'WNV PREDICTIONS - FAILURE - NO UPDATE'"
    }else {
        subj <- "'WNV PREDICTIONS - SUCCESS - NO UPDATE'"
    }
    cmd <- paste("echo", msg, "| mail -s", subj, " %s")
} else {
    msg <- paste0("'Email from ", thisfile, "\n",
                  "Data changed on ", url, "\n",
                  "WNV Prediction update triggered\n")
    subj <- "'WNV PREDICTIONS - SUCCESS - UPDATED'"
    
    writeLines(cur_digest, "data/wnv_digest.txt")
    
    cat("************************************************\n")
    cat("** CALCULATE ID TABLE                         **\n")
    cat("************************************************\n")
    res <- try(source("R/10_calculate_idtable.R"), silent = TRUE)
    # Append success or failure statement to email message:
    if(inherits(res, "try-error")){ 
        msg <- paste(msg, "FAIL - R/10_calculate_idtable.R", sep = "\n")
        subj <- "'WNV PREDICTIONS - FAIL - UPDATED'"
    } else {
        msg <- paste(msg, "SUCCESS - R/10_calculate_idtable.R", sep = "\n")
    }
    rm(res)

    cat("************************************************\n")
    cat("** CREATE FEATURES                            **\n")
    cat("************************************************\n")
    res <- try(source("R/21_create_features.R"), silent = TRUE)
    # Append success or failure statement to email message:
    if(inherits(res, "try-error")){
        msg <- paste(msg, "FAIL - R/21_create_features.R", sep = "\n")
        subj <- "'WNV PREDICTIONS - FAILURE - UPDATED'"
    } else {
        msg <- paste(msg, "SUCCESS - R/21_create_features.R", sep = "\n")
    }
    rm(res)
    
    cat("************************************************\n")
    cat("** RUN MODEL                                  **\n")
    cat("************************************************\n")
    res <- try(source("R/40_upload_predictions_ROracle.R"), silent = TRUE)
    # Append success or failure statement to email message:
    if(inherits(res, "try-error")){
        msg <- paste(msg, "FAIL - R/40_upload_predictions_ROracle.R'", sep = "\n")
        subj <- "'WNV PREDICTIONS - FAIL - UPDATED'"
    } else {
        msg <- paste(msg, "SUCCESS - R/40_upload_predictions_ROracle.R'", sep = "\n")
    }
    rm(res)
    
    # cat(msg)
    cmd <- paste("echo", msg, "| mail -s", subj, " %s")
}

system(sprintf(cmd, "gene.leynes@cityofchicago.org"))
system(sprintf(cmd, "tom.schenk@cityofchicago.org"))
system(sprintf(cmd, "nicholas.lucius2@cityofchicago.org"))

cat("************************************************\n")
cat("** FINISHED                                   **\n")
cat("************************************************\n")


