

##==============================================================================
## INITIALIZE
##==============================================================================

source("R/32a_multilevel_model.R", local = TRUE)

library(geneorama)
library("ROracle")


##==============================================================================
## LOG INTO ORACLE (GET USER INFO FROM TNS FILE)
##==============================================================================

dbinfo_dev <- readLines("untracked/zdt_credentials_dev.txt")
dbinfo_prod <- readLines("untracked/zdt_credentials_prod.txt")
# dbinfo_user <- readLines("untracked/zdt_credentials.txt")

drv <- dbDriver("Oracle")
# system("echo $TNS_ADMIN")
# Sys.getenv("TNS_ADMIN")
# Sys.setenv(TNS_ADMIN = "/app/Oracle")

Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

connect_string_dev <- paste0(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", dbinfo_dev[1], ")(PORT=1521))",
    "(CONNECT_DATA=(SERVICE_NAME=", dbinfo_dev[2], ")))")
ch_dev <- ROracle::dbConnect(drv, username = dbinfo_dev[3], 
                             password = dbinfo_dev[4], 
                             dbname = connect_string_dev)

connect_string_prod <- paste0(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", dbinfo_prod[1], ")(PORT=1521))",
    "(CONNECT_DATA=(SERVICE_NAME=", dbinfo_prod[2], ")))")
ch_prod <- ROracle::dbConnect(drv, username = dbinfo_prod[3], 
                              password = dbinfo_prod[4], 
                              dbname = connect_string_prod)

##==============================================================================
## CREATE DATA FOR ORACLE
##==============================================================================

# run_date <- as.character(Sys.time())
run_date <- Sys.time()
run_date <- as.POSIXct(round(run_date, "secs"))

sql_idtable <- cbind(RUNDATE = run_date, idTable)
sql_xmat <- as.data.table(cbind(RUNDATE = run_date, xmat))
sql_dat <- as.data.table(cbind(RUNDATE = run_date, dat))
sql_ranef_id <- data.table(RUNDATE = run_date,
                           id = rownames(ranef(m3)$id),
                           Intercept = unname(ranef(m3)$id[,1]))
sql_ranef_week <- data.table(RUNDATE = run_date,
                             week = rownames(ranef(m3)$week),
                             Intercept = unname(ranef(m3)$week[,1]))

setnames(sql_idtable, colnames(sql_idtable), toupper(colnames(sql_idtable)))
setnames(sql_xmat, colnames(sql_xmat), toupper(colnames(sql_xmat)))
setnames(sql_dat, colnames(sql_dat), toupper(colnames(sql_dat)))
setnames(sql_ranef_id, colnames(sql_ranef_id), toupper(colnames(sql_ranef_id)))
setnames(sql_ranef_week, colnames(sql_ranef_week), toupper(colnames(sql_ranef_week)))

##==============================================================================
## ADD PREDICTIONS TO DAT
##==============================================================================
## Convert WEEK to integer for merge
sql_xmat[ , WEEK := as.integer(as.character(WEEK))]
sql_dat <- merge(sql_dat, 
                 sql_xmat[ , list(ID, GRP, WEEK, 
                                  M2,                  ## original glmer wnv model
                                  M3, M3_FORECAST,     ## updated wnvwnv model
                                  M3A, M3A_FORECAST,   ## wnvwnv model for no post labor day data
                                  SELECTED_FORECAST = M3A_FORECAST)],
                 by = c("ID", "GRP", "WEEK"),
                 all.x = TRUE)
# sql_dat[ , SELECTED_FORECAST := round(SELECTED_FORECAST * 100, 0)]
sql_dat[ , SELECTED_FORECAST := round(SELECTED_FORECAST, 3)]

##==============================================================================
## CHECK DATA FOR UNIQUE KEY
##==============================================================================

## Check to make sure that the key is unique by year, week, trap
stopifnot(nrow(sql_dat[ , .N, list(SEASON_YEAR, WEEK, TRAP)]) == nrow(dat))

##==============================================================================
## FIX DATES
##==============================================================================
setnames(sql_xmat, "DATE", "COLLECTION_DATE")
setnames(sql_dat, "DATE", "COLLECTION_DATE")
sql_xmat[, COLLECTION_DATE := as.POSIXct(COLLECTION_DATE)]
sql_dat[, COLLECTION_DATE := as.POSIXct(COLLECTION_DATE)]

##==============================================================================
## COPY TABLES TO ORACLE
##==============================================================================

####################
## sql_idtable    ##
####################
DEST_TABLE <- "ID_TABLE"
# str(dbReadTable(ch_dev, DEST_TABLE))
# str(dbReadTable(ch_prod, DEST_TABLE))
dbWriteTable(ch_dev, DEST_TABLE, sql_idtable, overwrite = TRUE)
dbWriteTable(ch_prod, DEST_TABLE, sql_idtable, overwrite = TRUE)

####################
## sql_xmat       ##
####################
DEST_TABLE <- "XMAT"
# str(dbReadTable(ch_dev, DEST_TABLE))
# str(dbReadTable(ch_prod, DEST_TABLE))
dbWriteTable(ch_dev, DEST_TABLE, sql_xmat, overwrite = TRUE)
dbWriteTable(ch_prod, DEST_TABLE, sql_xmat, overwrite = TRUE)

####################
## sql_dat       ##
####################
DEST_TABLE <- "DAT"
# str(dbReadTable(ch_dev, DEST_TABLE))
# str(dbReadTable(ch_prod, DEST_TABLE))
dbWriteTable(ch_dev, DEST_TABLE, sql_dat, overwrite = TRUE)
dbWriteTable(ch_prod, DEST_TABLE, sql_dat, overwrite = TRUE)

####################
## sql_ranef_id   ##
####################
DEST_TABLE <- "RANEF_ID"
# str(dbReadTable(ch_dev, DEST_TABLE))
# str(dbReadTable(ch_prod, DEST_TABLE))
dbWriteTable(ch_dev, DEST_TABLE, sql_ranef_id, overwrite = TRUE)
dbWriteTable(ch_prod, DEST_TABLE, sql_ranef_id, overwrite = TRUE)


####################
## sql_ranef_week ##
####################
DEST_TABLE <- "RANEF_WEEK"
# str(dbReadTable(ch_dev, DEST_TABLE))
# str(dbReadTable(ch_prod, DEST_TABLE))
dbWriteTable(ch_dev, DEST_TABLE, sql_ranef_week, overwrite = TRUE)
dbWriteTable(ch_prod, DEST_TABLE, sql_ranef_week, overwrite = TRUE)
