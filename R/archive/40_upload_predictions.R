

##==============================================================================
## INITIALIZE
##==============================================================================

source("R/32a_multilevel_model.R", local = TRUE)

library(geneorama)
loadinstall_libraries(c("RODBC"))


##==============================================================================
## LOG INTO ORACLE (GET USER INFO FROM TNS FILE)
##==============================================================================

# dbinfo <- readLines("R/zdt_credentials_dev.txt")
dbinfo <- readLines("R/zdt_credentials_prod.txt")
# dbinfo <- readLines("R/zdt_credentials.txt")

ch <- odbcConnect(dbinfo[1], uid = dbinfo[2], pwd = dbinfo[3], interpretDot = FALSE)

##==============================================================================
## GET TABLE NAMES
##==============================================================================

## Manually drop table if it exists
tabs <- data.table(sqlTables(ch))
tabs[ , .N, TABLE_SCHEM]
tabs[TABLE_SCHEM == "WNV_MODEL"]
tabs[TABLE_SCHEM == "OPENDATA"]
# tabs[grep("idtable", TABLE_NAME, ignore.case=TRUE)]

lll()

run_date <- as.character(Sys.time())


##==============================================================================
## CREATE DATA FOR ORACLE
##==============================================================================

sql_idtable <- cbind(RUNDATE = run_date, idTable)
sql_xmat <- as.data.table(cbind(RUNDATE = run_date, xmat))
sql_dat <- as.data.table(cbind(RUNDATE = run_date, dat))
sql_ranef_id <- data.table(RUNDATE = run_date,
                           id = rownames(ranef(m3)$id),
                           Intercept = unname(ranef(m3)$id[,1]))
sql_ranef_week <- data.table(RUNDATE = run_date,
                             week = rownames(ranef(m3)$week),
                             Intercept = unname(ranef(m3)$week[,1]))

##==============================================================================
## CHECK DATA FOR UNIQUE KEY
##==============================================================================

## Check to make sure that the key is unique by year, week, trap
stopifnot(nrow(sql_dat[ , .N, list(season_year, week, trap)]) == nrow(dat))

##==============================================================================
## FIX DATES
##==============================================================================
sql_xmat[, date := as.character(date)]
sql_dat[, date := as.character(date)]

##==============================================================================
## CODE TO DELETE TABLES IF NEEDED TO START FROM SCRATCH
## THIS IS NOT RUN & COMMENTED OUT
##==============================================================================
# sqlDrop(channel = ch, sqtable = "ID_TABLE")
# sqlDrop(channel = ch, sqtable = "XMAT")
# sqlDrop(channel = ch, sqtable = "DAT")
# sqlDrop(channel = ch, sqtable = "RANEF_ID")
# sqlDrop(channel = ch, sqtable = "RANEF_WEEK")

##==============================================================================
## COPY TABLES TO ORACLE
##==============================================================================

## Just set them all to replace
APPEND_TO_DEST <- FALSE

####################
## sql_idtable    ##
####################
DEST_TABLE <- "ID_TABLE"
if(nrow(sqlTables(ch, schema = "WNV_MODEL", tableName = DEST_TABLE))) {
    cat("dropping", DEST_TABLE, "\n")
    sqlDrop(ch, DEST_TABLE)
}
sqlSave(channel = ch, 
        dat = sql_idtable,
        tablename = DEST_TABLE,
        rownames = FALSE,
        append = APPEND_TO_DEST)

####################
## sql_xmat       ##
####################
DEST_TABLE <- "XMAT"
if(nrow(sqlTables(ch, schema = "WNV_MODEL", tableName = DEST_TABLE))) {
    cat("dropping", DEST_TABLE, "\n")
    sqlDrop(ch, DEST_TABLE)
}
sqlSave(channel = ch, 
        dat = sql_xmat,
        tablename = DEST_TABLE,
        rownames = FALSE,
        append = APPEND_TO_DEST)

####################
## sql_dat       ##
####################
DEST_TABLE <- "DAT"
if(nrow(sqlTables(ch, schema = "WNV_MODEL", tableName = DEST_TABLE))) {
    cat("dropping", DEST_TABLE, "\n")
    sqlDrop(ch, DEST_TABLE)
}
sqlSave(channel = ch, 
        dat = sql_dat,
        tablename = DEST_TABLE,
        rownames = FALSE,
        append = APPEND_TO_DEST)

####################
## sql_ranef_id   ##
####################
DEST_TABLE <- "RANEF_ID"
if(nrow(sqlTables(ch, schema = "WNV_MODEL", tableName = DEST_TABLE))) {
    cat("dropping", DEST_TABLE, "\n")
    sqlDrop(ch, DEST_TABLE)
}
sqlSave(channel = ch, 
        dat = sql_ranef_id,
        tablename = DEST_TABLE,
        rownames = FALSE,
        append = APPEND_TO_DEST)

####################
## sql_ranef_week ##
####################
DEST_TABLE <- "RANEF_WEEK"
if(nrow(sqlTables(ch, schema = "WNV_MODEL", tableName = DEST_TABLE))) {
    cat("dropping", DEST_TABLE, "\n")
    sqlDrop(ch, DEST_TABLE)
}
sqlSave(channel = ch, 
        dat = sql_ranef_week,
        tablename = DEST_TABLE,
        rownames = FALSE,
        append = APPEND_TO_DEST)


##==============================================================================
## CODE TO SELECT TABLES FROM DATABASE TO CHECK
## THIS IS NOT RUN & COMMENTED OUT
##==============================================================================
# sqlQuery(ch, "select * from WNV_MODEL.ID_TABLE")
# sqlQuery(ch, "select * from WNV_MODEL.XMAT")
# sqlQuery(ch, "select * from WNV_MODEL.DAT")
# sqlQuery(ch, "select * from WNV_MODEL.RANEF_ID")
# sqlQuery(ch, "select * from WNV_MODEL.RANEF_WEEK")

