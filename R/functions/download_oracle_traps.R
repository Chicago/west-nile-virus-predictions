

download_oracle_traps <- function(credential_file = "untracked/zdt_credentials.txt"){
    require("RODBC")
    require("data.table")
    ##==============================================================================
    ## CONNECT TO ORACLE
    ##==============================================================================
    # dbinfo <- readLines("data/zdt_credentials.txt")
    dbinfo <- readLines(credential_file)
    ch <- odbcConnect(dbinfo[1], uid = dbinfo[2], pwd = dbinfo[3])
    
    ##==============================================================================
    ## DOWNLOAD TRAP DATA
    ##==============================================================================
    
    ## List tables & get row counts
    # tabs <- as.data.table(RODBC::sqlTables(ch))
    # tabs[ , .N, TABLE_SCHEM]
    # tabs <- tabs[TABLE_SCHEM %in% c("OPENDATA", "ZDT")]
    # tabs$TABLE_NAME
    # tab_cnt <- sapply(tabs[ , paste0(TABLE_SCHEM, ".", TABLE_NAME)],
    #                   function(x) sqlQuery(ch, paste0("select count (*) from  ", x),
    #                                        stringsAsFactors = FALSE))
    # tabs[ , row_count := unname(tab_cnt)]
    # tabs[row_count==0]
    
    
    ## Count rows
    sqlQuery(ch, "select count(*) from  ZDT.ZDT_TRAPS")
    
    ## Get traps
    traps <- sqlQuery(ch, "select * from  ZDT.ZDT_TRAPS", stringsAsFactors = FALSE)
    traps <- as.data.table(traps)
    
    ##==============================================================================
    ## Close connection
    ##==============================================================================
    odbcClose(ch)
    rm(ch)
    
    return(traps)
}
