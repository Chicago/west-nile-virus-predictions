

download_oracle_traps_linux <- function(credential_file = "untracked/zdt_credentials_prod.txt"){
    require("ROracle")
    require("data.table")
    ##==============================================================================
    ## CONNECT TO ORACLE
    ##==============================================================================
    
    drv <- dbDriver("Oracle")
    dbinfo <- readLines(credential_file)
    connect_string <- paste0(
        "(DESCRIPTION=",
        "(ADDRESS=(PROTOCOL=tcp)(HOST=", dbinfo[1], ")(PORT=1521))",
        "(CONNECT_DATA=(SERVICE_NAME=", dbinfo[2], ")))")
    ch <- ROracle::dbConnect(drv, 
                             username = dbinfo[3], 
                             password = dbinfo[4], 
                             dbname = connect_string)
    assign("ch", ch, envir = .GlobalEnv)
    
    ##==============================================================================
    ## DOWNLOAD TRAP DATA
    ##==============================================================================
    traps <- dbReadTable(ch, "ZDT_TRAPS", stringsAsFactors = FALSE)
    traps <- as.data.table(traps)
    
    ##==============================================================================
    ## Close connection
    ##==============================================================================
    dbDisconnect(ch)
    rm(ch)
    
    return(traps)
}
