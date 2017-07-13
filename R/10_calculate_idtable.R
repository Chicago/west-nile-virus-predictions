

## GWL 2017-07-13
## The main purposes of this script are to
##     1. Supplement the location data that is missing from the portal. 
##        The location on the data portal is actually a "fuzzed" location that
##        is only accurate to the nearest block. The source data is fuzzed
##        before it hits the portal. However, a few traps are located outside
##        just outside the City of Chicago's city limits and these traps
##        are missing their location data on the portal because the fuzzing
##        algorithm is only available for city addresses. So the precise
##        location is added to the data portal data. 
##     2. Uniquely identify each trap based on location. Most of the traps
##        never move, but a few have changed locations. This script adds a new
##        id that acts as a unique identifier for each location.  This id is 
##        used an intecept in the model, because the placement (or other 
##        circumstances) of the trap cause it to have a unique bias.
##     3. Fix / Correct addresses.  A few addresses are inconsistently entered
##        so these are corrected so that each address is unique and consistent.
##        This relies on reading in the address correction list, which isn't 
##        in the repository because it contains a few of the exact addresses of 
##        the traps instead of the fuzzed locations.  The address correction 
##        list looks like this (the 9's are fake numbers)
##
##             search_address,	replace_address
##             1199 W ROOSEVELT,	1199 W ROOSEVELT RD
##             9199 W HIGGINS,	9199 W HIGGINS RD
##             1199 S CALIFORNIA,	1199 S CALIFORNIA AVE
##             499 W 127TH,	499 W 127TH
##         
##        The left column is a search pattern, and the right is the 
##        replacement. For example "1199 S CALIFORNIA STREET" and 
##        "1199 S CALIFORNIA ST" would be replaced with "1199 S CALIFORNIA AVE"
##        because a greedy search is used.
##     4. Other small modifications.  See comments below for more details / 
##        actions, some comments are inside functions as well.
##        


##------------------------------------------------------------------------------
## INITIALIZE
##------------------------------------------------------------------------------

library(geneorama)
sourceDir("R/functions/", trace = FALSE)
geneorama::loadinstall_libraries(c("sp", "rgdal"))

##------------------------------------------------------------------------------
## GET DATA
##------------------------------------------------------------------------------
refresh_wnv_file()
wnv <- open_latest_wnv_file()

if(Sys.info()['sysname']== "Linux"){
    ## Linux / ROracle
    geneorama::loadinstall_libraries("ROracle")
    oracle_traps <- download_oracle_traps_linux(credential_file = "untracked/zdt_credentials_prod.txt")
} else {
    ## Windows / RODBC
    geneorama::loadinstall_libraries("RODBC")
    oracle_traps <- download_oracle_traps(credential_file = "untracked/zdt_credentials.txt")
}

##------------------------------------------------------------------------------
## PROCESS ORACLE DATA
##------------------------------------------------------------------------------

## Filter out bad records, simplify & rename columns, and combine a few records
address_fix_list <- fread("untracked/oracle_traps_address_repair_list.csv")
oracle_traps <- filter_oracle_trap_xy(oracle_traps, address_fix_list)

## Add in complete latitude / longitude
oracle_latlon <- oracle_traps[ , stateplane2latlon(OX, OY)]
oracle_traps[ , OLAT := oracle_latlon$latitude]
oracle_traps[ , OLON := oracle_latlon$longitude]
rm(oracle_latlon)

##------------------------------------------------------------------------------
## PROCESS DATA PORTAL DATA
##------------------------------------------------------------------------------

## Substitute GRAVID for CDC to match oracle 
wnv <- wnv[ , trap_type := gsub("CDC", "GRAVID", trap_type)]

wnv_xy <- latlon2stateplane(wnv$latitude, wnv$longitude)
wnv[ , X := round(wnv_xy$x)]
wnv[ , Y := round(wnv_xy$y)]
rm(wnv_xy)

##------------------------------------------------------------------------------
## MERGE ORACLE TRAP DATA INTO DATA PORTAL DATA
##------------------------------------------------------------------------------

oracle_traps[ , season_year := as.integer(season_year)]

## Oracle data has detail for each year
wnv <- merge(wnv,  oracle_traps, by=c("trap", "season_year", "trap_type"))

##------------------------------------------------------------------------------
## CALCULATE UNIQUE ID BASED ON TRAP, BLOCK, LAT/LON, AND TYPE
##------------------------------------------------------------------------------
id_table <- wnv[ , .N, keyby = list(trap, 
                                    block, 
                                    oracle_address = address,
                                    latitude, longitude,  
                                    X = round(X), 
                                    Y = round(Y), 
                                    OLAT, 
                                    OLON, 
                                    OX = round(OX), 
                                    OY = round(OY), 
                                    census_block,
                                    trap_type,
                                    distance_difference = sqrt((X-OX)^2 + (Y-OY)^2))]
## Remove N, it's not needed
id_table[ , N := NULL]

## Add in id, which is in the format of "id###"
id_table[ , id := sprintf("id%0.3i", 1:nrow(id_table))]

##------------------------------------------------------------------------------
## SAVE RESULTS AND ANNOUNCE FINISH FOR LOG
##------------------------------------------------------------------------------
saveRDS(id_table, "data/10_calculate_idtable.Rds")

cat("file data/10_calculate_idtable.Rds has been created \n")
