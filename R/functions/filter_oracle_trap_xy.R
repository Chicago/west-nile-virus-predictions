
##------------------------------------------------------------------------------
## Create a clean table of Oracle XY coordinates that can be merged by
## trap / season_year.
## The resulting columns will be called OX and OY for "ORACLE X" and "ORACLE Y"
##------------------------------------------------------------------------------

filter_oracle_trap_xy <- function(traps, address_fix_list){
    
    ## Create a copy of traps so that the original data isn't modified in place
    oracle_xy <- copy(traps)
    
    ## Standardize the address differences
    for(i in 1:nrow(address_fix_list)){
        s <- address_fix_list$search_address[i]
        r <- address_fix_list$replace_address[i]
        oracle_xy <- oracle_xy[grep(s, TRAP_FULL_ADDRESS), TRAP_FULL_ADDRESS := r]
    }
    
    ## Replace CDC with GRAVID for trap types
    oracle_xy <- oracle_xy[ , TRAP_TYPE_CD := gsub("CDC","GRAVID", TRAP_TYPE_CD)]
    
    ## Remove leading and trailing whitespace from address
    oracle_xy <- oracle_xy[ , TRAP_FULL_ADDRESS := gsub("^ +| +$", "", TRAP_FULL_ADDRESS)]
    
    ## Subset, aggregate, and rename, also
    ## Round X / Y to nearest foot
    oracle_xy <- oracle_xy[i = TRUE,
                           j = list(.N),
                           keyby = list(trap = TRAP_NAME,
                                        season_year = SEASON_YEAR,
                                        trap_type = TRAP_TYPE_CD,
                                        address = TRAP_FULL_ADDRESS,
                                        census_block = CT_CENSUS_BLOCK_FULL,
                                        OX = round(TRAP_COORD_X),
                                        OY = round(TRAP_COORD_Y))]
    oracle_xy <- oracle_xy[ , .SD, .SDcols=-"N"]
    
    ################################################################################
    ## There are two locations in 2010 for trap T035, but there are no observations
    ## in the first location. Remove the first location from the oracle reference
    ## in order to avoid duplicates in the merge.
    ################################################################################
    # oracle_xy[trap=="T035"& season_year==2010]
    oracle_xy <- oracle_xy[!(trap=="T035" & OX==1162883 & season_year==2010)]

    ################################################################################
    ## There are two locations in 2010 for this T076, but the trap only moved a few
    ## feet, so just use either location.
    ## Remove the first location from the oracle reference to avoid duplicates in
    ## the merge.  Also, note that it's tricky to remove because the location has a
    ## fractional amount that doesn't print by default, hence the "trunc" below.
    ################################################################################
    # browser()
    # traps[TRAP_NAME=="T076"& SEASON_YEAR==2010 ]
    # oracle_xy[(trap=="T076" & season_year==2010)]
    # oracle_xy[(trap=="T076" & season_year==2010), OX][1] - 1179914
    oracle_xy <- oracle_xy[!(trap=="T076" & season_year==2010 & trunc(OX)==1179914)]
    
    return(oracle_xy)
}

