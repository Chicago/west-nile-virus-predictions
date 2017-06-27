

download_census_tracts <- function(infile = "data/censustracts.Rds"){

    ## Make sure that it downloads despite it's misguided efforts to cache
    options("tigris_refresh"=TRUE)

    ## Get census track data
    if(!file.exists(infile)){
        census_tracts <- tigris::tracts(state = 17, county = '031')
        saveRDS(census_tracts, infile)
    } else {
        census_tracts <- readRDS(infile)
    }

    return(census_tracts)
}
