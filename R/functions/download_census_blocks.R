

download_census_blocks <- function(infile = "data/censusblocks.Rds"){

    ## Make sure that it downloads despite it's misguided efforts to cache
    options("tigris_refresh"=TRUE)

    ## Get census track data
    if(!file.exists(infile)){
        census_blocks <- tigris::blocks(state = '17', county = '031')
        saveRDS(census_blocks, infile)
    } else {
        census_blocks <- readRDS(infile)
    }

    return(census_blocks)
}
