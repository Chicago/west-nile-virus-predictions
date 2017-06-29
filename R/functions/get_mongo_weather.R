
get_mongo_weather <- function(gtdate, zipcode){
    require(data.table)
    require(httr)
    baseurl <- "https://webapps1.cityofchicago.org/windygridservice/rest/datasets/weather/"
    basequery <- 'query?q={"$and":[{"when.shardtime":{"$gt":%s}},{"where.zip_code":"%s"}]}'
    url <- sprintf(paste0(baseurl, basequery), 
                   as.integer(as.POSIXct(gtdate)) * 1000,
                   zipcode)
    timing <- system.time(response <- httr::GET(url))
    content <- httr::content(response)
    result <- rbindlist(lapply(content$features, `[[`, "properties"))
    attr(result, "timing") <- timing
    return(result)
}

if(FALSE){
    library(geneorama)
    # https://webapps1.cityofchicago.org/windygridservice/rest/datasets/weather/query?
    # q={"$and":[{"when.shardtime":{"$gt":1483250400000}},{"where.zip_code":"60602"}]}
    # "https://webapps1.cityofchicago.org/windygridservice/rest/datasets/weather/query"
    # '{"$and":[{"when.shardtime":{"$gt":1491022800000}},{"where.zip_code":"60602"}]}'
    
    ##--------------------------------------------------------------------------
    ## Weather for 60602
    ##--------------------------------------------------------------------------
    result <- get_mongo_weather("2017-01-01", 60602)
    attr(result, "timing")
    result[ , date := as.IDate(when.shardtime, "%m/%d/%Y")]
    rollup <- result[i = TRUE,
                     list(max_temp = max(what.currently.temperature),
                          mean_percip = mean(what.currently.precipIntensity),
                          sum_percip = mean(what.currently.precipIntensity)),
                     keyby = date]
    # clipper(rollup)
    
    ##--------------------------------------------------------------------------
    ## Weather for O'Hare (doesn't work)
    ##--------------------------------------------------------------------------
    result_ohare <- get_mongo_weather("2017-01-01", 60666)
    attr(result_ohare, "timing")
    
    ##--------------------------------------------------------------------------
    ## Weather for Midway
    ##--------------------------------------------------------------------------
    result_midway <- get_mongo_weather("2017-01-01", 60638)
    attr(result_midway, "timing")
    result_midway[ , date := as.IDate(when.shardtime, "%m/%d/%Y")]
    rollup_midway <- result_midway[i = TRUE,
                                   list(max_temp = max(what.currently.temperature),
                                        mean_percip = mean(what.currently.precipIntensity),
                                        sum_percip = mean(what.currently.precipIntensity)),
                                   keyby = date]
    # clipper(rollup_midway)
}




