

labor_day <- function(y){
    
    fn <- function(y) {
        start <- as.IDate(sprintf("%0.3i-09-01", y))
        dates <- start + 0:6
        dates[which(weekdays(dates)=="Monday")]
    }
    as.IDate(Vectorize(fn)(y), origin = "1970-01-01")
}


if(FALSE){
    
    y <- 2016
    rm(y, dates, start)
    labor_day(2015)
    labor_day(c(2015, 2014, 2016))
}

