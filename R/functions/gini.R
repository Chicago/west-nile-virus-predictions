

gini <- function(y, yhat){
    dt <- data.table(y, yhat)
    ret <- dt[order(-yhat), sum( (cumsum(y) / sum(y)) - ((1:.N) / .N))] /
        dt[order(-y), sum( (cumsum(y) / sum(y)) - ((1:.N) / .N))]
    return(ret)    
}
