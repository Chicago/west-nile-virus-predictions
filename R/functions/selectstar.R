selectstar <- function(ch, tablename){
    q <- paste0("select * from  ", tablename)
    result <- as.data.table(sqlQuery(ch, q, stringsAsFactors = FALSE))
    return(result)
}
