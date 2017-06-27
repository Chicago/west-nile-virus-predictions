##
## gene leynes 2016-10-28
##
## Label encoder based on
## http://stackoverflow.com/questions/38620424/label-encoder-functionality-in-r
##

get_encoding_list <- function(df, plug_missing = TRUE, missing_string = ""){
    df <- as.data.frame(df)
    cols <- which(sapply(df, is.character) | sapply(df, is.factor))
    if(!all(names(cols) == unique(names(cols)))) {
        stop("please ensure that you are using unique column names")
    }
    lol <- list() ## list of levels; lol
    for (i in cols){
        if(plug_missing){
            if (is.factor(df[,i])){
                df[,i] <- factor(df[,i], levels=c(levels(df[,i]), missing_string))
            }
            df[is.na(df[,i]), i] <- missing_string
        }
        lol[[colnames(df)[i]]] <- unique(df[,i])
    }
    return(lol)
}

encode <- function(df, lol, missing_string = "") {
    cols <- names(lol)
    for (i in cols) {
        if (is.factor(df[ , i])){
            df[ , i] <- factor(df[ , i], levels=c(levels(df[,i]), missing_string))
        }
        df[is.na(df[ , i]), i] <- missing_string
        if (!is.null(levels)){
            df[ , i] <- as.numeric(factor(df[ , i], levels = lol[[i]]))
        }
    }
    return(df)
}

if(FALSE){
    # as strings
    dat1 <- data.frame(a_fact = c('Red','Blue','Blue',NA,'Green'),
                       a_int = c(1,2,3,4,5),
                       a_str = c('a','b','c','a','v'),
                       stringsAsFactors = FALSE)
    dat2 <- data.frame(a_fact = c('Red','Blue','Blue',NA,'Green'),
                       a_int = c(1,2,3,4,5),
                       a_str = c('a','b','c','a','v'),
                       stringsAsFactors = TRUE)
    new_dat1 <- data.frame(a_fact=c('Green','purple', 'Blue'),
                           a_int=c(1,2, 3),
                           a_str=c('z', 'a', NA),
                           stringsAsFactors=FALSE)
    new_dat2 <- data.frame(a_fact=c('Green','purple', 'Blue'),
                           a_int=c(1,2, 3),
                           a_str=c('z', 'a', NA),
                           stringsAsFactors=TRUE)

    enc1 <- encoding_matrix(dat1)
    enc2 <- encoding_matrix(dat2)

    encode(new_dat1, enc1)
    #   a_fact a_int a_str
    # 1      4     1    NA
    # 2     NA     2     1
    # 3      2     3    NA

    encode(new_dat1, enc2)
    #   a_fact a_int a_str
    # 1      4     1    NA
    # 2     NA     2     1
    # 3      2     3    NA

    encode(new_dat2, enc1)
    #   a_fact a_int a_str
    # 1      4     1    NA
    # 2     NA     2     1
    # 3      2     3    NA

    encode(new_dat2, enc2)
    #   a_fact a_int a_str
    # 1      4     1    NA
    # 2     NA     2     1
    # 3      2     3    NA


}

