
##
## Loosely based on 42's answer in 
## http://stackoverflow.com/questions/13673894/suppress-nas-in-paste
##


paste2 <- function(..., sep="", collapse = NULL) {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    ret <- do.call(paste, c(L, list(sep=sep, collapse=collapse)) )
    ret
}


## Some checks
if(FALSE){
    vec1 <- c(letters[1:4], NA)
    vec2 <- c(1:4, "z", NA)
    paste0(vec1, "-", 1:4)
    paste2(vec1, "-", 1:4, sep="")
    paste2(vec1, "-", vec2, sep="")
    paste2(vec1, "-", vec2)
    paste2(vec1, "-", vec2, sep=", ")
    paste2(vec1, vec2, sep=", ")
    
    rm(vec1, vec2)
    
}

