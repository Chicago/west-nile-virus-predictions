

calculate_confusion_values <- function(actual, expected, r, as.value = FALSE){
    res <-  expected > r
    true_pos <- res & actual
    true_neg <- !res & !actual
    false_pos <- res & !actual
    false_neg <- !res & actual
    
    if(as.value){
        result <- c(r = r,
                    true_pos = sum(true_pos),
                    true_neg = sum(true_neg),
                    false_neg = sum(false_neg),
                    false_pos = sum(false_pos))
    } else{
        result <- c(r = r,
                    true_pos = sum(true_pos) / length(res),
                    true_neg = sum(true_neg) / length(res),
                    false_neg = sum(false_neg) / length(res),
                    false_pos = sum(false_pos) / length(res))
    }
    
    return(result)
}


calculate_confusion_matrix <- function(y, yhat, r = seq(0,1,.025)) {
    conf_mat <- data.table(t(sapply(r,
                                    calculate_confusion_values,
                                    actual = y,
                                    expected = yhat,
                                    as.value = TRUE)))
    conf_mat <- conf_mat[ , sensitivity := true_pos /(true_pos + false_neg)]
    conf_mat <- conf_mat[ , specificity := true_neg /(true_neg + false_pos)]
    conf_mat <- conf_mat[ , recall := sensitivity]
    conf_mat <- conf_mat[ , precision := true_pos /(true_pos + false_pos)]
    conf_mat <- conf_mat[ , fmeasure := 2 * (recall * precision) / (recall + precision)]
    conf_mat <- conf_mat[]
    conf_mat
}
