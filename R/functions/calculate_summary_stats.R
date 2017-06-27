

calculate_summary_stats <- function(results, kappa = FALSE, yearly=TRUE, data=FALSE){
    require(ROCR)
    require(pROC)
    require(e1071)
    
    # ##--------------------------------------------------------------------------
    # ## AUC
    # ##--------------------------------------------------------------------------
    # pred <- ROCR::prediction(yhat, y)
    # auc <- performance(pred, measure = "auc")@y.values[[1]]
    # roc <- pROC::roc(y, yhat, algorithm = 2)$auc[1]
    # # auc * 2 - 1
    # # gini
    # 
    # ##--------------------------------------------------------------------------
    # ## KAPPA
    # ##--------------------------------------------------------------------------
    # 
    # kappa <- e1071::classAgreement(table(y,yhat))[["kappa"]]
    # 
    # ### NICE METRIC SIMILAR TO KAPPA:
    # # plot(dt[ , sum(y==TRUE)/.N, list(yhat)])
    # 
    # #plot(performance(pred, "tpr", "fpr"))
    # # plot(performance(pred, measure = "phi"))
    # # plot(performance(pred, measure = "f"))
    
    ## Extract predictions
    preds <- lapply(results, function(l) lapply(l$cv, function(x) x$yhat_test))
    preds <- sapply(preds, unlist, use.names = F)
    
    ## Extract y values
    ymat_test <- lapply(results, function(l) lapply(l$cv, function(x) x$ymat_test))
    ymat_test <- sapply(ymat_test, unlist, use.names = F)
    
    ## Use the size of the cv results to calculate the "group"
    grp <- lapply(results[[1]]$cv, function(x) x$ymat_test)
    grp <- rep(names(grp), sapply(grp, length))
    
    ## Calculate abs error / rmse
    err <- (preds - ymat_test)
    errsqrd <- (err) ^ 2
    rmse_mean <- sqrt(apply(errsqrd, 2, mean))
    rmse_yearly <- data.table(t(apply(errsqrd, 2, function(col) 
        sapply(split(col, grp), function(e) sqrt(mean(e))))))
    
    ## AUC and ROC based on predictions
    metrics <- lapply(results, function(l) lapply(l$cv, function(x) 
        calculate_metrics(x$ymat_test, x$yhat_test)))
    auc_yearly <- data.table(t(sapply(metrics, function(l) sapply(l, `[[`, "auc"))))
    roc_yearly <- data.table(t(sapply(metrics, function(l) sapply(l, `[[`, "roc"))))
    kappa_yearly <- data.table(t(sapply(metrics, function(l) sapply(l, `[[`, "kappa"))))
    
    ## Overall results
    auc_mean <- apply(auc_yearly, 1, mean)
    roc_mean <- apply(roc_yearly, 1, mean)
    kappa_mean <- apply(kappa_yearly, 1, mean)
    
    
    setnames(auc_yearly, colnames(auc_yearly), paste0("auc_", colnames(auc_yearly)))
    setnames(roc_yearly, colnames(roc_yearly), paste0("roc_", colnames(roc_yearly)))
    setnames(rmse_yearly, colnames(rmse_yearly), paste0("rmse_", colnames(rmse_yearly)))
    setnames(kappa_yearly, colnames(kappa_yearly), paste0("kappa_", colnames(kappa_yearly)))
    
    grid <- t(sapply(results, `[[`, "param"))
    grid <- as.data.table(apply(grid, 2, unlist))
    result_summary <- data.table(grid)
    result_summary <- data.table(result_summary, auc_mean)
    result_summary <- data.table(result_summary, roc_mean)
    result_summary <- data.table(result_summary, rmse_mean)
    if(kappa) result_summary <- data.table(result_summary, kappa_mean)
    if (yearly) result_summary <- data.table(result_summary, auc_yearly)
    if (yearly) result_summary <- data.table(result_summary, roc_yearly)
    if (yearly) result_summary <- data.table(result_summary, rmse_yearly)
    if (yearly) if(kappa) result_summary <- data.table(result_summary, kappa_yearly)
    
    setnames(result_summary, colnames(result_summary), 
             gsub("_year", "_", colnames(result_summary)))
    
    if(data){
        ret <- list(result_summary = result_summary,
                    preds = preds,
                    ymat_test = ymat_test)
        return(ret)
    } else {
        return(result_summary)
    }
}


