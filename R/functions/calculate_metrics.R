

calculate_metrics <- function(y, yhat){
    
    require(ROCR)
    require(pROC)
    require(e1071)
    
    # y <- ymat_test[,1]
    # yhat <- preds[,1]
    # boxplot(yhat~y)
    ##--------------------------------------------------------------------------
    ## AUC
    ##--------------------------------------------------------------------------
    pred <- ROCR::prediction(yhat, y)
    auc <- performance(pred, measure = "auc")@y.values[[1]]
    # auc * 2 - 1
    # gini
    
    ##--------------------------------------------------------------------------
    ## KAPPA
    ##--------------------------------------------------------------------------
    
    kappa <- e1071::classAgreement(table(y,yhat))[["kappa"]]
    
    ### NICE METRIC SIMILAR TO KAPPA:
    # plot(dt[ , sum(y==TRUE)/.N, list(yhat)])
    
    #plot(performance(pred, "tpr", "fpr"))
    # plot(performance(pred, measure = "phi"))
    # plot(performance(pred, measure = "f"))
    
    ##--------------------------------------------------------------------------
    ## ROC, sens, spec
    ##--------------------------------------------------------------------------
    ROC <- pROC::roc(y, yhat)$auc[1]
    
    ##--------------------------------------------------------------------------
    ## Return List
    ##--------------------------------------------------------------------------
    ret <- list(auc = auc, 
                roc = ROC,
                kappa = kappa)
    return(ret)
}
