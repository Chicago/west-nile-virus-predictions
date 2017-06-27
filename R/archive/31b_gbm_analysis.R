##------------------------------------------------------------------------------
## INITIALIZE / IMPORT DATA
##------------------------------------------------------------------------------

rm(list=ls())

library(geneorama)
sourceDir("R/functions/")
# geneorama::loadinstall_libraries("fishmethods")
geneorama::loadinstall_libraries("labeling") # needed for ggplot2
geneorama::loadinstall_libraries("ggplot2")
geneorama::loadinstall_libraries("caret")
geneorama::loadinstall_libraries("glmnet")
geneorama::loadinstall_libraries("randomForest")
geneorama::loadinstall_libraries("gbm")
geneorama::loadinstall_libraries("e1071")
geneorama::loadinstall_libraries("ROCR")
geneorama::loadinstall_libraries("pROC")
geneorama::loadinstall_libraries("rpart")
geneorama::loadinstall_libraries("partykit")

dat <- readRDS("data/20_model_data.Rds")
results <- readRDS("data/30_gbm.Rds")


## Example model structure
# 
# names(results[[1]]$cv$year2008$model)
# [1] "initF"             "fit"               "train.error"       "valid.error"      
# [5] "oobag.improve"     "trees"             "c.splits"          "bag.fraction"     
# [9] "distribution"      "interaction.depth" "n.minobsinnode"    "num.classes"      
# [13] "n.trees"           "nTrain"            "train.fraction"    "response.name"    
# [17] "shrinkage"         "var.levels"        "var.monotone"      "var.names"        
# [21] "var.type"          "verbose"           "Terms"             "cv.folds"         
# [25] "call"              "m"   


##------------------------------------------------------------------------------
## Calculate result summaries
##------------------------------------------------------------------------------

result_summary <- calculate_summary_stats(results)
result_summary


# wtf(result_summary)

# plot(rmse_mean ~ grid$alpha)
# plot(rmse_mean ~ grid$lambda)

rmse <- result_summary[ , .SD, .SDcols = grep("rmse_",colnames(result_summary))]
roc <- result_summary[ , .SD, .SDcols = grep("roc_",colnames(result_summary))]
auc <- result_summary[ , .SD, .SDcols = grep("auc_",colnames(result_summary))]
boxplot(value ~ variable, melt(rmse), main = "CV RMSE Error by year (out of sample test results)")
boxplot(value ~ variable, melt(roc), main="CV ROC Error by year (out of sample test results)")
boxplot(value ~ variable, melt(auc), main="CV AUC Error by year (out of sample test results)")




result_summary[ , plot(rmse_mean, auc_mean, main="AUC vs RMSE")]


##------------------------------------------------------------------------------
## Results without latest season
##------------------------------------------------------------------------------
result_summary_ex2016 <- 
    data.table(grid, 
               auc = apply(auc[ , .SD,    .SDcols=-"auc_2016"], 1, mean), 
               roc = apply(roc[ , .SD,    .SDcols=-"roc_2016"], 1, mean), 
               rmse = apply(rmse[ , .SD,    .SDcols=-"rmse_2016"], 1, mean))


rpm <- rpart::rpart(roc ~ n.trees + shrinkage + bag.fraction + interaction.depth, 
                    result_summary_ex2016)
partykit::as.party(rpm)
plot(partykit::as.party(rpm))


result_summary_ex2016
rpm <- rpart::rpart(roc ~ n.trees + shrinkage + bag.fraction + interaction.depth,
                    result_summary_ex2016, control = rpart.control(minbucket = 1))
printcp(rpm)
partykit::as.party(prune(rpm, cp=0.034034))
plot(partykit::as.party(prune(rpm, cp=0.034034)))



rpm <- rpart::rpart(rmse ~ n.trees + shrinkage + bag.fraction + interaction.depth, 
                    result_summary_ex2016)
partykit::as.party(rpm)
plot(partykit::as.party(rpm))

result_summary[n.trees == 500 & interaction.depth > 3 & shrinkage > .006, 
               list(n.trees, shrinkage, bag.fraction, interaction.depth,
                    auc_mean, roc_mean, rmse_mean)]

# n.trees 500
# shrinkage .01
# bag.fraction .25
# interaction.depth 5

result_summary[, list(n.trees, shrinkage, bag.fraction, interaction.depth,
                      auc_mean, roc_mean, rmse_mean)]

i <- 39
examp <- data.table(
    y = unname(do.call(c, sapply(results[[i]]$cv, `[[`, 'ymat_test'))),
    yhat = unname(do.call(c, sapply(results[[i]]$cv, `[[`, 'yhat_test'))))
plot(examp[ , pROC::roc(y, yhat)])
# results[[1]]$cv$year2008$ymat_test
# pROC::roc(y, yhat)$auc



boxplot(yhat~y, examp)

conf_mat <- examp[ , calculate_confusion_matrix(y,yhat)]

ggplot(melt(conf_mat, id.vars="r", measure.vars=c("sensitivity", "specificity"))) +
    aes(x = r, y = value, colour = variable) + 
    labs(title=paste0("Sensitivity vs Specificity\n")) + ylab("") + geom_line(lwd = 1.5)

ggplot(melt(conf_mat, id.vars="r", measure.vars=c("precision", "recall"))) +
    aes(x = r, y = ifelse(is.nan(value),NA,value), colour = variable) + 
    labs(title=paste0("Precision vs Recall\n")) + ylab("") + geom_line(lwd = 1.5)

ggplot(melt(conf_mat, id.vars="r", measure.vars=c("fmeasure"))) +
    aes(x = r, y = value, colour = variable) + expand_limits(y=1) +
    labs(title=paste0("F Measure\n")) + ylab("") + geom_line(lwd = 1.5)

examp[ ,calculate_confusion_values(actual = y, expected = yhat, 
                                   r = conf_mat[which.max(conf_mat$fmeasure), r])]


