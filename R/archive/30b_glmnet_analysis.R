## This script is only working on the Logistic Regression model using "glmnet"

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

dat <- readRDS("data/20_model_data.Rds")
results <- readRDS("data/30_glmnet.Rds")

##------------------------------------------------------------------------------
## CREATE CV FOLDS
##------------------------------------------------------------------------------

## For named lists it works better if group is a character value
# dat[ , grp := paste0("year", year(date))]

##------------------------------------------------------------------------------
## Calculate results 
##------------------------------------------------------------------------------

## Additional Model metrics
a0_yearly <- t(sapply(results, function(l)sapply(l$cv, function(x) unname(x$model$a0))))
nulldev_yearly <- t(sapply(results, function(l)sapply(l$cv, function(x)x$model$nulldev)))
devratio_yearly <- t(sapply(results, function(l)sapply(l$cv, function(x)x$model$dev.ratio)))
a0_mean <- apply(a0_yearly, 1, mean)
nulldev_mean <- apply(nulldev_yearly, 1, mean)
devratio_mean <- apply(devratio_yearly, 1, mean)

result_summary <- calculate_summary_stats(results)
details <- calculate_summary_stats(results, data = TRUE)
preds <- details$preds
ymat <- details$ymat_test
str(preds)
str(ymat)

# wtf(result_summary)

# plot(rmse_mean ~ grid$alpha)
# plot(rmse_mean ~ grid$lambda)

rmse <- melt(result_summary[ , .SD, .SDcols = grep("rmse_",colnames(result_summary))])
roc <- melt(result_summary[ , .SD, .SDcols = grep("roc_",colnames(result_summary))])
auc <- melt(result_summary[ , .SD, .SDcols = grep("auc_",colnames(result_summary))])

roc <- roc[value > .5]
auc <- auc[value > .5]

boxplot(value ~ variable, rmse, main = "CV RMSE Error by year (out of sample test results)")
boxplot(value ~ variable, roc, main="CV ROC Error by year (out of sample test results)")
boxplot(value ~ variable, auc, main="CV AUC Error by year (out of sample test results)")

result_summary[ , plot(rmse_mean, auc_mean, main="AUC vs RMSE")]
result_summary[ , plot(rmse_mean, auc_mean, cex = 1+lambda)]
result_summary[ , lambda]
result_summary[auc_mean > .66 , plot(rmse_mean, auc_mean)]
result_summary[auc_mean > .66 &  rmse_mean < .31 , plot(rmse_mean, auc_mean)]
result_summary[auc_mean > .66 &  rmse_mean < .31 , rank(lambda)]
# result_summary[auc_mean > .66 &  rmse_mean < .31 , 
#                plot(rmse_mean, auc_mean, cex = scale(lambda))]
# result_summary[auc_mean > .66 &  rmse_mean < .31 , 
#                plot(rmse_mean, auc_mean, pch = lambda, col=as.factor(alpha))]


i <- 1
i <- 7
i <- 91
i <- 55 #***
i <- 71
i <- 120
i <- 46
result_summary[i, list(alpha, lambda, auc_mean, roc_mean, rmse_mean)]
examp <- data.table(
    y = unname(do.call(c, sapply(results[[i]]$cv, `[[`, 'ymat_test'))),
    yhat = unname(do.call(c, sapply(results[[i]]$cv, `[[`, 'yhat_test'))))
plot(examp[ , pROC::roc(y, yhat)], 
     main = paste0("ROC\n", "alpha=", result_summary[i, list(alpha)],
                   "   lambda=", round(result_summary[i, list(lambda)], 3)))
results[[1]]$cv$year2008$ymat_test

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



