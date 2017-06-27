

## Previous model grid from Hector:

# classifiers = {
#     'LR' : LogisticRegression(),
#     'KNN': KNeighborsClassifier(),
#     'DT' : DecisionTreeClassifier(),
#     'SVM': LinearSVC(),
#     'RF' : RandomForestClassifier(),
#     'GB' : GradientBoostingClassifier()}
#
# grid = {
#     'LR' : {'penalty': ['l1', 'l2'],
#             'C': [0.0001, 0.001, 0.01, 0.05, 0.1, 1, 5, 10, 20],
#             'n_jobs': [2]},
#     'KNN': {'n_neighbors': [1, 5, 10, 25, 50, 100],
#             'weights': ['uniform', 'distance'],
#             'algorithm': ['auto', 'ball_tree', 'kd_tree'],
#             'n_jobs': [2]},
#     'DT' : {'criterion': ['gini', 'entropy'],
#             'max_depth': [1, 5, 10, 20],
#             'max_features': ['sqrt', 'log2'],
#             'min_samples_split': [2, 5, 10]},
#     'SVM': {'C' : [0.01, 0.1, 0.25, 0.5, 1, 2, 5, 10]},
#     'RF' : {'n_estimators': [1, 5, 10],
#             'max_depth': [1, 3, 5, 10],
#             'max_features': ['sqrt', 'log2'],
#             'min_samples_split': [2, 5, 10]},
#     'GB' : {'n_estimators': [1, 2, 5, 10, 15],
#             'learning_rate' : [0.1, 0.25, 0.5, 0.75, 1],
#             'subsample' : [0.5, 1.0],
#             'max_depth': [1, 3, 5, 8]}}


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

# dat <- readRDS("data/20_model_data.Rds")
dat <- readRDS("data/21_model_data.Rds")

##------------------------------------------------------------------------------
## CREATE CV FOLDS
##------------------------------------------------------------------------------

## For named lists it works better if group is a character value
dat[ , grp := paste0("year", year(date))]

##------------------------------------------------------------------------------
## CREATE PREVIOUS VALUES
##------------------------------------------------------------------------------

grid <- expand.grid(alpha = c(0, .2, .4, .6, .8, 1),
                    # lambda = seq(.01, .2, length = 40))
                    #lambda = rev(exp(seq(log(.00001), log(200), length=10)))
                    lambda = rev(exp(seq(log(.00001), log(20), length=20))))
results <- list()

xmat <- dat[year(date) > 2007]
NAsummary(xmat)
exclude <- which(apply(xmat, 1, function(x) any(is.na(x))))
xmat <- xmat[-exclude]

# g <- unique(xmat$grp)[1]
# i <- 1
for(i in 1:nrow(grid)){
    print(i)
    results[[i]] <- list()  ## Placeholder for ith parameter set
    results[[i]][["cv"]] <- list() ## Placeholder for CV results
    
    results[[i]][['param']] <- grid[i,]
    params <- results[[i]][['param']]
    for(g in unique(xmat$grp)){
        
        ## Run model
        xmat_train <- xmat[!(grp == g), .SD, .SDcols=-c("grp","date","id", "wnv", "trap_obs_count")]
        ymat_train <- xmat[!(grp == g), wnv]
        m <- glmnet(x = as.matrix(xmat_train),
                    y = as.matrix(ymat_train),
                    family = "binomial",
                    alpha = params$alpha,
                    lambda = params$lambda)
        xmat_test <- xmat[(grp == g), .SD, .SDcols=-c("grp","date","id", "wnv", "trap_obs_count")]
        ymat_test <- xmat[(grp == g), wnv]
        yhat_test <- predict(m, as.matrix(xmat_test), type = "response")[,1]
        
        ## Store results
        results[[i]][["cv"]][[g]] <- list()
        results[[i]][["cv"]][[g]][["model"]] <- m
        results[[i]][["cv"]][[g]][["xmat_train"]] <- xmat_train
        results[[i]][["cv"]][[g]][["ymat_train"]] <- ymat_train
        results[[i]][["cv"]][[g]][["xmat_test"]] <- xmat_test
        results[[i]][["cv"]][[g]][["ymat_test"]] <- ymat_test
        results[[i]][["cv"]][[g]][["yhat_test"]] <- yhat_test
        if(length(yhat_test)==0)browser()
    }
}
# rm(i, g, m, xmat_test, xmat_train, ymat_test, ymat_train)
lll()

saveRDS(results, "data/30_glmnet.Rds")


# ## Example structure
# # results[[1]]$cv$year2008$model$a0
# 
# # str(results, 2)
# # sapply(results, function(l) sapply(l$cv, function(x)x$model$lambda))
# names(results[[1]])
# 
# ## Model metrics
# a0_yearly <- t(sapply(results, function(l)sapply(l$cv, function(x) unname(x$model$a0))))
# nulldev_yearly <- t(sapply(results, function(l)sapply(l$cv, function(x)x$model$nulldev)))
# devratio_yearly <- t(sapply(results, function(l)sapply(l$cv, function(x)x$model$dev.ratio)))
# 
# ## Predictions and RMSE
# preds <- lapply(results, function(l) lapply(l$cv, function(x) x$yhat_test))
# preds <- lapply(preds, unsplit, dat$grp)
# preds <- do.call(cbind, preds)
# ymat_test <- lapply(results, function(l) lapply(l$cv, function(x) x$ymat_test))
# ymat_test <- lapply(ymat_test, unsplit, dat$grp)
# ymat_test <- do.call(cbind, ymat_test)
# err <- (preds - ymat_test)
# errsqrd <- (err) ^ 2
# rmse_mean <- sqrt(apply(errsqrd, 2, mean))
# rmse_yearly <- t(apply(errsqrd, 2, function(col) 
#     sapply(split(col, dat$grp), function(e) sqrt(mean(e)))))
# 
# ## AUC and ROC based on predictions
# metrics <- lapply(results, function(l) lapply(l$cv, function(x) 
#     calculate_metrics(x$ymat_test, x$yhat_test)))
# auc_yearly <- t(sapply(metrics, function(l) sapply(l, `[[`, "auc")))
# roc_yearly <- t(sapply(metrics, function(l) sapply(l, `[[`, "roc")))
# kappa_yearly <- t(sapply(metrics, function(l) sapply(l, `[[`, "kappa")))
# 
# ## Overall results
# a0_mean <- apply(a0_yearly, 1, mean)
# nulldev_mean <- apply(nulldev_yearly, 1, mean)
# devratio_mean <- apply(devratio_yearly, 1, mean)
# auc_mean <- apply(auc_yearly, 1, mean)
# roc_mean <- apply(roc_yearly, 1, mean)
# kappa_mean <- apply(kappa_yearly, 1, mean)
# 
# result_summary <- data.table(grid, 
#                              a0 = a0_mean, 
#                              devratio = devratio_mean, 
#                              auc = auc_mean, 
#                              roc = roc_mean, 
#                              rmse = rmse_mean,
#                              a0_yearly, 
#                              devratio_yearly, 
#                              auc_yearly, 
#                              roc_yearly, 
#                              rmse_yearly)
# 
# result_summary
# # wtf(result_summary)
# 
# # plot(rmse_mean ~ grid$alpha)
# # plot(rmse_mean ~ grid$lambda)
# 
# boxplot(rmse_yearly, main="CV RMSE Error by year (out of sample test results)")
# boxplot(roc_yearly, main="CV RMSE Error by year (out of sample test results)")
# boxplot(auc_yearly, main="CV RMSE Error by year (out of sample test results)")
# 
# pROC::roc(y, yhat)$auc
# i <- 1
# i <- 7
# i <- 91
# i <- 55 #***
# i <- 71
# i <- 120
# examp <- data.table(
#     y = unname(do.call(c, sapply(results[[i]]$cv, `[[`, 'ymat_test'))),
#     yhat = unname(do.call(c, sapply(results[[i]]$cv, `[[`, 'yhat_test'))))
# plot(examp[ , pROC::roc(y, yhat)])
# results[[1]]$cv$year2008$ymat_test


