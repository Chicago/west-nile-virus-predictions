

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
geneorama::loadinstall_libraries("rpart")
geneorama::loadinstall_libraries("partykit")

dat <- readRDS("data/20_model_data.Rds")

##------------------------------------------------------------------------------
## CREATE CV FOLDS
##------------------------------------------------------------------------------

## The year label will be used as a list name later, so it's better if it has 
## some text in it because numeric list names are problematic. 
dat[ , grp := paste0("year", year(date))]

##------------------------------------------------------------------------------
## CREATE PREVIOUS VALUES
##------------------------------------------------------------------------------
#     'GB' : {'n_estimators': [1, 2, 5, 10, 15],
#             'learning_rate' : [0.1, 0.25, 0.5, 0.75, 1],
#             'subsample' : [0.5, 1.0],
#             'max_depth': [1, 3, 5, 8]}}

## Arguments for R's GBM function:
#     formula = formula(data)
#     distribution = "bernoulli"
#     data = list()
#     weights
#     var.monotone = NULL
#     n.trees = 100
#     interaction.depth = 1
#     n.minobsinnode = 10
#     shrinkage = 0.001
#     bag.fraction = 0.5
#     train.fraction = 1
#     cv.folds = 0
#     keep.data = TRUE
#     verbose = "CV"
#     class.stratify.cv = NULL
#     n.cores = NULL

grid <- expand.grid(n.trees = c(10, 50, 100, 500, 1000), #'n_estimators': [1, 2, 5, 10, 15],
                    shrinkage = c(0.001, 0.01, 0.1), # 'learning_rate' : [0.1, 0.25, 0.5, 0.75, 1],
                    bag.fraction = c(0.25, 0.50), # 'subsample' : [0.5, 1.0],
                    interaction.depth = c(1, 5, 8)) # 'max_depth': [1, 3, 5, 8])
                    #     n.minobsinnode = 10
results <- list()

# g <- unique(dat$grp)[1]
# i <- 1
for(i in 1:nrow(grid)){
    results[[i]] <- list()  ## Placeholder for ith parameter set
    results[[i]][["cv"]] <- list() ## Placeholder for CV results
    
    results[[i]][['param']] <- grid[i,]
    params <- results[[i]][['param']]
    
    cat(i, unlist(params), "\n")
    
    for(g in unique(dat$grp)){
        
        ## Run model
        xmat_train <- dat[!(grp == g), .SD, .SDcols=-c("grp","date","id", "wnv")]
        ymat_train <- dat[!(grp == g), wnv]
        m <- gbm(y ~ ., data = data.frame(xmat_train, y = ymat_train),
                 distribution =  "bernoulli",
                 keep.data = FALSE,
                 n.cores = 4,
                 n.trees = params$n.trees,
                 shrinkage = params$shrinkage,
                 bag.fraction = params$bag.fraction,
                 interaction.depth = params$interaction.depth)
        xmat_test <- dat[(grp == g), .SD, .SDcols=-c("grp","date","id", "wnv")]
        ymat_test <- dat[(grp == g), wnv]
        yhat_test <- predict(m, xmat_test, type = "response",
                             n.trees = params$n.trees)
        
        ## Store results
        results[[i]][["cv"]][[g]] <- list()
        results[[i]][["cv"]][[g]][["model"]] <- m
        results[[i]][["cv"]][[g]][["xmat_train"]] <- xmat_train
        results[[i]][["cv"]][[g]][["ymat_train"]] <- ymat_train
        results[[i]][["cv"]][[g]][["xmat_test"]] <- xmat_test
        results[[i]][["cv"]][[g]][["ymat_test"]] <- ymat_test
        results[[i]][["cv"]][[g]][["yhat_test"]] <- yhat_test
    }
}
# rm(i, g, m, xmat_test, xmat_train, ymat_test, ymat_train)
lll()

##------------------------------------------------------------------------------
## Save results
##------------------------------------------------------------------------------
saveRDS(results, "data/30_gbm.Rds")





# str(results, 2)
# sapply(results, function(l) sapply(l$cv, function(x)x$model$lambda))
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
# rmse_yearly <- data.table(t(apply(errsqrd, 2, function(col) 
#     sapply(split(col, dat$grp), function(e) sqrt(mean(e))))))
# 
# ## AUC and ROC based on predictions
# metrics <- lapply(results, function(l) lapply(l$cv, function(x) 
#     calculate_metrics(x$ymat_test, x$yhat_test)))
# auc_yearly <- data.table(t(sapply(metrics, function(l) sapply(l, `[[`, "auc"))))
# roc_yearly <- data.table(t(sapply(metrics, function(l) sapply(l, `[[`, "roc"))))
# kappa_yearly <- data.table(t(sapply(metrics, function(l) sapply(l, `[[`, "kappa"))))
# 
# ## Overall results
# auc_mean <- apply(auc_yearly, 1, mean)
# roc_mean <- apply(roc_yearly, 1, mean)
# kappa_mean <- apply(kappa_yearly, 1, mean)
# 
# result_summary <- data.table(grid, 
#                              auc = auc_mean, 
#                              roc = roc_mean, 
#                              kappa = kappa_mean,
#                              rmse = rmse_mean,
#                              auc_yearly, 
#                              roc_yearly, 
#                              kappa_yearly,
#                              rmse_yearly)
