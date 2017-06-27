
##
## Example of code for doing computations in parallel (but it's slower than 
## not doing it in parallel)
##




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
geneorama::loadinstall_libraries("ggplot2")
geneorama::loadinstall_libraries("labeling") # needed for ggplot2
geneorama::loadinstall_libraries("caret")    # for preprocessing functions
geneorama::loadinstall_libraries("caret")
geneorama::loadinstall_libraries("glmnet")
geneorama::loadinstall_libraries("randomForest")
geneorama::loadinstall_libraries("gbm")
geneorama::loadinstall_libraries("e1071")
geneorama::loadinstall_libraries("ROCR")
geneorama::loadinstall_libraries("pROC")

dat <- readRDS("data/20_model_data.Rds")

##------------------------------------------------------------------------------
## CREATE CV FOLDS
##------------------------------------------------------------------------------

## For named lists it works better if group is a character value
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

model_fun <- function(g, dt = dat, params){
    ## Run model
    require(data.table)
    require(gbm)
    xmat_train <- dt[!(grp == g), .SD, .SDcols=-c("grp","date","id", "wnv")]
    ymat_train <- dt[!(grp == g), wnv]
    m <- gbm(y ~ ., data = data.frame(xmat_train, y = ymat_train),
             distribution =  "bernoulli",
             keep.data = FALSE,
             n.cores = 4,
             n.trees = params$n.trees,
             shrinkage = params$shrinkage,
             bag.fraction = params$bag.fraction,
             interaction.depth = params$interaction.depth)
    xmat_test <- dt[(grp == g), .SD, .SDcols=-c("grp","date","id", "wnv")]
    ymat_test <- dt[(grp == g), wnv]
    yhat_test <- predict(m, xmat_test, type = "response",
                         n.trees = params$n.trees)
    
    ## Store results
    result <- list()
    result[["model"]] <- m
    result[["xmat_train"]] <- xmat_train
    result[["ymat_train"]] <- ymat_train
    result[["xmat_test"]] <- xmat_test
    result[["ymat_test"]] <- ymat_test
    result[["yhat_test"]] <- yhat_test
    return(result)
}

require(parallel)
cl <- makeCluster(detectCores())

for(i in 1:nrow(grid)){
    results[[i]] <- list()  ## Placeholder for ith parameter set
    results[[i]][["cv"]] <- list() ## Placeholder for CV results
    
    results[[i]][['param']] <- grid[i,]
    params <- results[[i]][['param']]
    cat(i, unlist(params), "\n")
    results[[i]][["cv"]] <- parLapply(cl, unique(dat$grp), model_fun, dat, params)
}

lll()

stopCluster(cl)


