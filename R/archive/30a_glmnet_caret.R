################################################################################
### Section 12.5 Penalized Models

## The glmnet model
glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                        # lambda = seq(.01, .2, length = 40))
                        lambda = rev(exp(seq(log(.00001), log(200), length=20))))

xmat_train <- as.matrix(dat[, .SD, .SDcols=-c("grp","date","id", "wnv")])
ymat_train <- as.matrix(dat[, wnv])
seasons <- split(1:nrow(dat), dat$grp)
names(seasons) <- letters[1:length(seasons)]

set.seed(476)
glmnFit <- train(x = xmat_train,
                 y = as.factor(ifelse(ymat_train[,1],"yes", "no") ),
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = trainControl(method = "LGOCV",
                                          summaryFunction = twoClassSummary,
                                          classProbs = TRUE,
                                          index = seasons,
                                          savePredictions = TRUE))
glmnFit

glmnetPreds <- merge(glmnFit$pred,  glmnFit$bestTune)
glmnetCM <- confusionMatrix(glmnFit, norm = "none")
glmnetCM

glmnetRoc <- roc(response = glmnetPreds$obs,
                 predictor = glmnetPreds$successful,
                 levels = rev(levels(glmnetPreds$obs)))

glmnFit0 <- glmnFit
glmnFit0$results$lambda <- format(round(glmnFit0$results$lambda, 3))

glmnPlot <- plot(glmnFit0,
                 plotType = "level",
                 cuts = 15,
                 scales = list(x = list(rot = 90, cex = .65)))

update(glmnPlot,
       ylab = "Mixing Percentage\nRidge <---------> Lasso",
       sub = "",
       main = "Area Under the ROC Curve",
       xlab = "Amount of Regularization")

plot(plsRoc2, type = "s", col = rgb(.2, .2, .2, .2), legacy.axes = TRUE)
plot(ldaRoc, type = "s", add = TRUE, col = rgb(.2, .2, .2, .2), legacy.axes = TRUE)
plot(lrRoc, type = "s", col = rgb(.2, .2, .2, .2), add = TRUE, legacy.axes = TRUE)
plot(glmnetRoc, type = "s", add = TRUE, legacy.axes = TRUE)




