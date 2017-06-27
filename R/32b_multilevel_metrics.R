

##==============================================================================
## INITIALIZE
##==============================================================================


source("R/32a_multilevel_model.R", local = TRUE)

library(geneorama)


##==============================================================================
## CONVERT XMAT'S WEEK AND WNV TO NUMERIC
##==============================================================================
# xmat[ , weeknumber := as.numeric(levels(week)[week])]
# xmat[ , wnvnumber := as.numeric(levels(wnv)[wnv])]
# xmat[ , wnvwnv := wnv==1 & wnvw1==1]  ## Metric for WNV two weeks in a row

par(mfrow=c(1,2))
boxplot(m2 ~ wnv, xmat[grp=="year2016"], main="predicting any week\n old model (m2)")
boxplot(m3 ~ wnv, xmat[grp=="year2016"], main="predicting any week\n new model (m3)")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(m2 ~ wnvwnv, xmat[grp=="year2016"], main="predicting second week\n old model (m2)", ylim = c(0,1))
boxplot(m3 ~ wnvwnv, xmat[grp=="year2016"], main="predicting second week\n new model (m3)", ylim = c(0,1))
par(mfrow=c(1,1))


##==============================================================================
## AUC / ROC
##==============================================================================

## AUC for overall model
xmat[grp=="year2016" , calculate_metrics(wnv, m2)]
## AUC for predicting just the second week
xmat[grp=="year2016" , calculate_metrics(wnvwnv, m2)]

## Confirming auc calc with gini:
xmat[grp=="year2016", gini(wnvnum, m2)/2+.5]
xmat[grp=="year2016", gini(wnvwnvnum, m2)/2+.5]



## AUC for overall model
xmat[grp=="year2016" , calculate_metrics(wnv, m3)]
## AUC for predicting just the second week
xmat[grp=="year2016" , calculate_metrics(wnvwnv, m3)]

## Confirming auc calc with gini:
xmat[grp=="year2016", gini(wnvnum, m3)/2+.5]
xmat[grp=="year2016", gini(wnvwnvnum, m3)/2+.5]


## Actual and predicted shown by Site ID by Date
# matwnv <- dcast(xmat[ , list(wnv, id, date)], date ~ id, value.var = "wnv")
# matm2 <- dcast(xmat[ , list(m2, id, date)], date ~ id, value.var = "m2")


##------------------------------------------------------------------------------
## pROC::ROC CURVES FOR WNV AND WNVWNV
##------------------------------------------------------------------------------
plot(xmat[grp=="year2016" , pROC::roc(wnv, m3)])
plot(xmat[grp=="year2016" , pROC::roc(wnvwnv, m3)])

m3roc_2016 <- pROC::roc(response = xmat[grp=="year2016", levels(wnv)[wnv]],
                        predictor = xmat[grp=="year2016", m3],
                        levels = levels(xmat$wnv))
m3roc_2016_twice <- pROC::roc(response = xmat[grp=="year2016", levels(wnvwnv)[wnvwnv]],
                              predictor = xmat[grp=="year2016", m3],
                              levels = levels(xmat$wnv))
m2roc_2016 <- pROC::roc(response = xmat[grp=="year2016", levels(wnv)[wnv]],
                        predictor = xmat[grp=="year2016", m2],
                        levels = levels(xmat$wnv))
m2roc_2016_twice <- pROC::roc(response = xmat[grp=="year2016", levels(wnvwnv)[wnvwnv]],
                              predictor = xmat[grp=="year2016", m2],
                              levels = levels(xmat$wnv))
# plot(roc_2016, lty=2)
plot(m2roc_2016_twice, lty=2, 
     main = paste0("ROC curves for m2 (dotted) and m3 (solid)"))
plot(m3roc_2016_twice, lty=1, add=TRUE)

##------------------------------------------------------------------------------
## OPTIMAL CHOICE OF CUTOFF YEAR BY YEAR (RETROSPECTIVE)
##------------------------------------------------------------------------------
sapply(unique(xmat$grp), function(i)
    xmat[grp == i & !is.na(m2), 
         calculate_confusion_matrix(wnv==1, m3, seq(0,1,.0025))][which.max(fmeasure)])
sapply(unique(xmat$grp), function(i)
    xmat[grp == i & !is.na(m2), 
         calculate_confusion_matrix(wnvwnv==1, m3, seq(0,1,.0025))][which.max(fmeasure)])

sapply(unique(xmat$grp), function(i)
    xmat[grp == i & !is.na(m3), 
         calculate_confusion_matrix(wnv==1,  m3, seq(0,1,.0025))][which.max(fmeasure)])
sapply(unique(xmat$grp), function(i)
    xmat[grp == i & !is.na(m3), 
         calculate_confusion_matrix(wnvwnv==1,  m3, seq(0,1,.0025))][which.max(fmeasure)])

##------------------------------------------------------------------------------
## METRICS
##------------------------------------------------------------------------------
# library("scales")
# xmat[ , list(rescale(m2), m2)][,range(V1, na.rm = T)]


str(xmat[grp=="year2016"& !is.na(m3), list(m3, wnvwnvnum)])
conf_mat_twice <- xmat[grp=="year2016"& !is.na(m3),
                       calculate_confusion_matrix(y = wnvwnvnum,
                                                  yhat = m3)]
print(conf_mat_twice, row.names = FALSE)

conf_mat_twice <- xmat[grp=="year2016"& !is.na(m3),
                       calculate_confusion_matrix(y = wnvwnvnum,
                                                  yhat = m3,
                                                  r = seq(0,1,.0025))]

ggplot(conf_mat_twice) +
    aes(x = 1 - specificity, y = sensitivity) + geom_line(lwd = 1.5) +
    geom_point(size = 3) + geom_abline(slope = 1, intercept = 0) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
              fill = "transparent", color = "black", size = .5) +
    labs(title=paste0("ROC Curve for regular WNV predictions in 2016\n")) 


ggplot(melt(conf_mat_twice, id.vars="r", measure.vars=c("sensitivity", "specificity"))) +
    aes(x = r, y = value, colour = variable) + 
    labs(title=paste0("Sensitivity vs Specificity\n")) + ylab("") + geom_line(lwd = 1.5)

ggplot(melt(conf_mat_twice, id.vars="r", measure.vars=c("precision", "recall"))) +
    aes(x = r, y = ifelse(is.nan(value),NA,value), colour = variable) + 
    labs(title=paste0("Precision vs Recall\n")) + ylab("") + geom_line(lwd = 1.5)

ggplot(melt(conf_mat_twice, id.vars="r", measure.vars=c("fmeasure"))) +
    aes(x = r, y = value, colour = variable) + expand_limits(y=1) +
    labs(title=paste0("F Measure\n")) + ylab("") + geom_line(lwd = 1.5)

conf_mat_twice[which.max(fmeasure)]

##------------------------------------------------------------------------------
## Week by week summary of Precision / Recall
##------------------------------------------------------------------------------
# r <- .25
# r <- .4
# r <- .375
# r <- .35
# r <- .385
r <- .39

tab <- xmat[grp=="year2016"& !is.na(m3), 
            list(.N,
                 TRUE_POS = sum(m3[wnvwnvnum==1] > r),
                 TRUE_NEG = sum(m3[wnvwnvnum==0] <= r),
                 FALSE_NEG = sum(m3[wnvwnvnum==1] < r),
                 FALSE_POS = sum(m3[wnvwnvnum==0] > r),
                 TOTAL_ACTUAL_POS = sum(wnvwnv == 1),
                 TOTAL_PRED_POS = sum(m3 > r)), 
            keyby = list(date)]
tab[ , date := as.character(date)]
tab_totals <- tab[ , list("TOTAL", sum(N), sum(TRUE_POS), sum(TRUE_NEG), 
                          sum(FALSE_NEG), sum(FALSE_POS), sum(TOTAL_ACTUAL_POS),
                          sum(TOTAL_PRED_POS))]
setnames(tab_totals, colnames(tab))
tab <- rbind(tab, tab_totals)
rm(tab_totals)
print(tab, row.names = FALSE)
tab[date=="TOTAL", list(paste0(TRUE_POS, "/" ,TOTAL_ACTUAL_POS), TRUE_POS / TOTAL_ACTUAL_POS)]
tab[date=="TOTAL", list(paste0(TRUE_POS, "/" ,TOTAL_PRED_POS), TRUE_POS / TOTAL_PRED_POS)]

##==============================================================================
## CALCULATE SERIES OF CONFUSION MATRICIES FOR "WHAT IF" ANALYSIS
##==============================================================================

prev <- xmat[!is.na(m2), list(y = wnvwnvnum, yhat = m3, grp)]
prev <- split(prev, prev$grp)
prev <- lapply(prev, function(mat) 
    calculate_confusion_matrix(mat$y, mat$yhat, r = seq(0, 1, .0025)))
prev[["year2009"]] <- NULL

sapply(prev, function(x) which.max(x$fmeasure))
prev[["year2008"]][68]
prev[["year2009"]][NA]
prev[["year2010"]][121]
prev[["year2011"]][153]
prev[["year2012"]][168]
prev[["year2013"]][119]
prev[["year2014"]][106]
prev[["year2015"]][91]

## This is "what would have happened if we had used 20% as a cutoff, 
## 25%, 30%, etc...
sapply(prev, function(x)x[which.min(abs(r-.20))])
sapply(prev, function(x)x[which.min(abs(r-.25))])
sapply(prev, function(x)x[which.min(abs(r-.30))])
sapply(prev, function(x)x[which.min(abs(r-.35))])
sapply(prev, function(x)x[which.min(abs(r-.40))])


## Based on the above, I would probably just use a low threshold and take 
## high sensitivity for slighly lower specificity 
prev[["year2016"]][which.min(abs(r-.20))]
prev[["year2016"]][which.min(abs(r-.25))]
prev[["year2016"]][which.min(abs(r-.30))]

##==============================================================================
## METRICS
##==============================================================================

## Total positive cases of WNV by year &
## Total cases where WNV happened two weeks in a row in the same trap, by year
xmat[i = TRUE,
     j = list(WNV_Cases = sum(wnvnum),
              WNV_Cases_twice_in_a_row = sum(wnvwnvnum)),
     keyby = grp]

conf_mat_twice <- xmat[!is.na(m2),
                       list(y = as.integer(wnv==1 & wnvw1==1), 
                            yhat = m2)][
                                ,calculate_confusion_matrix(y,yhat)]

##==============================================================================
## SEASONALITY OF SCORES
##==============================================================================

xmat[ , weeknumber := as.numeric(as.character(week))]

xmat[i = TRUE,
     j = list(wnv_cases = sum(wnvnum), 
              total = .N,
              percent = sum(wnvnum) / .N),
     keyby = weeknumber]

msum <- xmat[i = !is.na(m2),
             j = list(score = m2, 
                      weeknumber,
                      wnv = sum(wnvnum), 
                      wnvwnv = sum(wnvwnvnum)), 
keyby = list(year = as.character(grp),
                          date,
                          week = paste0("week_", week))]
msum


ggplot(msum, aes(week, score)) + geom_boxplot()
ggplot(msum[weeknumber > 24], aes(week, score)) + geom_boxplot(aes(fill = year))

ggplot(msum, aes(week, wnv)) + geom_boxplot()
ggplot(msum, aes(week, wnvwnv)) + geom_boxplot()
msum[i = TRUE,
     j = list(average_date = paste0(round(mean(month(date)), 0), "/",
                                    round(mean(mday(date)), 0))),
     keyby = week]




# geom_line(aes(month, mean, colour = variable, group = variable), 
#           data= mmsum, size = 2) +
#     geom_point(aes(month, mean, colour = variable, group = variable), 
#                data= mmsum, size = 2, colour = "black") +
#     ggtitle(paste0("Citywide count of traps collected (BLUE) compared to\n",
#                    "count of traps that were WNV positive (ORANGE)\n",
#                    "2008 - 2016\n"))


par(mfrow=c(1,1))
hist(xmat[ , m2])

par(mfrow=c(2, 1))
hist(xmat[wnv==0 , m2], xlim=c(-8, 2))
hist(xmat[wnv==1 , m2], xlim=c(-8, 2))

par(mfrow=c(2, 1))
hist(xmat[wnv==0 , rescale(m2)], xlim=c(0, 1), 20)
hist(xmat[wnv==1 , rescale(m2)], xlim=c(0, 1), 20)

par(mfrow=c(2,2))
hist(xmat[wnv==0 & wnvw1 == 0 , rescale(m2)], xlim=c(0, 1), 20)
hist(xmat[wnv==0 & wnvw1 == 1 , rescale(m2)], xlim=c(0, 1), 20)
hist(xmat[wnv==1 & wnvw1 == 1 , rescale(m2)], xlim=c(0, 1), 20)
hist(xmat[wnv==1 & wnvw1 == 0 , rescale(m2)], xlim=c(0, 1), 20)
par(mfrow=c(1,1))



par(mfrow=c(1,1))
xmat[wnv==0 & wnvw1 == 0 , outcome:= "00"]
xmat[wnv==0 & wnvw1 == 1 , outcome:= "01"]
xmat[wnv==1 & wnvw1 == 1 , outcome:= "11"]
xmat[wnv==1 & wnvw1 == 0 , outcome:= "10"]
boxplot(rescale(m2) ~ outcome, xmat)
boxplot(m2 ~ outcome, xmat)



conf_mat_twice

ggplot(melt(conf_mat_twice, id.vars="r", measure.vars=c("sensitivity", "specificity"))) +
    aes(x = r, y = value, colour = variable) + 
    labs(title=paste0("Sensitivity vs Specificity\n")) + ylab("") + geom_line(lwd = 1.5)

ggplot(melt(conf_mat_twice, id.vars="r", measure.vars=c("precision", "recall"))) +
    aes(x = r, y = ifelse(is.nan(value),NA,value), colour = variable) + 
    labs(title=paste0("Precision vs Recall\n")) + ylab("") + geom_line(lwd = 1.5)

ggplot(melt(conf_mat_twice, id.vars="r", measure.vars=c("fmeasure"))) +
    aes(x = r, y = value, colour = variable) + expand_limits(y=1) +
    labs(title=paste0("F Measure\n")) + ylab("") + geom_line(lwd = 1.5)




##------------------------------------------------------------------------------
## TEST FOR PREDICTING WITH NEW DATA WITH PARTIAL FACTOR LEVELS
##------------------------------------------------------------------------------
if(FALSE){
    ii <- sample(xmat[, which(!is.na(m2))])[1:10]
    ftemp <- tempfile()
    write.table(xmat[ii,list(wnvw1, wnvw2,awnd,tmax,prcp,id,week)],ftemp,row.names=F)
    fread(ftemp)
    tmp <- fread(ftemp)
    lme4:::predict.merMod(m2, tmp)
    str(tmp)
    xmat[ii,m2]
}


##------------------------------------------------------------------------------
## ORIGINAL MODEL METRICS FOR ALL OBSERVATIONS
##------------------------------------------------------------------------------

if(FALSE){
    # xmat[ , wnv]
    # xmat[ , as.integer(wnv)-1]
    # xmat[ , wnv==1]
    # 
    # library("scales")
    # xmat[ , list(rescale(m2), m2)][,range(V1, na.rm = T)]
    
    conf_mat <- xmat[grp == "year2016" & !is.na(m2) , 
                     calculate_confusion_matrix(wnv==1,  m2, seq(0,1,.0025))]
    conf_mat
    conf_mat[which.max(fmeasure)]
    
    ggplot(conf_mat) +
        aes(x = 1 - specificity, y = sensitivity) + geom_line(lwd = 1.5) +
        geom_point(size = 3) + geom_abline(slope = 1, intercept = 0) +
        geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
                  fill = "transparent", color = "black", size = .5) +
        labs(title=paste0("ROC Curve for regular WNV predictions in 2016\n")) 
    
    ggplot(melt(conf_mat, id.vars="r", measure.vars=c("precision", "recall"))) +
        aes(x = r, y = ifelse(is.nan(value),NA,value), colour = variable) + 
        labs(title=paste0("Precision vs Recall\n")) + ylab("") + geom_line(lwd = 1.5)
    
    ggplot(melt(conf_mat, id.vars="r", measure.vars=c("fmeasure"))) +
        aes(x = r, y = value, colour = variable) + expand_limits(y=1) +
        labs(title=paste0("F Measure\n")) + ylab("") + geom_line(lwd = 1.5)
    
    ggplot(melt(conf_mat, id.vars="r", measure.vars=c("sensitivity", "specificity"))) +
        aes(x = r, y = value, colour = variable) + 
        labs(title=paste0("Sensitivity vs Specificity\n")) + ylab("") + geom_line(lwd = 1.5)
    
    conf_mat[which.max(fmeasure)]
}
