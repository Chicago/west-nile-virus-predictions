

## Multilevel models using Gelman book (chapter 14)

##------------------------------------------------------------------------------
## INITIALIZE / IMPORT DATA
##------------------------------------------------------------------------------

library(geneorama)
sourceDir("R/functions/", trace = FALSE)
loadinstall_libraries(c("labeling", "ggplot2", "e1071", "ROCR", "pROC", "arm", "scales"))

idTable <- readRDS("data/10_calculate_idtable.Rds")
dat <- readRDS("data/21_full_wnv_data_aggregated.Rds")
# oracle_traps <- download_oracle_traps()
# wnv <- open_latest_wnv_file()

# mat <- dcast(dat[ , list(tot = total_true + total_false, id, date)], 
#                    date ~ id, value.var = "tot")
# mat_length <- dcast(dat[ , list(tot = total_true + total_false, id, date=round_weeks(date))], 
#                     date ~ id, value.var = "tot", fun.aggregate = length)
# mat_sum <- dcast(dat[ , list(tot = total_true + total_false, id, date=round_weeks(date))], 
#                  date ~ id, value.var = "tot", fun.aggregate = sum)
# n_valid <- apply(mat, 1, function(x)length(x) - length(x[is.na(x)]) + 1)
# 


##------------------------------------------------------------------------------
## CREATE CV FOLDS
##------------------------------------------------------------------------------

## For named lists it works better if group is a character value
dat[ , grp := paste0("year", year(date))]

##------------------------------------------------------------------------------
## CREATE XMAT
## The variable xmat is the matrix of variables that are used in the model. In 
## this case it also contains the y variables. For some models xmat must 
## literally be a matrix, but in this case a data.table / data.frame was fine. 
## The y value being predicted is wnvwnv, or "WNV" happening two weeks in a 
## row. 
##------------------------------------------------------------------------------
xmat <- dat[ , list(id = as.factor(id), 
                    grp = as.factor(grp),
                    week = as.factor(week),
                    date,
                    wnvw1, wnvw2, wnv_ytd,
                    awnd = (awnd - mean(awnd)) / sd (awnd), 
                    prcp = (prcp - mean(prcp)) / sd (prcp), 
                    tmax = (tmax - mean(tmax)) / sd (tmax), 
                    tot = total_true + total_false,
                    total_true = total_true, 
                    total_false = total_false,
                    # tot_log = log(total_true + total_false),
                    # tot_half = (total_true + total_false) / 2,
                    wnvnum = wnv,
                    wnv = as.factor(wnv),
                    wnvwnv = as.factor(as.integer(wnv & wnvw1)),
                    wnvwnvnum = as.integer(wnv & wnvw1),
                    wnv_f1 = wnv_f1,
                    wnvwnv_f1 = as.integer(wnv & wnv_f1))]
xmat_means <- xmat[ , list(mean = mean(tot)), keyby = list(id, grp)]
xmat <- merge(xmat, xmat_means, key(xmat_means))
# xmat[ , tot := (tot - mean(tot)) / sd(tot)]
# xmat[ , tot_half := (tot_half - mean(tot_half)) / sd(tot_half)]

## Remove rows where wnvwnv is NA 
## Sometimes a trap will only appear once in a season, which casuses the NA value
xmat <- xmat[!is.na(wnvwnvnum)]

## Labor day indicator
xmat[ , labor_day := labor_day(year(date))]

# xmat[ , range(as.numeric(as.character(week)))]
weeksummary <- xmat[ , list(
    .N, 
    postwice = sum(wnvwnvnum), 
    integertime = year(date) + (as.numeric(as.character(week)) - 20) / 20), 
    keyby = list(year(date), week, date)]
# plot(N ~ integertime, weeksummary, type = "o")
# points(postwice ~ integertime, weeksummary, type = "o", col = "red")

## remove 2009 because it never had any postive values that met the business rule
# xmat[ , list(.N, wnvwnv = sum(wnvwnvnum)), keyby = list(year(date))]
# xmat <- xmat[year(date)!= 2009]

## Get common ids and common weeks so that we can do prediction later
common_ids <- xmat[grp!="year2016" & grp!="year2009", unique(id)]
common_weeks <- xmat[grp!="year2016" & grp!="year2009", unique(week)]
xmat <- xmat[id %in% common_ids]
xmat <- xmat[week %in% common_weeks]
xmat <- droplevels(xmat)

xmat[is.na(wnv_ytd), wnv_ytd := 0]
xmat[is.na(wnvw1), wnvw1 := 0]
xmat[is.na(wnvw2), wnvw2 := 0]

xmat
summary(xmat)
str(xmat)
NAsummary(xmat)

##------------------------------------------------------------------------------
## MODEL 2
## This was the second model using glmer (and the first model with reasonable
## results).  Model 2 is only predicting if there will be WNV, which does not
## follow the business rule for spraying in which WNV has to be present two
## weeks in a row.
##------------------------------------------------------------------------------

##------------------------------------
## Production: run model on all data
##------------------------------------
m2p <- glmer(wnv ~ wnvw1 + wnvw2 + wnv_ytd + awnd + tmax + prcp + 
                (1 | id) + (1 | week), 
            family = binomial, 
            data = xmat)
tmpr <- lme4:::predict.merMod(m2p, xmat, type = c("link", "response")[2])
xmat[as.integer(names(tmpr)) , m2 := unname(tmpr)]
rm(tmpr)

##------------------------------------
## Test: Leave out 2016 as a holdout
##------------------------------------

## This is the function to examine for most errors happen with glmer
## Errors happen for example when data isn't complete, or doesn't contain
## enough unique values, e.g. near zero variance.
# undebug(lme4:::mkNewReTrms) 

m2t <- glmer(wnv ~ wnvw1 + wnvw2 + wnv_ytd + awnd + tmax + prcp + 
                 (1 | id) + (1 | week), 
             family = binomial, 
             data = xmat[grp!="year2016"])
display(m2t)
ranef(m2t)

## Extract the usual prediction, and add the test prediction to the data
tmpr <- lme4:::predict.merMod(m2t, xmat, type = c("link", "response")[2])
xmat[as.integer(names(tmpr)) , m2t := unname(tmpr)]

## Two predictions are given, one is a transform of the other.
tmpl <- lme4:::predict.merMod(m2t, xmat, type = c("link", "response")[1])
# plot(exp(tmpl)/(1+exp(tmpl)) ~ tmpr)
# hist(exp(tmpl)/(1+exp(tmpl)))
# hist(tmpr)

## The temp prediction components are no longer needed
rm(tmpr, tmpl)

## This is a simple dignostic plot that shows the seperation of classes
# boxplot(m2 ~ wnv, xmat)

## AUC / ROC values:
xmat[i = TRUE        , calculate_metrics(wnv, m2t)]  ## Overall
xmat[grp=="year2016" , calculate_metrics(wnv, m2t)]  ## Holdout
xmat[grp=="year2016" , calculate_metrics(wnvwnv, m2t)] ## Effectiveness for wnv twice

## Trap by Date scores
## Trap by Date results
## These views side-by-side are useful views for checking score performance
# matwnv <- dcast(xmat[ , list(wnv, id, date)], date ~ id, value.var = "wnv")
# matm2 <- dcast(xmat[ , list(m2t, id, date)], date ~ id, value.var = "m2t")

##------------------------------------------------------------------------------
## TEST OF DATA WITH PARTIAL FACTOR LEVELS
## Basically this was a round trip test to see how glmer handles factors. Turns
## out that it handles factors very well! So, new data sets with partial factor
## levels are appropriately linked to the right factor. For example, if you 
## modeled on AZ, CA, MI, and UT, the predicted on MI and CA, they would be 
## linked to levels #3 and #2
##------------------------------------------------------------------------------
# ii <- sample(xmat[, which(!is.na(m2))])[1:10]
# ftemp <- tempfile()
# write.table(xmat[ii,list(wnvw1, wnvw2,awnd,tmax,prcp,id,week)],ftemp,row.names=F)
# fread(ftemp)
# tmp <- fread(ftemp)
# lme4:::predict.merMod(m2, tmp)
# str(tmp)
# xmat[ii,m2]

##------------------------------------------------------------------------------
## MODEL 3
## Predict the actual business case of "two weeks in a row"
##------------------------------------------------------------------------------
options(warn = -1)
m3 <- glmer(wnvwnv ~ wnvw1 + wnvw2 + wnv_ytd + 
                awnd + tmax + prcp + (1 | id) + (1 | week) , 
            family = binomial, data = xmat)
m3t <- glmer(wnvwnv ~ wnvw1 + wnvw2 + wnv_ytd + 
                 awnd + tmax + prcp + (1 | id) + (1 | week) , 
             family = binomial, data = xmat[grp!="year2016"])
options(warn = 0)

tmpr <- lme4:::predict.merMod(m3, xmat, type = c("link", "response")[2])
xmat[as.integer(names(tmpr)) , m3 := unname(tmpr)]
tmpr <- lme4:::predict.merMod(m3t, xmat, type = c("link", "response")[2])
xmat[as.integer(names(tmpr)) , m3t := unname(tmpr)]
rm(tmpr)

## Add in model forecast
xmat_f <- xmat[ , list(wnvw1 = wnv,
                       wnvw2 = wnvw1,
                       wnv_ytd = wnv_ytd + wnvnum,
                       awnd, tmax, prcp, id, week)]
tmpr <- lme4:::predict.merMod(m3, xmat_f, type = c("link", "response")[2])
xmat[as.integer(names(tmpr)) , m3_forecast := unname(tmpr)]
rm(tmpr)

tmpr <- lme4:::predict.merMod(m3t, xmat_f, type = c("link", "response")[2])
xmat[as.integer(names(tmpr)) , m3t_forecast := unname(tmpr)]
rm(tmpr)

# boxplot(m3t ~ wnvwnv, xmat[grp=="year2016"])
xmat[grp=="year2016" , calculate_metrics(wnvwnv, m3t_forecast)]
xmat[grp=="year2016" & !is.na(wnvwnv_f1) , calculate_metrics(wnvwnv_f1, m3t_forecast)]

##------------------------------------------------------------------------------
## Model 3a (No sprays after labor day business rule)
##------------------------------------------------------------------------------
options(warn = -1)
m3a <- glmer(wnvwnv ~ wnvw1 + wnvw2 + wnv_ytd + 
                awnd + tmax + prcp + (1 | id) + (1 | week) , 
             family = binomial, data = xmat[date < labor_day][grp!="year2016"])
options(warn = 0)
tmpr <- lme4:::predict.merMod(m3a, xmat, type = c("link", "response")[2], allow.new.levels = TRUE)
xmat[as.integer(names(tmpr)) , m3a := unname(tmpr)]
rm(tmpr)
xmat[grp=="year2016" , calculate_metrics(wnvwnv, m3a)]



## Add in model forecast
tmpr <- lme4:::predict.merMod(m3a, xmat_f, type = c("link", "response")[2], allow.new.levels = TRUE)
xmat[as.integer(names(tmpr)) , m3a_forecast := unname(tmpr)]
rm(tmpr)
xmat[grp == "year2016" & !is.na(wnvwnv_f1) , calculate_metrics(wnvwnv_f1, m3a_forecast)]
xmat[grp == "year2016", calculate_metrics(wnvwnv, m3a_forecast)]

## Round predictions
for (col in colnames(xmat[ , m2:m3a_forecast])){
    xmat[ , (col) := round(xmat[[col]], 5)][]
}
xmat

