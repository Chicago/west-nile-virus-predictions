

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
# wtf(mat)
# wtf(mat_sum)
# wtf(mat_length)
# idTable[id=='id188']
# idTable[trap=='T904']
# oracle_traps[TRAP_NAME=="T904"][order(TRAP_ACTIVATE_DATE)]
# wnv[trap=='T904',.N,keyby=list(date)]
# sum((wday(mat$date) * n_valid)) / sum(n_valid)


##------------------------------------------------------------------------------
## CREATE CV FOLDS
##------------------------------------------------------------------------------

## For named lists it works better if group is a character value
dat[ , grp := paste0("year", year(date))]

##------------------------------------------------------------------------------
## CREATE XMAT
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
## Model 2
##------------------------------------------------------------------------------
m2 <- glmer(wnv ~ wnvw1 + wnvw2 + wnv_ytd + 
                awnd + tmax + prcp + (1 | id) + (1 | week) , 
            family = binomial, data = xmat[grp!="year2016"])
display(m2)
ranef(m2)
# undebug(lme4:::mkNewReTrms)
tmpl <- lme4:::predict.merMod(m2, xmat, type = c("link", "response")[1])
tmpr <- lme4:::predict.merMod(m2, xmat, type = c("link", "response")[2])
# hist(exp(tmpl)/(1+exp(tmpl)))
# hist(tmpr)
xmat[as.integer(names(tmpr)) , m2 := unname(tmpr)]
rm(tmpr, tmpl)
# boxplot(m2 ~ wnv, xmat)
xmat[ , calculate_metrics(wnv, m2)]

## AUC for overall model
xmat[grp=="year2016" , calculate_metrics(wnv, m2)]

## AUC for predicting just the second week
xmat[grp=="year2016" , calculate_metrics(wnvwnv, m2)]

# matwnv <- dcast(xmat[ , list(wnv, id, date)],
#                 date ~ id, value.var = "wnv")
# wtf(matwnv)
# matm2 <- dcast(xmat[ , list(m2, id, date)],
#                 date ~ id, value.var = "m2")
# wtf(matm2)


## TEST OF DATA WITH PARTIAL FACTOR LEVELS
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
## Model 3
##------------------------------------------------------------------------------
options(warn = -1)
m3 <- glmer(wnvwnv ~ wnvw1 + wnvw2 + wnv_ytd + 
                awnd + tmax + prcp + (1 | id) + (1 | week) , 
            family = binomial, data = xmat[grp!="year2016" & grp != "year2009"])
display(m3)
ranef(m3)
options(warn = 0)
# undebug(lme4:::mkNewReTrms)
tmpl <- lme4:::predict.merMod(m3, xmat, type = c("link", "response")[1])
tmpr <- lme4:::predict.merMod(m3, xmat, type = c("link", "response")[2])

# hist(exp(tmpl)/(1+exp(tmpl)))
# hist(tmpr)
xmat[as.integer(names(tmpr)) , m3 := unname(tmpr)]
# boxplot(m2 ~ wnv, xmat)
xmat[ , calculate_metrics(wnvwnv, m3)]

## AUC for overall model
xmat[grp=="year2016" , calculate_metrics(wnv, m3)]

## AUC for predicting just the second week
xmat[grp=="year2016" , calculate_metrics(wnvwnv, m3)]

# matwnv <- dcast(xmat[ , list(wnv, id, date)],
#                 date ~ id, value.var = "wnv")
# wtf(matwnv)
# matm2 <- dcast(xmat[ , list(m2, id, date)],
#                 date ~ id, value.var = "m2")
# wtf(matm2)

## Add in model forecast
xmat_f <- xmat[ , list(wnvw1 = wnv,
                       wnvw2 = wnvw1,
                       wnv_ytd = wnv_ytd + wnvnum,
                       awnd, tmax, prcp, id, week)]
tmpr_f <- lme4:::predict.merMod(m3, xmat_f, type = c("link", "response")[2])
xmat[as.integer(names(tmpr_f)) , m3_forecast := unname(tmpr_f)]
# boxplot(m2 ~ wnv, xmat)
xmat[ , calculate_metrics(wnvwnv, m3_forecast)]
xmat[!is.na(wnvwnv_f1) , calculate_metrics(wnvwnv_f1, m3_forecast)]

##------------------------------------------------------------------------------
## Model 3a (Business Rule)
##------------------------------------------------------------------------------
options(warn = -1)
m3a <- glmer(wnvwnv ~ wnvw1 + wnvw2 + wnv_ytd + 
                awnd + tmax + prcp + (1 | id) + (1 | week) , 
             family = binomial, data = xmat[date < labor_day][grp!="year2016" & grp != "year2009"])
display(m3a)
ranef(m3a)
options(warn = 0)
# undebug(lme4:::mkNewReTrms)
tmpl <- lme4:::predict.merMod(m3a, xmat, type = c("link", "response")[1], allow.new.levels = TRUE)
tmpr <- lme4:::predict.merMod(m3a, xmat, type = c("link", "response")[2], allow.new.levels = TRUE)

# hist(exp(tmpl)/(1+exp(tmpl)))
# hist(tmpr)
xmat[as.integer(names(tmpr)) , m3a := unname(tmpr)]
# boxplot(m2 ~ wnv, xmat)
xmat[ , calculate_metrics(wnvwnv, m3a)]

## AUC for overall model
xmat[grp=="year2016" , calculate_metrics(wnv, m3a)]

## AUC for predicting just the second week
xmat[grp=="year2016" , calculate_metrics(wnvwnv, m3a)]

# matwnv <- dcast(xmat[ , list(wnv, id, date)],
#                 date ~ id, value.var = "wnv")
# wtf(matwnv)
# matm2 <- dcast(xmat[ , list(m2, id, date)],
#                 date ~ id, value.var = "m2")
# wtf(matm2)

## Add in model forecast
tmpr_f <- lme4:::predict.merMod(m3a, xmat_f, type = c("link", "response")[2], allow.new.levels = TRUE)
xmat[as.integer(names(tmpr_f)) , m3a_forecast := unname(tmpr_f)]
# boxplot(m2 ~ wnv, xmat)
xmat[ , calculate_metrics(wnvwnv, m3a_forecast)]
xmat[!is.na(wnvwnv_f1) , calculate_metrics(wnvwnv_f1, m3a_forecast)]

