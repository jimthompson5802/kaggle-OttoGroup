###
# initial status
###

library(caret)

data.dir <- "./data"

load(file=paste0(data.dir,"/train_calib_test.RData"))

predictors <- train.raw[setdiff(names(train.raw),c("id","target"))]

nz <- nearZeroVar(predictors, saveMetrics= TRUE)

nzidx <- nearZeroVar(predictors)

nz.vars <- names(predictors)[nzidx]

save(nz.vars,file=paste0(data.dir,"/near_zero_vars.RData"))

correlations <- cor(predictors)

coridx <- findCorrelation(correlations,cutoff=0.8)

corr.vars <- names(predictors)[coridx]

save(corr.vars, file=paste0(data.dir,"/correlated_vars.RData"))

