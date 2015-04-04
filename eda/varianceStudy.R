###
# initial status
###

library(caret)

data.dir <- "./data"

load(file=paste0(data.dir,"/train_calib_test.RData"))

nz <- nearZeroVar(train.raw[,2:(ncol(train.raw)-1)], saveMetrics= TRUE)

nzidx <- nearZeroVar(train.raw[,2:(ncol(train.raw)-1)])

nz.vars <- names(train.raw[,2:(ncol(train.raw)-1)])[nzidx]

save(nz.vars,file=paste0(data.dir,"/near_zero_vars.RData"))

