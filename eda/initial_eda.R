###
# initial EDA
###

library(Hmisc)

data.dir <- "./data"


load(file=paste0(data.dir,"/train_calib_test.RData"))

# summary stats
summary(train.raw)

# determine number of distict values
distinct.values <- apply(train.raw[,2:ncol(train.raw)],2,function(x){length(table(x))})

# chi-sq test
bin.df <- data.frame(lapply(train.raw[,2:(ncol(train.raw)-1)],function(x){cut2(x,quantile(x,seq(0,1,0.25)))}))
r <- apply(train.raw[,2:(ncol(train.raw)-1)],2,function(x){chisq.test(x,train.raw$target)})

sapply(r,function(x){x$p.value})
