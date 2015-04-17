###
# initial EDA
###

library(Hmisc)
library(plyr)

data.dir <- "./data"


load(file=paste0(data.dir,"/train_calib_test.RData"))

# summary stats
summary(train.raw)

# determine number of distict values
distinct.values <- apply(train.raw[,2:ncol(train.raw)],2,function(x){length(table(x))})
distinct.value.list <- apply(train.raw[,2:ncol(train.raw)],2,function(x){table(x)})

# chi-sq test
# bin.df <- data.frame(lapply(train.raw[,2:(ncol(train.raw)-1)],function(x){cut2(x,quantile(x,seq(0,1,0.25)))}))
r <- apply(train.raw[,2:(ncol(train.raw)-1)],2,function(x){chisq.test(x,train.raw$target,simulate.p.value=TRUE)})

sapply(r,function(x){x$p.value})

# bin the feature values and then re-run chisq test
features <- setdiff(names(train.raw),c("id","target"))
df <- as.data.frame(lapply(features,function(this.col,mydf){cut(mydf[,this.col],
                                                                 breaks=c(seq(0,10,1),Inf),right=FALSE)},
                                                                 train.raw[features]))
names(df) <- features

r1 <- apply(df,2,function(x){chisq.test(x,train.raw$target,simulate.p.value=TRUE)})
sapply(r1,function(x){x$p.value})

df <- data.frame(id=train.raw$id,df)

df3 <- dcast(df,id~feat_1,id.vars="feat_1")
