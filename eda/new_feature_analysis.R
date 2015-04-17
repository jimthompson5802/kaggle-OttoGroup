###
# generate new features and perform analysis
###

source("./src/CommonFunctions.R")


# load training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
rm(calib.raw,test.raw)


# generate some new features
features <- setdiff(names(train.raw),c("id","target"))

# number of non-zero features
non.zero.count <- apply(train.raw[features],1,function(r){sum(r>0)})
boxplot(non.zero.count~train.raw$target)

# sum of counts
sum.of.counts <- apply(train.raw[features],1,sum)
boxplot(sum.of.counts~train.raw$target)

# max of counts
max.of.counts <- apply(train.raw[features],1,max)
boxplot(max.of.counts~train.raw$target)
