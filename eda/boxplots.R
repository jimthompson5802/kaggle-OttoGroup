###
# Boxplots
###

library(caret)
library(ggplot2)
library(plyr)

data.dir <- "./data"

load(file=paste0(data.dir,"/train_calib_test.RData"))

boxplot(train.raw$feat_62~train.raw$target)

qplot(target,feat_62,data=train.raw,geom="boxplot")

by(train.raw[,2:(ncol(train.raw)-1)],train.raw$target,summary)

