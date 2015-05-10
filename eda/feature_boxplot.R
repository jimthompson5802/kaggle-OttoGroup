###
# generate boxplot by feature
###

library(ggplot2)
library(tidyr)
library(caret)
library(dplyr)

DATA.DIR <- "./data"

source("./src/CommonFunctions.R")

load(paste0(DATA.DIR,"/train_calib_test.RData"))

idx <- createDataPartition(train.raw$target,p=0.005,list=FALSE)

train.raw <- train.raw[idx,]

predictor.vars <- setdiff(names(train.raw), c("id","target"))

train.raw$non.zero.count <- apply(train.raw[,predictor.vars], 1, function(row){sum(row>0)})
train.raw$count.total <- apply(train.raw[,predictor.vars],1,sum)

diff.features <- calcPairwiseDiff(predictor.vars,train.raw)

train.raw <- cbind(train.raw,diff.features)

predictor.vars <- c(predictor.vars,"non.zero.count","count.total",colnames(diff.features))


# convert to long format 
df <- gather(train.raw, feature, value, one_of(predictor.vars))

df$feature.id <- do.call(rbind,strsplit(as.character(df$feature),"_"))[,2]
df$class.id <- do.call(rbind,strsplit(df$target,"_"))[,2]

df2 <- select(df,starts_with("feat_"))

png("./eda/feature_boxplot.png",height=17, width=11, units="in",res=300)
qplot(class.id,value,data=df2,geom="boxplot") + 
    facet_wrap(~feature,ncol=8,scales="free_y") +
    xlab("Class")
dev.off()
