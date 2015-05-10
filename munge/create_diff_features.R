###
#  create Diff features
###

library(caret)
library(dplyr)


source("src/CommonFunctions.R")

load(paste0(DATA.DIR,"/train_calib_test.RData"))

# diff features for training data
d.train.raw <- calcPairwiseDiff(paste("feat",1:93,sep="_"),train.raw)
d.train.raw <- data.frame(id=train.raw$id,d.train.raw,target=train.raw$target,
                          stringsAsFactors=FALSE)

features <- setdiff(names(d.train.raw),c("id","target"))

# zeor/near zero variance features
idx <- nearZeroVar(select(d.train.raw,one_of(features)))
d.nz.features <- names(select(d.train.raw,one_of(features)))[idx]

# correlated features
idx <- findCorrelation(cor(select(d.train.raw,one_of(features))))
d.cor.features <- names(select(d.train.raw,one_of(features)))[idx]

# diff features for calibration data set
d.calib.raw <- calcPairwiseDiff(paste("feat",1:93,sep="_"),calib.raw)
d.calib.raw <- data.frame(id=calib.raw$id,d.calib.raw,target=calib.raw$target,
                     stringsAsFactors=FALSE)

# diff features for test data set 
d.test.raw <- calcPairwiseDiff(paste("feat",1:93,sep="_"),test.raw)
d.test.raw <- data.frame(id=test.raw$id,d.test.raw,target=test.raw$target,
                    stringsAsFactors=FALSE)

save(d.train.raw,d.calib.raw,d.test.raw,d.nz.features,d.cor.features,
     file=paste0(DATA.DIR,"/diff_train_calib_test.RData"))
