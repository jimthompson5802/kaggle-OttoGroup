###
# create training, calibration and test data sets for model stacking 
###

library(caret)


data.dir <- "./data"


raw <- read.csv(unz(paste0(data.dir,"/train.csv.zip"),"train.csv"),stringsAsFactors=FALSE)

table(raw$target)/nrow(raw)

# create the partitions
set.seed(13)
idx <- createDataPartition(raw$target, p = 0.9, list=FALSE)

# extract subset for training
train.set <- raw[idx,]

# split into two parts for training the different levels of the model stack
set.seed(29)
idx2 <- createDataPartition(train.set$target, p = 0.5, list=FALSE)

train1.raw <- train.set[idx2,]      # training data for level 1
train2.raw <- train.set[-idx2,]     # training data for level 2

table(train1.raw$target)/nrow(train1.raw)
table(train2.raw$target)/nrow(train2.raw)

# extract subset for calib and test data sets
calib.test.set <- raw[-idx,]

# split evenly into calib and test sets
set.seed(123)
idx2 <- createDataPartition(calib.test.set$target, p = 0.5, list=FALSE)

calib.raw <- calib.test.set[idx2,]
test.raw <- calib.test.set[-idx2,]

table(calib.raw$target)/nrow(calib.raw)
table(test.raw$target)/nrow(test.raw)

rm(raw,train.set,calib.test.set)

#save training, calibration and test data sets
save(train1.raw,train2.raw,calib.raw,test.raw,file=paste0(data.dir,"/stack_train_calib_test.RData"))
