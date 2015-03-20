###
# create training, calibration and test data sets
###

library(caret)


data.dir <- "./data"


raw <- read.csv(unz(paste0(data.dir,"/train.csv.zip"),"train.csv"),stringsAsFactors=FALSE)

table(raw$target)/nrow(raw)

# create the partitions
idx <- createDataPartition(raw$target, p = 0.6, list=FALSE)

train.raw <- raw[idx,]

table(train.raw$target)/nrow(train.raw)

raw <- raw[-idx,]

idx <- createDataPartition(raw$target, p = 0.5, list=FALSE)

calib.raw <- raw[idx,]
test.raw <- raw[-idx,]

table(calib.raw$target)/nrow(calib.raw)
table(test.raw$target)/nrow(test.raw)
rm(raw)

#save training, calibration and test data sets
save(train.raw,calib.raw,test.raw,file=paste0(data.dir,"/train_calib_test.RData"))
