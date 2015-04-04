###
# training gbm model
###

library(caret)
library(gbm)

# Common Functions and Global variables
source("./src/CommonFunctions.R")


# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
     
# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))


# extract subset for inital training
set.seed(29)
idx <- sample(nrow(train.raw),0.10*nrow(train.raw))
train <- train.raw[idx,]

# eliminate near zero Variance
train <- train[,setdiff(names(train),c(nz.vars,"id"))]

tr.ctrl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10)

set.seed(825)
gbmFit1 <- train(target ~ ., data = train,
                 method = "gbm",
                 trControl = tr.ctrl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

