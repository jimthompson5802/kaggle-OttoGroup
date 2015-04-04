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
idx <- sample(nrow(train.raw),0.05*nrow(train.raw))
train <- train.raw[idx,]

# eliminate near zero Variance
train <- train[,setdiff(names(train),c(nz.vars,"id"))]
train$target <- factor(train$target)

tr.ctrl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 3,
    classProbs=TRUE)

set.seed(825)
gbmFit1 <- train(target ~ ., data = train,
                 method = "gbm",
                 trControl = tr.ctrl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 metric="Accuracy")
gbmFit1


# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c(nz.vars,"id"))]

pred.probs <- predict(gbmFit1,newdata = test[,1:(ncol(test)-1)],type = "prob")

