###
# training randomForest model
###

library(caret)
library(randomForest)

# Common Functions and Global variables
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf_model"

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
     
# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

library(doMC)
registerDoMC(cores = 5)

# extract subset for inital training
set.seed(29)
idx <- sample(nrow(train.raw),0.1*nrow(train.raw))
train.df <- train.raw[idx,]

# eliminate near zero Variance
train.df <- train.df[,setdiff(names(train.df),c(nz.vars,"id"))]
train.df$target <- factor(train.df$target)

tr.ctrl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats=1,
    verboseIter = TRUE,
    classProbs=TRUE,
    summaryFunction=caretLogLossSummary)

tune.grid <-  expand.grid(mtry=seq(2,81,5))

set.seed(825)
system.time(rfFit1 <- train(train.df[,1:(ncol(train.df)-1)],
                 train.df[,ncol(train.df)],
                 method = "rf",

                 ## This last option is actually one
                 ##  that passes through to model
                 
                 
                 ## remaining train options
                 trControl = tr.ctrl,
                 tuneGrid=tune.grid,
                 maximize=FALSE,
                 metric="LogLoss"))
rfFit1


# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c(nz.vars,"id"))]

pred.probs <- predict(rfFit1,newdata = test[,1:(ncol(test)-1)],type = "prob")

logLossEval(pred.probs,test$target)

#save generated model
# save(rfFit1,file=paste0(WORK.DIR,"/rfFit1.RData"))



