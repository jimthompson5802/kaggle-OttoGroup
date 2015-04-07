###
# training svm model
###

library(caret)
library(kernlab)

# Common Functions and Global variables
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/svm_model"

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
    number = 5,
    repeats=1,
    verboseIter = TRUE,
    classProbs=TRUE,
    summaryFunction=caretLogLossSummary)

# tune.grid <-  expand.grid(interaction.depth = c(1, 3, 5, 7, 9),
#                         n.trees = (1:10)*50,
#                         shrinkage = 0.1)

Sys.time()
set.seed(825)
time.data <- system.time(svmFit1 <- train(train.df[,1:(ncol(train.df)-1)],
                 train.df[,ncol(train.df)],
                 method = "svmRadial",

                 ## This last option is actually one
                 ## for gbm() that passes through
                 
                 ## remaining train options
                 trControl = tr.ctrl,
                 maximize=FALSE,
#                  tuneGrid=tune.grid,
                 metric="LogLoss"))
time.data
svmFit1


# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c(nz.vars,"id"))]

pred.probs <- predict(svmFit1,newdata = test[,1:(ncol(test)-1)],type = "prob")

score <- logLossEval(pred.probs,test$target)
score

# save generated model
# save(svmFit1,file=paste0(WORK.DIR,"/svmFit1.RData"))



