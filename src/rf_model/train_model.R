###
# training random forest model
###

library(caret)
library(randomForest)

# Common Functions and Global variables
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf_model"
MODEL.METHOD <- "rf"

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

# tune.grid <-  expand.grid(interaction.depth = c(1, 3, 5),
#                         n.trees = (1:10)*50,
#                         shrinkage = 0.1)

Sys.time()
set.seed(825)
time.data <- system.time(rfFit1 <- train(train.df[,1:(ncol(train.df)-1)],
                 train.df[,ncol(train.df)],
                 method = MODEL.METHOD,

                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 
                 ## remaining train options
                 trControl = tr.ctrl,
                 maximize=FALSE,
#                  tuneGrid=tune.grid,
                 metric="LogLoss"))
time.data
rfFit1


# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c(nz.vars,"id"))]

pred.probs <- predict(rfFit1,newdata = test[,1:(ncol(test)-1)],type = "prob")

score <- logLossEval(pred.probs,test$target)
score

# record Model performance
load(paste0(WORK.DIR,"/modPerf.RData"))
modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              train.df[,1:(ncol(train.df)-1)],
                              score,rfFit1$bestTune)
save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))

#display model performance record for this run
modPerf.df[nrow(modPerf.df),1:(ncol(modPerf.df)-1)]


# save generated model
# save(rfFit1,file=paste0(WORK.DIR,"/rfFit1.RData"))



