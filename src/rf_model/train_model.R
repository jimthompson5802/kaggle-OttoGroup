###
# training random forest model
###

library(caret)
library(randomForest)

# Common Functions and Global variables
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf_model"
MODEL.METHOD <- "rf"
RF.NTREE <- 3000   #custom parameter for randomForest
FRACTION.TRAINING.DATA <- 1.0

# load model performance data
load(paste0(WORK.DIR,"/modPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
     
# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

library(doMC)
registerDoMC(cores = 5)

# extract subset for inital training
set.seed(29)
idx <- sample(nrow(train.raw),FRACTION.TRAINING.DATA*nrow(train.raw))
train.df <- train.raw[idx,]

# eliminate identifier
train.df <- train.df[,setdiff(names(train.df),c("id"))]

#set target as factor to build classifcation 
train.df$target <- factor(train.df$target)

tr.ctrl <- trainControl(
#     method = "repeatedcv",
    method="none",
    number = 5,
    repeats=1,
    verboseIter = TRUE,
    classProbs=TRUE,
    summaryFunction=caretLogLossSummary)

tune.grid <-  expand.grid(mtry=47)

Sys.time()
set.seed(825)
time.data <- system.time(rfFit1 <- train(train.df[,1:(ncol(train.df)-1)],
                 train.df[,ncol(train.df)],
                 method = MODEL.METHOD,

                 ## model specific parameters, if any
                 ntree=RF.NTREE,
                 
                 ## remaining train options
                 trControl = tr.ctrl,
                 maximize=FALSE,
                 tuneGrid=tune.grid,
                 metric="LogLoss"))
time.data
rfFit1


# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c("id"))]

pred.probs <- predict(rfFit1,newdata = test[,1:(ncol(test)-1)],type = "prob")

score <- logLossEval(pred.probs,test$target)
score

# record Model performance
modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              train.df[,1:(ncol(train.df)-1)],
                              score,rfFit1$bestTune,
                              ntree=RF.NTREE)
save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))

#display model performance record for this run
modPerf.df[nrow(modPerf.df),1:(ncol(modPerf.df)-1)]

# if last score recorded is better than previous ones save model object
last.idx <- length(modPerf.df$score)
if (last.idx == 1 ||
        modPerf.df$score[last.idx] < min(modPerf.df$score[1:(last.idx-1)])) {
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save generated model
    file.name <- paste0("/rfFit1_",modPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(rfFit1,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



