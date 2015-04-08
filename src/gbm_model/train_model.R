###
# training gbm model
###

library(caret)
library(gbm)

# Common Functions and Global variables
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/gbm_model"
MODEL.METHOD <- "gbm"

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
idx <- sample(nrow(train.raw),0.3*nrow(train.raw))
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

tune.grid <-  expand.grid(interaction.depth = c(3, 5, 7, 9),
                        n.trees = c(1000, 2000, 3000),
                        shrinkage = 0.01)  # avoid numerical issue in gbm

Sys.time()
set.seed(825)
time.data <- system.time(gbmFit1 <- train(train.df[,1:(ncol(train.df)-1)],
                 train.df[,ncol(train.df)],
                 method = MODEL.METHOD,

                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE,
                 
                 ## remaining train options
                 trControl = tr.ctrl,
                 maximize=FALSE,
                 tuneGrid=tune.grid,
                 metric="LogLoss"))
time.data
gbmFit1


# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c(nz.vars,"id"))]

pred.probs <- predict(gbmFit1,newdata = test[,1:(ncol(test)-1)],type = "prob")

score <- logLossEval(pred.probs,test$target)
score

# record Model performance
modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              train.df[,1:(ncol(train.df)-1)],
                              score,gbmFit1$bestTune)
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
    file.name <- paste0("/gbmFit1_",modPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(gbmFit1,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}




