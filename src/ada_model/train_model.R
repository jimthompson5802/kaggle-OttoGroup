###
# training ada model
###

library(caret)
library(adabag)

# Common Functions and Global variables
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/ada_model"
MODEL.METHOD <- "AdaBoost.M1"

# load model performance data
load(paste0(WORK.DIR,"/modPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
     
# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

# library(doMC)
# registerDoMC(cores = 2)
library(doSNOW)
cl <- makeCluster(5,type="SOCK")
registerDoSNOW(cl)

# extract subset for inital training
set.seed(29)
idx <- sample(nrow(train.raw),0.1*nrow(train.raw))
train.df <- train.raw[idx,]

# eliminate near zero Variance
train.df <- train.df[,setdiff(names(train.df),c("id"))]
train.df$target <- factor(train.df$target)

tr.ctrl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats=1,
    verboseIter = TRUE,
    classProbs=TRUE,
    summaryFunction=caretLogLossSummary)

TUNE.LENGTH <- NULL
# TUNE.GRID <- NULL
TUNE.GRID <-  expand.grid(coeflearn="Zhu",
                        maxdepth=c(1,5,9),
                        mfinal=(1:10)*30)

clusterExport(cl,list("logLossEval"))
Sys.time()
set.seed(825)
time.data <- system.time(adaFit1 <- train(target~.,data=train.df,
                 method = MODEL.METHOD,

                 ## other model specific options
#                  verbose = FALSE,
                 
                 ## remaining train options
                 trControl = tr.ctrl,
                 maximize=FALSE,
                 tuneGrid=TUNE.GRID,
                 tuneLength=TUNE.LENGTH,
                 metric="LogLoss"))
time.data
adaFit1

stopCluster(cl)

# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c("id"))]

pred.probs <- predict(adaFit1,newdata = test[,1:(ncol(test)-1)],type = "prob")

score <- logLossEval(pred.probs,test$target)
score

# record Model performance
modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              train.df[,1:(ncol(train.df)-1)],
                              score,adaFit1$bestTune)
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
    file.name <- paste0("/adaFit1_",modPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(adaFit1,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}




