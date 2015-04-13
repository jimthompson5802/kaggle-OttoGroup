###
# training random forest model
###

library(caret)
library(kernlab)

# Common Functions and Global variables
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/svm_model"
MODEL.METHOD <- "svmLinear"

tr.ctrl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats=1,
    verboseIter = TRUE,
    classProbs=TRUE,
    summaryFunction=caretLogLossSummary,
    allowParallel=FALSE)

# tune.grid <-  expand.grid(interaction.depth = c(1, 3, 5),
#                         n.trees = (1:10)*50,
#                         shrinkage = 0.1)


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
idx <- sample(nrow(train.raw),0.2*nrow(train.raw))
train.df <- train.raw[idx,]

# eliminate near zero Variance
train.df <- train.df[,setdiff(names(train.df),c(nz.vars,"id"))]
train.df$target <- factor(train.df$target)


Sys.time()
set.seed(825)
time.data <- system.time(svmFit1 <- train(train.df[,1:(ncol(train.df)-1)],
                 train.df[,ncol(train.df)],
                 method = MODEL.METHOD,
                 preProcess=c("center","scale"),

                 ## model specific parameters, if any
                 
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

# record Model performance
modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              train.df[,1:(ncol(train.df)-1)],
                              score,data.frame(sigma=NA,svmFit1$bestTune))
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
    file.name <- paste0("/svmFit1_",modPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(svmFit1,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



