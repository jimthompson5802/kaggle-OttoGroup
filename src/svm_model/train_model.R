###
# training svm model
###

library(caret)
# add any model specific package library commands
library(kernlab)

# set working directory
WORK.DIR <- "./src/svm_model"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# set caret training parameters
CARET.TRAIN.PARMS <- list(method="svmLinear",  # Replace MODEL.METHOD with appropriate caret model
                          preProcess=c("center","scale"))   

# CARET.TUNE.GRID <-  NULL  # NULL provides model specific default tuning parameters

# user specified tuning parameters
CARET.TUNE.GRID <- expand.grid(C=10^(-5:0))

# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=1,
                                 verboseIter=TRUE,
                                 classProbs=TRUE,
#                                  allowParallel=FALSE,
                                 summaryFunction=caretLogLossSummary)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                            maximize=FALSE,
                           tuneGrid=CARET.TUNE.GRID,
#                            tuneLength=5,
                           metric="LogLoss")

MODEL.SPECIFIC.PARMS <- list(tol=1e-4)  #NULL # Other model specific parameters

MODEL.COMMENT <- "preProcess(center and scale)"

# amount of data to train
FRACTION.TRAIN.DATA <- 0.1



# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
     
# extract subset for inital training
set.seed(29)
idx <- sample(nrow(train.raw),FRACTION.TRAIN.DATA*nrow(train.raw))
train.df <- train.raw[idx,]

# prepare data for training
train.data <- prepModelData(train.df)

# library(doMC)
# registerDoMC(cores = 5)

library(doSNOW)
cl <- makeCluster(5,type="SOCK")
registerDoSNOW(cl)
clusterExport(cl,list("logLossEval"))

# train the model
Sys.time()
set.seed(825)

time.data <- system.time(mdl.fit <- do.call(train,c(list(train.data$predictors,
                                                         train.data$response),
                                                    CARET.TRAIN.PARMS,
                                                    MODEL.SPECIFIC.PARMS,
                                                    CARET.TRAIN.OTHER.PARMS)))

time.data
mdl.fit

stopCluster(cl)

# prepare data for training
test.data <- prepModelData(test.raw)

pred.probs <- predict(mdl.fit,newdata = test.data$predictors,type = "prob")

score <- logLossEval(pred.probs,test.data$response)
score

# determine if score improved
improved <- ifelse(score < min(modelPerf.df$score),"Yes","No")

# record Model performance
modelPerf.df <- recordModelPerf(modelPerf.df,
                              mdl.fit$method,
                              time.data,
                              train.data$predictors,
                              score,
                              improved=improved,
                              bestTune=flattenDF(mdl.fit$bestTune),
                              tune.grid=flattenDF(CARET.TUNE.GRID),
                              model.parms=paste(names(MODEL.SPECIFIC.PARMS),
                                                as.character(MODEL.SPECIFIC.PARMS),
                                                sep="=",collapse=","),
                              comment=MODEL.COMMENT)
save(modelPerf.df,file=paste0(WORK.DIR,"/modelPerf.RData"))

#display model performance record for this run
tail(modelPerf.df[,1:10],1)

# if last score recorded is better than previous ones save model object
last.idx <- length(modelPerf.df$score)
if (last.idx == 1 || improved == "Yes") {
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save generated model
    file.name <- paste0("/model_",mdl.fit$method,"_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(mdl.fit,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



