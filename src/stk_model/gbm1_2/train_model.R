###
# training gbm model - one vs all apprach
###

library(caret)
library(gbm)
library(pROC)

# set working directory
WORK.DIR <- "./src/stk_model/gbm1_2"

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source("./src/stk_model/StackCommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))


# set caret training parameters
CARET.TRAIN.PARMS <- list(method="gbm")

CARET.TUNE.GRID <-  NULL #expand.grid(C=c(0.1,0.01))
# CARET.TUNE.GRID <- expand.grid(interaction.depth = c(3, 5, 7, 9, 11),
#                                n.trees = c(100,200,300,400,500,600),
#                                shrinkage = 0.1)

CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=1,
                                 verboseIter=TRUE,
                                 classProbs=TRUE,
                                 summaryFunction=twoClassSummary)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
#                                 maximize=FALSE,
                                tuneGrid=CARET.TUNE.GRID,
                                tuneLength=5,
                                metric="ROC")

MODEL.SPECIFIC.PARMS <- list(verbose=FALSE)

MODEL.COMMENT <- "gbm one vs all Level 2 Model"

# amount of data to train
FRACTION.TRAIN.DATA <- 1.0


# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/stack_train_calib_test.RData"))

library(doMC)
registerDoMC(cores = 5)

# extract subset for inital training
set.seed(29)
idx <- createDataPartition(train2.raw$target, p = FRACTION.TRAIN.DATA, list=FALSE)
train.df <- train2.raw[idx,]

# prepare data for modeling
train.data <- prepModelData(train.df)

# get predictions from Level 1 models
level1.features <- runAllLevel1Models(train.df)

# combine level 1 predictions with current training data
train.data$predictors <- cbind(level1.features,train.data$predictors)

Sys.time()
set.seed(825)
time.data <- system.time(gbm.mdls<-lapply(PRODUCT.CLASSES,trainForOneClass,
                                          train.data$predictors,
                                          train.data$response))
time.data
names(gbm.mdls) <- PRODUCT.CLASSES
comment(gbm.mdls) <- MODEL.COMMENT


# prepare data for testing
test.data <- prepModelData(test.raw)

# get predictions from Level 1 models
level1.features <- runAllLevel1Models(test.raw)

# combine level 1 predictions with current training data
test.data$predictors <- cbind(level1.features,test.data$predictors)

# for each classes predict probability of for that class
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,test.data$predictors)
names(ll) <- PRODUCT.CLASSES

pred.probs <- do.call(cbind,ll)

score <- logLossEval(pred.probs,test.raw$target)
score

# determine if score improved
improved <- ifelse(score < min(modelPerf.df$score),"Yes","No")

# record Model performance
ll <- llply(gbm.mdls,function(x){print(x$bestTune)})
bestTune <- do.call(cbind,ll)

# record Model performance
modelPerf.df <- recordModelPerf(modelPerf.df,
                                gbm.mdls[[1]]$method,
                                time.data,
                                train.data$predictors,
                                score,
                                improved=improved,
                                bestTune=flattenDF(bestTune),
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
if (last.idx == 1 || improved == "Yes") {  #force saving the model file
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save generated model
    file.name <- paste0("/model_",gbm.mdls[[1]]$method,"_one_vs_all_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(gbm.mdls,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}




