###
# training caretEnsemble model one vs all approach
###

library(caretEnsemble)
# add any model specific package library commands
library(pROC)

# set working directory
WORK.DIR <- "./src/caretEnsemble_model"  # modify to specify directory to contain model artififacts

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# set caretEnsemble training parameters
ENS.MODELS <- c('gbm', 'rf',"glm","svmRadial")

MODEL.COMMENT <- "caretEnsemble one vs all"

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

library(doSNOW)
cl <- makeCluster(5,type="SOCK")
registerDoSNOW(cl)

# train the model
Sys.time()
set.seed(825)

ENS.TRAIN.CTRL <- trainControl(savePredictions=TRUE,
                               classProbs=TRUE,
                               index=createMultiFolds(train.data$response,5,1),
                               summaryFunction=twoClassSummary)

time.data <- system.time(all.classes <-lapply(PRODUCT.CLASSES,trainEnsembleForOneClass,
                                          train.data$predictors,
                                          train.data$response))

names(all.classes) <- PRODUCT.CLASSES
comment(all.classes) <- MODEL.COMMENT

time.data
stopCluster(cl)

# prepare data for testing
test.data <- prepModelData(test.raw)

# for each classes predict probability of for that class
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,all.classes,test.data$predictors)
names(ll) <- PRODUCT.CLASSES

pred.probs <- do.call(cbind,ll)

score <- logLossEval(pred.probs,test.raw$target)
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



