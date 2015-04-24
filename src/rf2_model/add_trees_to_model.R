###
#  add trees to model
#  This code adds trees to a random forest model created with caret
###

library(caret)
library(randomForest)

# set up global variables and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf2_model"
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

TREES.TO.ADD <- 1000
MODEL.COMMENT <- paste("Growing trees by",TREES.TO.ADD)

# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# load caret model structure
load(paste0(WORK.DIR,"/model_rf_2015-04-23_21_34_24.RData"))

# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

train.raw <- rbind(train.raw,calib.raw)

# prepare data for training
train.data <- prepModelData(train.raw)

# add trees to model
Sys.time()
time.data <- system.time({rf.new <- randomForest(train.data$predictors,train.data$response,
                                                 ntree=TREES.TO.ADD)
                          mdl.fit$finalModel <- combine(mdl.fit$finalModel,rf.new)})


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



