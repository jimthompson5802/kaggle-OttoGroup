###
#  build rf model in parallel
#  This code adds trees to a random forest model created with caret
###

library(foreach)
library(randomForest)

# set up global variables and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf2_model"
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))


PARALLEL.WORKERS <- 5
RF.TREES <- 1000
MTRY <- 48
MODEL.SPECIFIC.PARMS <- NULL
MODEL.COMMENT <- paste("Growing trees in parallel workers",PARALLEL.WORKERS,
                       "each with",RF.TREES,"trees")

# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

train.raw <- rbind(train.raw,calib.raw)

# prepare data for training
train.data <- prepModelData(train.raw)

# add trees to model
library(doMC)
registerDoMC(cores = PARALLEL.WORKERS)
Sys.time()
time.data <- system.time(rf <- foreach(ntree=rep(RF.TREES, PARALLEL.WORKERS), .combine=combine, .packages='randomForest') %dopar% 
                             randomForest(train.data$predictors, train.data$response, ntree=ntree,mtry=MTRY))


# prepare data for training
test.data <- prepModelData(test.raw)

pred.probs <- predict(rf,newdata = test.data$predictors,type = "prob")

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
                                bestTune=flattenDF(data.frame(mtry=MTRY, workers=PARALLEL.WORKERS, each.tree=RF.TREES)),
                                tune.grid=flattenDF(NULL),
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



