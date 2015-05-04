###
#  build rf model in parallel
#  This uses the native randomForest functions
#  uses all data for training (train.raw,calib.raw and test.raw)
###

library(foreach)
library(randomForest)

# set up global variables and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf2_model"
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))


PARALLEL.WORKERS <- 6
RF.TREES <- 1000
MTRY <- 48
MODEL.SPECIFIC.PARMS <- NULL
MODEL.COMMENT <- paste("Growing trees in parallel workers",PARALLEL.WORKERS,
                       "each with",RF.TREES,"trees")

# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

train.raw <- rbind(train.raw,calib.raw,test.raw)

# prepare data for training
train.data <- prepModelData(train.raw)

# add trees to model
library(doMC)
registerDoMC(cores = PARALLEL.WORKERS)
Sys.time()
set.seed(13)
time.data <- system.time(mdl.fit <- foreach(ntree=rep(RF.TREES, PARALLEL.WORKERS), .combine=combine, .packages='randomForest') %dopar% 
                             randomForest(train.data$predictors, train.data$response, ntree=ntree,mtry=MTRY,norm.votes=FALSE))

time.data

#make prediction on test data set - note this will give overly optimistic score
system.time(pred.probs <- predictInParallel(mdl.fit,test.raw,PARALLEL.WORKERS))

score <- logLossEval(pred.probs[,2:10],test.raw$target)
score

# determine if score improved
improved <- ifelse(score < min(modelPerf.df$score),"Yes","No")

# record Model performance
modelPerf.df <- recordModelPerf(modelPerf.df,
                                "parallel_rf",
                                time.data,
                                train.data$predictors,
                                score,
                                improved=improved,
                                bestTune=flattenDF(data.frame(mtry=MTRY, workers=PARALLEL.WORKERS, each.tree=RF.TREES)),
                                tune.grid=flattenDF(data.frame(total.ntree=mdl.fit$ntree)),
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
#     #yes we have improvement or first score, save generated model
    file.name <- paste("model_rf_all_data_ntree_",mdl.fit$ntree,".RData",sep="")#     
    save(mdl.fit,file=paste(WORK.DIR,file.name,sep="/"))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



