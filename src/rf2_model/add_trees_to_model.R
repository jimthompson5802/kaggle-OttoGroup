###
#  add trees to an existing rf model in parallel
###

library(foreach)
library(randomForest)

# set up global variables and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf2_model"
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))


PARALLEL.WORKERS <- 5
NEW.RF.TREES <- 200
MTRY <- 48
MODEL.SPECIFIC.PARMS <- NULL
MODEL.COMMENT <- paste("Adding trees to model",PARALLEL.WORKERS * NEW.RF.TREES)

# load orignal rf model
load(paste0(WORK.DIR,"/model_parRF_2015-04-24_06_49_26.RData"))

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
time.data <- system.time({new.rf <- foreach(ntree=rep(NEW.RF.TREES, PARALLEL.WORKERS), .combine=combine, .packages='randomForest') %dopar% 
                             randomForest(train.data$predictors, train.data$response, ntree=ntree,mtry=MTRY)
#combine rf models
mdl.fit <- combine(mdl.fit, new.rf)})

time.data

# make prediction on test data set
system.time(pred.probs <- predictInParallel(mdl.fit,test.raw,PARALLEL.WORKERS))

score <- logLossEval(pred.probs[,2:10],test.data$response)
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
                                bestTune=flattenDF(data.frame(mtry=MTRY, workers=PARALLEL.WORKERS, new.tree=NEW.RF.TREES)),
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
    #yes we have improvement or first score, save generated model
    file.name <- paste0("/model_parRF_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(mdl.fit,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



