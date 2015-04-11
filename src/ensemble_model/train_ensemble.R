###
#  train ensemble model by calculating weight factors
###


library(caret)
library(gbm)
library(randomForest)

# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/ensemble_model"

# model performance Comments to captuer
MODEL.METHOD <- "weighted ensemble"
FEATURES <- c("models in the ensemble",
            "gbm_model/gbmFit1_2015-04-07_21_12_42.RData",
             "rf_model/rfFit1_2015-04-09_23_06_33.RData",
             sep=", ")

# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

# get calibration data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

# load model performance data
load(paste0(WORK.DIR,"/modPerf.RData"))
#
# make gbm prediction
#

# read calibration data set
new.df <- calib.raw

#save id vector
id <- new.df$id

# prep the data for submission
new.df <- new.df[,setdiff(names(new.df),c(nz.vars,"id"))]

# retrive gbm model
load("./src/gbm_model/gbmFit1_2015-04-07_21_12_42.RData")

# predict class probabilities
gbm.probs <- predict(gbmFit1,newdata = new.df,type = "prob")

# combine with id
gbm.probs <- data.frame(id,gbm.probs)

#
# make rf prediction
#

# read calibaration data set
new.df <- calib.raw

#save id vector
id <- new.df$id

# prep the data for submission
new.df <- new.df[,setdiff(names(new.df),c("id"))]

# retrive rf model
load("./src/rf_model/rfFit1_2015-04-09_23_06_33.RData")

# predict class probabilities
rf.probs <- predict(rfFit1,newdata = new.df,type = "prob")

# recombine with id
rf.probs <- data.frame(id,rf.probs)

#
# determine optimal weighting factor for combining model estimates
#
makeEnsembleFunction <- function(target,gbm.probs, rf.probs) {
    function(w) {
        pred.probs <- w*gbm.probs + (1-w)*rf.probs
        logLossEval(pred.probs,target)
    }
}

ensFunc <- makeEnsembleFunction(calib.raw$target,gbm.probs[,2:10],rf.probs[2:10])

wt <- 0.5

opt.wts <- optim(0.5,ensFunc,method="L-BFGS-B",lower=0,upper=1)

score <- opt.wts$value

#
# record Model performance
#

ensemble.weights <- c(opt.wts$par,1-opt.wts$par)

model.weights <- paste(c("gbmFit1_2015-04-07_21_12_42.RData","rfFit1_2015-04-09_23_06_33.RData"),ensemble.weights,sep="=",collapse=",")
bestTune <- data.frame(model.weights, stringsAsFactors=FALSE)

# set up dummy data structures to account for recrodModelPerf() function
time.data <- system.time(dummy.df <- data.frame())

# record performance
modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              dummy.df,
                              score,bestTune)
save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))

#display model performance record for this run
tail(modPerf.df[,1:(ncol(modPerf.df)-1)],1)

# if last score recorded is better than previous ones save model object
last.idx <- length(modPerf.df$score)
if (last.idx == 1 ||
        modPerf.df$score[last.idx] < min(modPerf.df$score[1:(last.idx-1)])) {
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save generated model
    file.name <- paste0("/ensembleWeights_",modPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(ensemble.weights,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}

