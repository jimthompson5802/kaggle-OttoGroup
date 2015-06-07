###
#  Determine weighting factors for blending the Level 2 models
###


library(caret)
library(alabama)

WORK.DIR <- "./src/stk_model/ensemble1_3"

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source("./src/stk_model/StackCommonFunctions.R")
# source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# get data to calibrate the weighting
load(paste0(DATA.DIR,"/stack_train_calib_test.RData"))

# get Level 2 model predictions
rf1_2.pred.probs <- rf1_2Predictions(calib.raw)
gbm1_2.pred.probs <- gbm1_2Predictions(calib.raw)


#
# determine optimal weighting factor for combining model estimates
#
makeEnsembleFunction <- function(target,rf1_2,gbm1_2) {
    function(w) {
        pred.probs <- w[1]*rf1_2 + w[2]*gbm1_2
        logLossEval(pred.probs,target)
    }
}

ensFunc <- makeEnsembleFunction(calib.raw$target,rf1_2.pred.probs,gbm1_2.pred.probs)

# define equality constraints
heq <- function(w) {
    h <- rep(NA,1)
    
    h[1] <- sum(w) - 1
    
    return(h)
}

heq.jac <- function(w){
    j <- matrix(NA,1,length(w))
    
    j[1,] <- c(1,1)
    
    return(j)
}

# define inequality constraints
hin <- function(w) {
    h <- rep(NA,4)
    
    for (i in 1:2) {
        h[i] <- w[i]
        h[i+2] <- 1 - w[i]
    }
    
    return(h)
    
}

hin.jac <- function(w) {
    j <- matrix(NA,4,length(w))
    
    j[1,] <- c(1,0)
    j[2,] <- c(-1,0)
    
    j[3,] <- c(0,1)
    j[4,] <- c(0,-1)
    
    return(j)
}

time.data <- system.time(opt.wts <- constrOptim.nl(c(1/2,1/2),fn=ensFunc,
                                      hin=hin, hin.jac=hin.jac,
                                      heq=heq, heq.jac=heq.jac,
#                                       control.outer=list(itmax=2),
                                      control.optim=list(trace=2)))

#
# Determine result with test data set
#

# get Level 2 model predictions for the test data set
rf1_2.pred.probs <- rf1_2Predictions(test.raw)
gbm1_2.pred.probs <- gbm1_2Predictions(test.raw)

# combine the Level 2 predictions
pred.probs <- opt.wts$par[1]*rf1_2.pred.probs + opt.wts$par[2]*gbm1_2.pred.probs

# compute log loss error
score <- logLossEval(pred.probs,test.raw$target)
score

opt.wts

ensemble.wts <- data.frame(rbind(opt.wts$par))
names(ensemble.wts) <- c("rf1_2","gbm1_2")

# determine if score improved
improved <- ifelse(score < min(modelPerf.df$score),"Yes","No")

# record Model performance
modelPerf.df <- recordModelPerf(modelPerf.df,
                                "blending",
                                time.data,
                                data.frame(),
                                score,
                                improved=improved,
                                bestTune=flattenDF(ensemble.wts),
                                tune.grid="",
                                model.parms="",
                                comment="Level 3 Blending weight")
save(modelPerf.df,file=paste0(WORK.DIR,"/modelPerf.RData"))

#display model performance record for this run
tail(modelPerf.df[,1:10],1)

# if last score recorded is better than previous ones save model object
last.idx <- length(modelPerf.df$score)
if (last.idx == 1 || improved == "Yes") { #force saving all ensemble weights
    cat("found improved model, saving...\n")
    flush.console()
    #yes we have improvement or first score, save generated model
    file.name <- paste0("/opt_wts_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(ensemble.wts,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}



