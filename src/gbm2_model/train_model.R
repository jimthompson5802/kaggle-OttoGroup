###
# training gbm model - one vs all apprach
###

library(caret)
library(gbm)
library(pROC)
# Common Functions and Global variables
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/gbm2_model"
MODEL.METHOD <- "gbm"
MODEL.COMMENT <- "Collection of gbm one vs all classification models."

# load model performance data
# load(paste0(WORK.DIR,"/modPerf.RData"))

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


trainForOneClass <- function(this.class,train.df) {
    
    train.data <- subset(train.df,select=-target)
    response <- factor(ifelse(train.df$target == this.class,this.class,
                              paste0("Not_",this.class)))
    
    gbmFit <- train(train.data,
                     response,
                     method = MODEL.METHOD,
                     
                     ## This last option is actually one
                     ## for gbm() that passes through
                     verbose = FALSE,
                     
                     ## remaining train options
                     trControl = TRAIN.CTRL,
                     tuneGrid=TUNE.GRID,
                     metric="ROC")
    return(gbmFit)
}

predictForOneClass <- function(this.class,mdls,new.data) {
    pred.probs <- predict(mdls[[this.class]],newdata = new.data,type = "prob")
    return(pred.probs[,1])
}

# eliminate near zero Variance
train.df <- train.df[,setdiff(names(train.df),c(nz.vars,"id"))]


TRAIN.CTRL<- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats=1,
    verboseIter = TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary)

TUNE.GRID <- NULL
# TUNE.GRID <-  expand.grid(interaction.depth = c(3, 5, 7, 9),
#                         n.trees = c(1000, 2000, 3000),
#                         shrinkage = 0.01)  # avoid numerical issue in gbm

classes <- paste("Class_",1:9,sep="")

Sys.time()
set.seed(825)
time.data <- system.time(gbm.mdls<-lapply(classes,trainForOneClass,train.df))
time.data
names(gbm.mdls) <- classes
comment(gbm.mdls) <- MODEL.COMMENT


# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c(nz.vars,"id"))]

ll <- lapply(classes,predictForOneClass,gbm.mdls,test)
names(ll) <- classes

pred.probs <- do.call(cbind,ll)

score <- logLossEval(pred.probs,test$target)
score

# record Model performance
modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              train.df[,1:(ncol(train.df)-1)],
                              score,gbmFit1$bestTune)
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
    file.name <- paste0("/gbmFit1_",modPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(gbmFit1,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}




