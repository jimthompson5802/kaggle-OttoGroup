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
FRACTION.TRAIN.DATA <- 0.6

TRAIN.CTRL<- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats=1,
    verboseIter = TRUE,
    classProbs=TRUE,
    summaryFunction=twoClassSummary)

# TUNE.GRID <- NULL
TUNE.GRID <-  expand.grid(interaction.depth = c(3, 5, 7, 9),
                        n.trees = c(100,200,300,400,500),
                        shrinkage = 0.1)  # avoid numerical issue in gbm


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
idx <- sample(nrow(train.raw),FRACTION.TRAIN.DATA*nrow(train.raw))
train.df <- train.raw[idx,]

trainForOneClass <- function(this.class,train.data,response) {
    
    response <- factor(ifelse(response == this.class,this.class,
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
train.data <- train.df[,setdiff(names(train.df),c("id","target"))]

# generate list of classes to model
classes <- paste("Class_",1:9,sep="")

Sys.time()
set.seed(825)
time.data <- system.time(gbm.mdls<-lapply(classes,trainForOneClass,train.data,train.df$target))
time.data
names(gbm.mdls) <- classes
comment(gbm.mdls) <- MODEL.COMMENT


# evaluate on test ste
test <- test.raw[,setdiff(names(test.raw),c("id","target"))]

ll <- lapply(classes,predictForOneClass,gbm.mdls,test)
names(ll) <- classes

pred.probs <- do.call(cbind,ll)

score <- logLossEval(pred.probs,test.raw$target)
score

# record Model performance
ll <- llply(gbm.mdls,function(x){print(x$bestTune)})
bestTune <- do.call(cbind,ll)

modPerf.df <- recordModelPerf(modPerf.df,MODEL.METHOD,time.data,
                              train.df[,1:(ncol(train.df)-1)],
                              score,bestTune)
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
    file.name <- paste0("/gbm.mdls_",modPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(gbm.mdls,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}




