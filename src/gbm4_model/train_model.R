###
# training gbm model - one vs all apprach
# class specific synthetic features
###

library(caret)
library(gbm)
library(pROC)

# set working directory
WORK.DIR <- "./src/gbm4_model"

# Common Functions and Global variables
source("./src/CommonFunctions.R")
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
                                 #                                  allowParallel=FALSE,
                                 summaryFunction=twoClassSummary)

MODEL.TUNE <- list(
    Class_1=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(7,9,11,13,15), shrinkage=0.1),
    Class_2=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(7,9,11,13,15), shrinkage=0.1),
    Class_3=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(7,9,11,13,15), shrinkage=0.1),
    Class_4=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(7,9,11,13,15), shrinkage=0.1),
    Class_5=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(1,3,5,7), shrinkage=0.1),
    Class_6=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(7,9,11,13,15), shrinkage=0.1),
    Class_7=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(7,9,11,13,15), shrinkage=0.1),
    Class_8=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(7,9,11,13,15), shrinkage=0.1),
    Class_9=expand.grid(n.trees=seq(100,1000,100), interaction.depth=c(7,9,11,13,15), shrinkage=0.1)
)

CARET.TRAIN.OTHER.PARMS <- list(trControl=CARET.TRAIN.CTRL,
                                maximize=TRUE,
                                metric="ROC")

MODEL.SPECIFIC.PARMS <- list(verbose=FALSE)

MODEL.COMMENT <- "aws(9) synthetic features gbm one vs all - top 150 class specific features"

# amount of data to train
FRACTION.TRAIN.DATA <- 1


# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
load(paste0(DATA.DIR,"/diff_train_calib_test.RData"))
load("./eda/selected_features_for_each_class.RData")


library(doMC)
registerDoMC(cores = 9)

# extract subset for inital training
set.seed(29)
idx <- createDataPartition(train.raw$target, p = FRACTION.TRAIN.DATA, list=FALSE)
train.df <- train.raw[idx,]
d.train.df <- d.train.raw[idx,]

# prepare data for modeling
train.data <- prepModelData(train.df,d.train.df)

# function to do final model training
trainOneClass <- function(this.class) {
    
    response <- factor(ifelse(RESPONSE == this.class,this.class,
                              paste0("Not_",this.class)))
    
    # get class specific features
    use.these.features <- class.feature.list[[this.class]][1:min(150,length(class.feature.list[[this.class]]))]
    class.predictors <- PREDICTORS[,use.these.features]
    set.seed(825)
    mdl.fit <- do.call(train, c(list(class.predictors,response),
                                CARET.TRAIN.PARMS,
                                MODEL.SPECIFIC.PARMS,
                                CARET.TRAIN.OTHER.PARMS,
                                list(tuneGrid=MODEL.TUNE[[this.class]])))
    
    return(mdl.fit)
}

PREDICTORS <- train.data$predictors
RESPONSE <-train.data$response


Sys.time()

time.data <- system.time(gbm.mdls <- foreach(this.class=PRODUCT.CLASSES) %do%
                             trainOneClass(this.class))

time.data

names(gbm.mdls) <- PRODUCT.CLASSES
comment(gbm.mdls) <- MODEL.COMMENT


# prepare data for testing
test.data <- prepModelData(test.raw,d.test.raw)

# for each classes predict probability of for that class
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,test.data$predictors,
             class.feature.list)
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
if (last.idx == 1 || improved == "Yes") {
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
