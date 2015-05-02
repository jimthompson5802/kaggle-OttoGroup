###
# training caretEnsemble model one vs all approach
# train models separately then combine into caretEnsemble structure
# this allows support for intermedidate saves of models.
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
# ENS.MODELS <- c('gbm', 'rf',"svmRadial","glm")
ENS.MODELS <- c('glm')

MODEL.COMMENT <- "caretEnsemble one vs all with separate model training"

# amount of data to train
FRACTION.TRAIN.DATA <- 0.05

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

# train one model to initialize the caretList structure
time.data <- system.time(caret.list <-lapply(PRODUCT.CLASSES,trainCaretListForOneClass,
                                          train.data$predictors,
                                          train.data$response))

names(caret.list) <- PRODUCT.CLASSES
save(caret.list,file=paste0(WORK.DIR,"/chkpt_caret_list.RData"))


# generate the other models, set up list of remaining models
model.list <- list(list(method="gbm",verbose=FALSE),
                   list(method="rf"),
                   list(method="svmRadial"))

# generate remaining models using caret train()
other.mdls <- lapply(model.list,function(mdl.parms){
    time.data <- system.time(models<-lapply(PRODUCT.CLASSES,trainForOneClass,train.data$predictors,
                                          train.data$response,mdl.parms))
    names(models) <- PRODUCT.CLASSES
    save(models,file=paste0(WORK.DIR,"/chkpt_model_",mdl.parms$method,".RData"))
    return(list(model.parms=mdl.parms, models=models,time.data=time.data))
})

stopCluster(cl)

###
# merge models into caretList structure
###

all.classes <- list()

for (cls in PRODUCT.CLASSES) {
    for (i in 1:length(other.mdls)) {
        # add models to respective class
        caret.list[[cls]][[other.mdls[[i]]$model.parm$method]] <- other.mdls[[i]]$models[[cls]]
        
    }
    
    #generate caretEnsemble from the newly combined models
    all.classes[[cls]] <- caretEnsemble(caret.list[[cls]])
}

# accumulate the time for training
time.data <- time.data + Reduce("+",lapply(other.mdls,function(x){x$time.data}))

comment(all.classes) <- MODEL.COMMENT
time.data

# clean-up data structures no longer needed
rm(caret.list,other.mdls)

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
                              "caretEnsemble",
                              time.data,
                              train.data$predictors,
                              score,
                              improved=improved,
                              bestTune="",
                              tune.grid="",
                              model.parms=paste(c(ENS.MODELS,unlist(lapply(model.list,function(x){x$method}))),
                                                collapse=","),
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
    file.name <- paste0("/model_ensemble_",modelPerf.df$date.time[last.idx],".RData")
    file.name <- gsub(" ","_",file.name)
    file.name <- gsub(":","_",file.name)
    
    save(all.classes,file=paste0(WORK.DIR,file.name))
} else {
    cat("no improvement!!!\n")
    flush.console()
}

# record details on training log
sink(paste0(WORK.DIR,"/training_log.txt"),append=TRUE)
cat(rep("\n",3),rep("*",80),rep("\n",1),rep("*",80),rep("\n",1),rep("*",80),sep="")

cat("\n\n***************Date/Time******************\n")
modelPerf.df$date.time[last.idx]

cat("\n\n***************Dimension of training data**************\n")
dim(train.data$predictors)

cat("\n\n***************Timing data******************************\n")
time.data

cat("\n\n**************Ensembles by Class************************\n")
l_ply(PRODUCT.CLASSES, function(x){cat("\n\n",x);
                                   print(all.classes[[x]]);
                                   cat("\nModel Correlations\n");
                                   modelCor(resamples(all.classes[[x]]$models))}, .print=TRUE)

sink()


