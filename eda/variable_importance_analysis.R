###
#  Analyze variable importance from gbm and rf models
###

library(randomForest)
library(gbm)
library(caret)
library(pROC)
library(dplyr)
library(kernlab)
library(glmnet)


source("src/CommonFunctions.R")

# get the relevant models 

# randomForest variable importance
load("./src/rf2_model/model_rf_all_data_ntree_4000.RData")
rf.varImp <- varImp(mdl.fit)
rf.varImp <- data.frame(feature=row.names(rf.varImp),rf.varImp)
rf.varImp$Normalized.Overall <- 100 * rf.varImp$Overall / max(rf.varImp$Overall)
rf.varImp <- rf.varImp[rev(order(rf.varImp$Overall)),]

# gbm variable importance
load("./src/gbm2_model/model_gbm_one_vs_all_2015-05-08_22_59_43.RData")
ll <- lapply(paste0("Class_",1:9),function(cls){data.frame(Class=cls,
                                                           feature=row.names(varImp(gbm.mdls[[cls]]$finalModel)),
                                                           varImp(gbm.mdls[[cls]]$finalModel),
                                                           stringsAsFactors=FALSE)})
ll <- lapply(ll,function(df){df$Normalized.Overall <- 100 * df$Overall / max(df$Overall); return(df)})

gbm.varImp <- do.call(rbind,ll)

gbm.varImp <- gbm.varImp[order(gbm.varImp$Class,-gbm.varImp$Overall),]




# test out subsets of teh important variables
this.vars <- subset(gbm.varImp,Class=="Class_5")[1:5,"feature"]

#load training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

set.seed(13)
idx <- createDataPartition(train.raw$target, p=0.5,list=FALSE)

predictors <- train.raw[idx,setdiff(names(train.raw),c("id","target"))]
predictors$non.zero.count <- apply(predictors,1,function(row){sum(row>0)})

tr.ctrl <- trainControl(method="repeatedcv", number=5, repeats=1,
                        verboseIter = FALSE,
                        classProbs=TRUE,
                        summaryFunction=twoClassSummary)

predictors <- select(predictors,one_of(this.vars))
response <- factor(ifelse(train.raw$target[idx] == "Class_5","Class_5","Not_Class_5"))

# library(doMC)
# registerDoMC(5)

mdl <- train(predictors,response,
             method="glmnet", 
             preProcess=c("center","scale"),
#              verbose=FALSE,
             trControl=tr.ctrl,
#              tuneLength=5,
             metric="ROC")

mdl

# stopCluster(cl)

predictors <- test.raw[,setdiff(names(test.raw),c("id","target"))]
predictors$non.zero.count <- apply(predictors,1,function(row){sum(row>0)})
predictors <- predictors[,this.vars]


pred <- predict(mdl,predictors,type="prob")

myroc <- roc(test.raw$target, pred[,1])
myroc
