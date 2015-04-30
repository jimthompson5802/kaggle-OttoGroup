###
#  Test of caretEnsemble for individual classifiers for one vs all approach
###


library(caretEnsemble)
library(plyr)
library(dplyr)
library('pROC')
library(caTools)

source("./src/CommonFunctions.R")

# get test data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
rm(calib.raw)
idx <- createDataPartition(train.raw$target, p=0.5, list=FALSE)
train.raw <- train.raw[idx,]


# augment features with count of non-zero counters
predictors <- select(train.raw,contains("feat_"))
non.zero.count <- apply(predictors,1,function(row){sum(row>0)})
predictors <- cbind(predictors,non.zero.count)
train.obs <- dim(predictors)

# Test out Class 2
response <- factor(ifelse(train.raw$target == "Class_2","Class_2","Not_Class_2"))

table(response)/length(response)

library(doSNOW)
cl <- makeCluster(5,type="SOCK")
registerDoSNOW(cl)

# set up caret ensemble
set.seed(107)
my_control <- trainControl(savePredictions=TRUE,
                            classProbs=TRUE,
                            index=createMultiFolds(response,5,1),
                            summaryFunction=twoClassSummary)

time.data<- system.time(model_list <- caretList(predictors,response,
                        trControl=my_control,
                        methodList=c('gbm', 'rf',"glm","svmRadial")))

stopCluster(cl)
ens <- caretEnsemble(model_list)



# make predictions using test set
# augment features with count of non-zero counters
predictors <- select(test.raw,contains("feat_"))
non.zero.count <- apply(predictors,1,function(row){sum(row>0)})
predictors <- cbind(predictors,non.zero.count)


pred.probs <- lapply(model_list,predict,newdata=predictors,type="prob")
pred.probs <- lapply(pred.probs,function(x){x[,"Class_2"]})
pred.probs <- data.frame(pred.probs)

pred.probs$ensemble <- predict(ens,newdata=predictors)


sink("./eda/caret_ensemble_testing.txt",append=TRUE)
cat(rep("\n",3),rep("*",80),rep("\n",1),rep("*",80),rep("\n",1),rep("*",80),sep="")
cat("\n\n***************Dimension of training data ",train.obs,"\n")
cat("\n\n***************Timing data\n")
time.data

cat("\n\n***************Models trained\n")
model_list

cat("\n\n***************Models in the ensemble\n")
ens

cat("\n\n**************Model correlations\n")
modelCor(resamples(model_list))

cat("\n\n**************Individual Model and Ensemble AUC\n")
response <- factor(ifelse(test.raw$target == "Class_2","Class_2","Not_Class_2"))
colAUC(pred.probs,response)
sink()
