###
#  create ensemble model combining selected models
###


library(caret)
library(gbm)
library(randomForest)
library(kernlab)

# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/ensemble_model"

# load optimal weighting factors
load(paste0(WORK.DIR,"/ensembleWeights_2015-04-11_17_29_18.RData"))

# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

#
# make gbm prediction
#

# # prep the data for submission
# source("./src/gbm_model/ModelCommonFunctions.R")
# new.df <- prepModelData(new.df,only.predictors=TRUE)
# 
# # retrive gbm model
# load("./src/gbm_model/gbmFit1_2015-04-07_21_12_42.RData")
# 
# # predict class probabilities
# gbm.probs <- predict(gbmFit1,newdata = new.df$predictors,type = "prob")
# 
# # combine with id
# gbm.probs <- data.frame(id,gbm.probs)

#
# make rf prediction
#

# # read kaggle submission data
# new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)
# 
# #save id vector
# id <- new.df$id
# 
# # prep the data for submission
# source("./src/rf_model/ModelCommonFunctions.R")
# new.df <- prepModelData(new.df,only.predictors=TRUE)
# 
# # retrive rf model
# load("./src/rf_model/rfFit1_2015-04-09_23_06_33.RData")
# 
# # predict class probabilities
# rf.probs <- predict(rfFit1,newdata = new.df$predictors,type = "prob")
# 
# # recombine with id
# rf.probs <- data.frame(id,rf.probs)

#
# make rf prediction with expanded feature set
#
source("./src/rf2_model/ModelCommonFunctions.R")
# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# retrive rf model with expanded features
load("./src/rf2_model/model_rf_all_data_ntree_5000.RData")

# predict class probabilities
system.time(rf2.probs <- predictInParallel(mdl.fit,new.df,5,only.predictors = TRUE))

#
# make one vs all using gbm predictions
#

predictForOneClass <- function(this.class,mdls,new.data) {
    pred.probs <- predict(mdls[[this.class]],newdata = new.data,type = "prob")
    return(pred.probs[,1])
}

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
source("./src/gbm2_model/ModelCommonFunctions.R")
new.df <- prepModelData(new.df,only.predictors=TRUE)

# retrive one versus all gbm model
load(paste0("./src/gbm2_model/model_gbm_one_vs_all_2015-04-19_12_57_35.RData"))

# predict class probabilities
classes <- paste("Class_",1:9,sep="")  # generate list of classes to model
ll <- lapply(classes,predictForOneClass,gbm.mdls,new.df$predictors)
names(ll) <- classes

gbm2.probs <- do.call(cbind,ll)

gbm2.probs <- data.frame(id,gbm2.probs)

#
# Average the individual probablities
#

pred.probs <- ((1/2)*rf2.probs[,2:10]) + 
    ((1/2)*gbm2.probs[,2:10])


#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)

