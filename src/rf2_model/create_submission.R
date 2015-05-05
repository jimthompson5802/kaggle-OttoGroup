###
# generate submission for  model
###

library(caret)
# add model specific libraries
library(randomForest)

# set working directory
WORK.DIR <- "./src/rf2_model"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prepare data for prediction
new.df <- prepModelData(new.df,only.predictors = TRUE)

# retrive generated model-name created in training run
load(paste0(WORK.DIR,"/model_rf_all_data_ntree_1000.RData"))

# predict class probabilities

system.time(pred.probs <- predictInParallel(mdl.fit,new.df$predictors,5,only.predictors = TRUE))

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




