###
# generate submission for  model
###

library(caret)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/stk_model/rf1_1"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
submission <- prepModelData(new.df,only.predictors=TRUE)

# retrive generated model-name created in training run
load(paste0(WORK.DIR,"/model_rf_2015-06-06_13_42_31.RData"))

# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




