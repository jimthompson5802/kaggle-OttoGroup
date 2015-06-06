###
# generate submission for  model random forest - level 2
###

library(caret)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/stk_model/rf1_2"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source("./src/stk_model/StackCommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# retrive Level 2 model
load(paste0(WORK.DIR,"/model_rf_2015-06-06_16_41_23.RData"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
submission <- prepModelData(new.df,only.predictors=TRUE)

# get level 1 predictions for the submission data set
rf1_1.pred.probs <- rf1_1Predictions(new.df)
names(rf1_1.pred.probs) <- paste0("rf1_1.",names(rf1_1.pred.probs))

gbm1_1.pred.probs <- gbm1_1Predictions(new.df)
names(gbm1_1.pred.probs) <- paste0("gbm1_1.",names(gbm1_1.pred.probs))

# combine level 1 predictions with submission features
submission$predictors <- cbind(rf1_1.pred.probs,gbm1_1.pred.probs,submission$predictors)

# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




