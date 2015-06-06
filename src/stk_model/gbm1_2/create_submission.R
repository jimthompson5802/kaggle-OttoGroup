###
# generate submission for one vs all model using gbm  - Level 2 model
###

library(caret)
library(gbm)

WORK.DIR <- "./src/stk_model/gbm1_2"

# import global variabels and common functions
source("./src/CommonFunctions.R")
source("./src/stk_model/StackCommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# retrive one versus all gbm model - Level 2
load(paste0(WORK.DIR,"/model_gbm_one_vs_all_2015-06-06_16_58_43.RData"))

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
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,submission$predictors)
names(ll) <- PRODUCT.CLASSES

pred.probs <- do.call(cbind,ll)

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




