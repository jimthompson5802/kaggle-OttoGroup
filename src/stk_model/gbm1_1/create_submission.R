###
# generate submission for one vs all model using gbm
###

library(caret)
library(gbm)

WORK.DIR <- "./src/stk_model/gbm1_1"

# import global variabels and common functions
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
submission <- prepModelData(new.df,only.predictors=TRUE)

# retrive one versus all gbm model
load(paste0(WORK.DIR,"/model_gbm_one_vs_all_2015-06-06_14_13_28.RData"))

# predict class probabilities
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,submission$predictors)
names(ll) <- PRODUCT.CLASSES

pred.probs <- do.call(cbind,ll)

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




