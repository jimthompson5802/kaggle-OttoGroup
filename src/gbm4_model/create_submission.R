###
# generate submission for one vs all model using gbm
###

library(caret)
library(gbm)

WORK.DIR <- "./src/gbm4_model"

# import global variabels and common functions
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# get class specific feature set
load("./eda/selected_features_for_each_class.RData")

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

d.new.df <- data.frame(calcPairwiseDiff(setdiff(names(new.df),c("id","target")),new.df))

#save id vector
id <- new.df$id

# prep the data for submission
submission <- prepModelData(new.df,d.new.df,only.predictors=TRUE)

# retrive one versus all gbm model
load(paste0(WORK.DIR,"/model_gbm_one_vs_all_2015-05-12_16_28_54.RData"))

# predict class probabilities
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,submission$predictors,
             class.feature.list)
names(ll) <- PRODUCT.CLASSES

pred.probs <- do.call(cbind,ll)

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




