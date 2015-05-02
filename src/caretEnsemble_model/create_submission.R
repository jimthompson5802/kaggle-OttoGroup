###
# generate submission for  model
###

library(caretEnsemble)
# add model specific libraries
library(caTools)

# set working directory
WORK.DIR <- "./src/caretEnsemble_model"   # directory where model artifacts are stored

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
submission <- prepModelData(new.df,only.predictors=TRUE)

# retrive one versus all gbm model
load(paste0(WORK.DIR,"/model_ensemble_2015-05-02_12_45_41.RData"))

# predict class probabilities
ll <- lapply(PRODUCT.CLASSES,predictForOneClass,all.classes,submission$predictors)
names(ll) <- PRODUCT.CLASSES

pred.probs <- do.call(cbind,ll)

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




