###
# generate submission for svm model
###

library(caret)
library(gbm)


# set working directory
WORK.DIR <- "./src/gbm_model"

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
submission <- prepModelData(new.df,only.predictors=TRUE)

# retrive gbm model
load(paste0(WORK.DIR,"/gbmFit1_2015-04-07_21_12_42.RData"))

mdl.fit <- gbmFit1
# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




