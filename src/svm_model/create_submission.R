###
# generate submission for svm model
###

library(caret)


# set working directory
WORK.DIR <- "./src/svm_model"

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
load(paste0(WORK.DIR,"/model_svmLinear2015-04-15_21_50_43.RData"))

# predict class probabilities
pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




