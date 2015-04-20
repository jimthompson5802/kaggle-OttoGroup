###
#  add trees to model
#  This code adds trees to a random forest model created with caret
###

library(caret)
library(randomForest)

# set up global variables and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf_model"
source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

TREES.TO.ADD <- 500
MODEL.COMMENT <- paste("Growing trees by",TREES.TO.ADD)

# load caret model structure
load(paste0(WORK.DIR,"/rfFit1_2015-04-15_06_43_37.RData"))

# load model performance data
load(paste0(WORK.DIR,"/modelPerf.RData"))

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))

# generate original test score
# prepare data for testing
test.data <- prepModelData(test.raw)

pred.probs <- predict(rfFit1,newdata = test.data$predictors,type = "prob")

before.score <- logLossEval(pred.probs,test.data$response)
before.score

# print model specifics
print(rfFit1$finalModel)

# add trees to model
time.data <- system.time(rfFit1$finalModel <- grow(rfFit1$finalModel,TREES.TO.ADD))

# check score after adding tees
after.score <- logLossEval(pred.probs,test.data$predictors)
after.score

