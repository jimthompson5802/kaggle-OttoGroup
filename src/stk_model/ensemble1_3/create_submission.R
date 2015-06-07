###
#  Determine weighting factors for blending the Level 2 models
###


library(caret)
library(alabama)

WORK.DIR <- "./src/stk_model/ensemble1_3"

OPT.WTS <- "opt_wts_2015-06-07_06_15_57.RData"

cat("Using wts:",OPT.WTS,"/n")

# Common Functions and Global variables
source("./src/CommonFunctions.R")
source("./src/stk_model/StackCommonFunctions.R")
# source(paste0(WORK.DIR,"/ModelCommonFunctions.R"))

# get optimal ensemble weights
load(paste0(WORK.DIR,"/",OPT.WTS))

# read kaggle test data set for submission
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# get Level 2 model predictions
rf1_2.pred.probs <- rf1_2Predictions(new.df)
gbm1_2.pred.probs <- gbm1_2Predictions(new.df)


# blend the Level 2 predictions
pred.probs <- ensemble.wts[1,"rf1_2"]*rf1_2.pred.probs + 
    ensemble.wts[1,"gbm1_2"] * gbm1_2.pred.probs

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)