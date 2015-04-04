###
#  Test Bed
###

data.dir <- "./data"

load(paste0(data.dir,"/train_calib_test.RData"))

source("./src/CommonFunctions.R")

test.probs <- matrix(rep(1/9,nrow(test.raw)*9),ncol=9)
colnames(test.probs) <- paste0("Class_",1:9)


logLossEval(test.probs,test.raw$target)
