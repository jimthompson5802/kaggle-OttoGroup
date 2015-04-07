###
#  Test Bed
###

data.dir <- "./data"

load(paste0(data.dir,"/train_calib_test.RData"))

source("./src/CommonFunctions.R")

# test.probs <- matrix(rep(1/9,nrow(test.raw)*9),ncol=9)
# colnames(test.probs) <- paste0("Class_",1:9)
# 
# test.probs[1,1] <- 0
# 
# 
# logLossEval(test.probs,test.raw$target)

df <- createModelPerfDF(n.trees=integer(0), shrinkage=numeric(0),interaction.depth=integer(0))
df <- recordModelPerf(df,"gbm",time.data,train.df,score,gbmFit1$bestTune)
