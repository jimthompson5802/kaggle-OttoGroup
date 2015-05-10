###
#  explore pair wise differences
###


library(ggplot2)
library(dplyr)
library(plyr)
library(caret)



source("./src/CommonFunctions.R")


load(paste0(DATA.DIR,"/train_calib_test.RData"))
rm(calib.raw,test.raw)

predictor.vars <- setdiff(names(train.raw), c("target","id"))

diffs <- data.frame(calcPairwiseDiff(predictor.vars,train.raw))

# elimiate zero or near zero variance predictors
nz.idx <- nearZeroVar(diffs)
diffs <- diffs[,-nz.idx]

# elimanate correlated predictors
cor.idx <- findCorrelation(cor(diffs))
diffs <- diffs[,-cor.idx]



response <- ifelse(train.raw$target == "Class_5",1,0)
mdl <- glm.fit(diffs[,1], response, family="binomial")
