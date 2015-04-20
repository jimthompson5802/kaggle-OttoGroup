###
# Variable Importance Analysis for gbm model
###

library(caret)
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/gbm_model"


# load model file
load(paste0(WORK.DIR,"/model_gbm_2015-04-17_21_01_46.RData"))


# extract variable importance
var.ll <- varImp(mdl.fit)
var.importance <- var.ll$importance[rev(order(var.ll$importance$Overall)),]
names(var.importance) <- row.names(var.ll$importance)[rev(order(var.ll$importance$Overall))]

# extract top 20
gbm.top.20 <- names(var.importance)[1:20]

# save top 20 variable names 
save(gbm.top.20,file=paste0(WORK.DIR,"/top_20_variables.RData"))


