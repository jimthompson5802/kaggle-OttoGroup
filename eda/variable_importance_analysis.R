###
#  Analyze variable importance from gbm and rf models
###

library(randomForest)
library(gbm)


# get the relevant models 

# randomForest variable importance
load("./src/rf2_model/model_rf_all_data_ntree_4000.RData")
rf.varImp <- varImp(mdl.fit)

# gbm variable importance
load("./src/gbm2_model/model_gbm_one_vs_all_2015-05-08_22_59_43.RData")
ll <- lapply(paste0("Class_",1:9),function(cls){data.frame(Class=cls,
                                                           feature=row.names(varImp(gbm.mdls[[cls]]$finalModel)),
                                                           varImp(gbm.mdls[[cls]]$finalModel),
                                                           stringsAsFactors=FALSE)})

gbm.varImp <- do.call(rbind,ll)

gbm.varImp <- gbm.varImp[order(gbm.varImp$Class,gbm.varImp$Overall),]
