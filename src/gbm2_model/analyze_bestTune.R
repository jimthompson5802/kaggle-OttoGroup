###
#  extract bestTune data for anlaysis
###

WORK.DIR <- "./src/gbm2_model"

source(paste0(WORK.DIR,"/modelCommonFunctions.R"))


ll <- lapply(gbm.mdls,function(x){x$bestTune})

bestTune.df <- do.call(rbind,ll)

bestTune.df <- data.frame(class=names(gbm.mdls),bestTune.df)
bestTune.df <- data.frame(model.version=2,bestTune.df)

combined.df <- rbind(combined.df,bestTune.df)

write.csv(combined.df,file=paste0(WORK.DIR,"/bestTunePerClass.csv"))
