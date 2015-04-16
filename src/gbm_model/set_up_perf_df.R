###
#  set up data frame to collect model performance
###

source("./src/CommonFunctions.R")
WORK.DIR <- "./src/gbm_model"  # change to appropriate directory

# set up for gbm model, sequence of model specific parameters to match bestTune data
modelPerf.df <- createModelPerfDF()

save(modelPerf.df,file=paste0(WORK.DIR,"/modelPerf.RData"))