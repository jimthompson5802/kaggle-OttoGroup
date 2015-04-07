###
#  set up data frame to collect model performance
###

source("./src/CommonFunctions.R")
WORK.DIR <- "./src/rf_model"  # change to appropriate directory

# set up for gbm model, sequence of model specific parameters to match bestTune data
modPerf.df <- createModelPerfDF(mtry=integer(0))

save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))