###
#  set up data frame to collect model performance
###

source("./src/CommonFunctions.R")
WORK.DIR <- "./src/ensemble_model"  # change to appropriate directory

# set up capturing sequence of model specific parameters to match bestTune data
modPerf.df <- createModelPerfDF(comment=character(0))

save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))