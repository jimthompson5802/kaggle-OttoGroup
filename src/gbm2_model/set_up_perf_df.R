###
#  set up data frame to collect model performance
###

source("./src/CommonFunctions.R")
WORK.DIR <- "./src/gbm_model"  # change to appropriate directory

# set up for gbm model, sequence of model specific parameters to match bestTune data
modPerf.df <- createModelPerfDF(n.trees=integer(0), 
                        shrinkage=numeric(0),
                        interaction.depth=integer(0))

save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))