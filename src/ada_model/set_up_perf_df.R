###
#  set up data frame to collect model performance
###

source("./src/CommonFunctions.R")
WORK.DIR <- "./src/ada_model"  # change to appropriate directory

# set up to collect model performance data sequence of model specific parameters 
# to match bestTune data
modPerf.df <- createModelPerfDF(mfinal=integer(0), 
                        maxdepth=integer(0),
                        coeflearn=character(0))

save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))