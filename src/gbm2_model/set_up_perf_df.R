###
#  set up data frame to collect model performance
###

source("./src/CommonFunctions.R")
WORK.DIR <- "./src/gbm2_model"  # change to appropriate directory

# set up for gbm model, sequence of model specific parameters to match bestTune data
modPerf.df <- createModelPerfDF(Class_1.n.trees=integer(0),
                                Class_1.interaction.depth=integer(0),
                                Class_1.shrinkage=numeric(0),
                                Class_2.n.trees=integer(0),
                                Class_2.interaction.depth=integer(0),
                                Class_2.shrinkage=numeric(0),
                                Class_3.n.trees=integer(0),
                                Class_3.interaction.depth=integer(0),
                                Class_3.shrinkage=numeric(0),
                                Class_4.n.trees=integer(0),
                                Class_4.interaction.depth=integer(0),
                                Class_4.shrinkage=numeric(0),
                                Class_5.n.trees=integer(0),
                                Class_5.interaction.depth=integer(0),
                                Class_5.shrinkage=numeric(0),
                                Class_6.n.trees=integer(0),
                                Class_6.interaction.depth=integer(0),
                                Class_6.shrinkage=numeric(0),
                                Class_7.n.trees=integer(0),
                                Class_7.interaction.depth=integer(0),
                                Class_7.shrinkage=numeric(0),
                                Class_8.n.trees=integer(0),
                                Class_8.interaction.depth=integer(0),
                                Class_8.shrinkage=numeric(0),
                                Class_9.n.trees=integer(0),
                                Class_9.interaction.depth=integer(0),
                                Class_9.shrinkage=numeric(0))

save(modPerf.df,file=paste0(WORK.DIR,"/modPerf.RData"))
