###
# generate submission for one vs all model using gbm
###

library(caret)


# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/gbm2_model"

# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
new.df <- new.df[,setdiff(names(new.df),c("id"))]

# retrive one versus all gbm model
load(paste0(WORK.DIR,"/gbm.mdls_2015-04-14_22_32_15.RData"))

# predict class probabilities
ll <- lapply(classes,predictForOneClass,gbm.mdls,new.df)
names(ll) <- classes

pred.probs <- do.call(cbind,ll)

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




