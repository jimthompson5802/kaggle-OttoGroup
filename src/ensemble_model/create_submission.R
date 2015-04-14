###
#  create ensemble model combining selected models
###


library(caret)
library(gbm)
library(randomForest)
library(kernlab)

# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/ensemble_model"

# load optimal weighting factors
load(paste0(WORK.DIR,"/ensembleWeights_2015-04-11_17_29_18.RData"))

# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

#
# make gbm prediction
#

# prep the data for submission
new.df <- new.df[,setdiff(names(new.df),c(nz.vars,"id"))]

# retrive gbm model
load("./src/gbm_model/gbmFit1_2015-04-07_21_12_42.RData")

# predict class probabilities
gbm.probs <- predict(gbmFit1,newdata = new.df,type = "prob")

# combine with id
gbm.probs <- data.frame(id,gbm.probs)

#
# make rf prediction
#

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
new.df <- new.df[,setdiff(names(new.df),c("id"))]

# retrive rf model
load("./src/rf_model/rfFit1_2015-04-09_23_06_33.RData")

# predict class probabilities
rf.probs <- predict(rfFit1,newdata = new.df,type = "prob")

# recombine with id
rf.probs <- data.frame(id,rf.probs)

#
# make svm prediction
#

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
new.df <- new.df[,setdiff(names(new.df),c(nz.vars,"id"))]

# retrive svm model
load("./src/svm_model/svmFit1_2015-04-12_20_19_00.RData")

# predict class probabilities
svm.probs <- predict(svmFit1,newdata = new.df,type = "prob")

# recombine with id
svm.probs <- data.frame(id,svm.probs)

#
# Average the individual probablities
#

pred.probs <- ((1/3)*gbm.probs[,2:10]) + 
    ((1/3)*rf.probs[,2:10]) +
    ((1/3)*svm.probs[,2:10])


#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)

