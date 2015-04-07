###
# generate submission for svm model
###

library(caret)


# import global variabels and common functions
source("./src/CommonFunctions.R")
WORK.DIR <- "./src/svm_model"

# get near zero Vars to eliminate
load(paste0(DATA.DIR,"/near_zero_vars.RData"))

# read kaggle submission data
new.df <- read.csv(unz(paste0(DATA.DIR,"/test.csv.zip"),"test.csv"),stringsAsFactors=FALSE)

#save id vector
id <- new.df$id

# prep the data for submission
new.df <- new.df[,setdiff(names(new.df),c(nz.vars,"id"))]

# retrive gbm model
load(paste0(WORK.DIR,"/svmFit1.RData"))

# predict class probabilities
pred.probs <- predict(svmFit1,newdata = new.df,type = "prob")

#create kaggle submission file
write.csv(data.frame(id,pred.probs),file=paste0(WORK.DIR,"/submission.csv"),
          row.names=FALSE)




