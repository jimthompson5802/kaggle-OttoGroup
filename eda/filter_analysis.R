###
#  single feature filter
###

library(caret)
library(dplyr)
library(doMC)

source("./src/CommonFunctions.R")

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
rm(test.raw)

idx <- createDataPartition(train.raw$target,p=0.1,list=FALSE)
train.raw <- train.raw[idx,]

predictor.vars <- setdiff(names(train.raw),c("id","target"))

predictors <- select(train.raw,one_of(predictor.vars))
predictors <- data.frame(predictors,non.zero.count)

response <- factor(ifelse(train.raw$target == "Class_3","Class_3","Not_Class_3"))

non.zero.count <- apply(predictors,1,function(row){sum(row>0)})


filterCtrl <- sbfControl(functions = rfSBF,
                         method = "repeatedcv", repeats = 5)

set.seed(10)
rfWithFilter <- sbf(predictors, response, sbfControl = filterCtrl)
rfWithFilter



scoreEachFeature <- function (cls,predictors,target) {
    # change target to binary classs
    response <- factor(ifelse(target==cls,cls,paste0("Not_",cls)))
    
    ll <- lapply(names(predictors), function(x){anovaScores(predictors[,x],response)})
    names(ll) <- names(predictors)
    df <- data.frame(do.call(rbind,ll))
    df <- data.frame(class=cls,feature=row.names(df),df,stringsAsFactors=FALSE)
    names(df) <- c("class","feature","anova.score")
    df <- df[order(df$anova.score),]
}

df <- do.call(rbind,lapply(paste0("Class_",1:9),scoreEachFeature,predictors,train.raw$target))

