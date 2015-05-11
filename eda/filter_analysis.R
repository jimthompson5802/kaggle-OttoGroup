###
#  single feature filter
###

library(caret)
library(dplyr)
library(doMC)

source("./src/CommonFunctions.R")

# get training data
load(paste0(DATA.DIR,"/train_calib_test.RData"))
load(paste0(DATA.DIR,"/diff_train_calib_test.RData"))
rm(test.raw,calib.raw,d.calib.raw,d.test.raw)

# idx <- createDataPartition(train.raw$target,p=0.1,list=FALSE)
# train.raw <- train.raw[idx,]

predictor.vars <- setdiff(names(train.raw),c("id","target"))

predictors <- select(train.raw,one_of(predictor.vars))
non.zero.count <- apply(predictors,1,function(x){sum(x>0)})
predictors <- data.frame(predictors,non.zero.count)


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

# univariate for difference
predictor.vars <- setdiff(names(d.train.raw),c("id","target"))
predictors <- select(d.train.raw,one_of(predictor.vars))
set.seed(21)
idx <- createDataPartition(d.train.raw$target,p=0.5,list=FALSE)
predictors <- predictors[idx,]
target <- d.train.raw[idx,"target"]

registerDoMC(5)
diff.df <- do.call(rbind,mclapply(paste0("Class_",1:9),scoreEachFeature,predictors,target,
                                   mc.cores=5))

save(df,diff.df,file="./eda/anovaScores.RData")

