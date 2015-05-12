###
#  Model specific common functions
###

# generate list of classes to model
PRODUCT.CLASSES <- paste("Class_",1:9,sep="")

# function to calculate feature difference
calcDifference <- function(pair,top.pairs,df) {
    value <- df[,top.pairs[1,pair]] - df[,top.pairs[2,pair]]
    
    return(value)
}


# function to prepare data for modeling
prepModelData <- function(df,diff.df,only.predictors=FALSE){
    # df: raw data
    # diff.df:  pairwise difference features
    # returns a list(predictors,response)
    
    predictors <- df[,setdiff(names(df),c("id","target"))]
    
    #calculate the number of non-zero count features
    non.zero.count <- apply(predictors,1,function(x){sum(x>0)})
    
    predictors <- cbind(predictors,non.zero.count)
    
    d.predictors <- diff.df[,setdiff(names(diff.df),c("id","target"))]
    
    predictors <- cbind(predictors,d.predictors)
    
   
    if (only.predictors) {
        # eliminate unwanted variables
        return(list(predictors=predictors))
    } else {
        # eliminate unwanted variables
        
        response <- factor(df$target)
        
        return(list(predictors=predictors,response=response))
    }

}

#
# helper functions for one vs all modeling
#
trainForOneClass <- function(this.class,train.data,response) {
    
    response <- factor(ifelse(response == this.class,this.class,
                              paste0("Not_",this.class)))
    
    mdl.fit <- do.call(train, c(list(train.data,response),
                            CARET.TRAIN.PARMS,
                            MODEL.SPECIFIC.PARMS,
                            CARET.TRAIN.OTHER.PARMS))
    return(mdl.fit)
}

predictForOneClass <- function(this.class,mdls,new.data,class.feature.list) {
    pred.probs <- predict(mdls[[this.class]],newdata = new.data[,class.feature.list[[this.class]]],
                          type = "prob")
    return(pred.probs[,1])
}