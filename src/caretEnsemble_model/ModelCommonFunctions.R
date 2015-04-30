###
#  Model specific common functions
###

# generate list of classes to model
PRODUCT.CLASSES <- paste("Class_",1:9,sep="")

# this function prepares data for training or use in predictions
prepModelData <- function(df,only.predictors=FALSE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    # eliminate unwanted variables
    predictors <- df[,setdiff(names(df),c("id","target"))]
    non.zero.count <- apply(predictors,1,function(row){sum(row>0)})
    
    predictors <- cbind(predictors,non.zero.count)
   
    if (only.predictors) {

        ans <- list(predictors=predictors)
    } else {
        
        response <- factor(df$target)
        
        ans <- list(predictors=predictors,response=response)
    }

    return(ans)
}


#
# helper functions for one vs all modeling
#
trainEnsembleForOneClass <- function(this.class,train.data,response) {
    
    response <- factor(ifelse(response == this.class,this.class,
                              paste0("Not_",this.class)))
    
    model.list <- do.call(caretList,c(list(train.data,response),
                               list(trControl=ENS.TRAIN.CTRL,
                                    methodList=ENS.MODELS)))

    ens <- caretEnsemble(model.list)
    
    return(list(ens=ens))
}

predictForOneClass <- function(this.class,mdls,new.data) {
    pred.probs <- predict(mdls[[this.class]]$ens,newdata = new.data)
    return(1.0 - pred.probs)
}