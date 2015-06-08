###
#  Model specific common functions
###

# this function prepares data for training or use in predictions
prepModelData <- function(df,only.predictors=FALSE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)

    
    # eliminate unwanted variables
    predictors <- df[,setdiff(names(df),c("id","target"))]
    
    #calculate the number of non-zero count features
    non.zero.count <- apply(predictors,1,function(x){sum(x>0)})
    
    #convert to log(1+feat_x)
    predictors <- log(1 + predictors)
    
    predictors <- cbind(predictors,non.zero.count)
    
   
    if (only.predictors) {

        ans <- list(predictors=predictors)
    } else {
        
        response <- factor(df$target)
        
        ans <- list(predictors=predictors,response=response)
    }

    return(ans)
}

