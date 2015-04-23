###
#  Model specific common functions
###

# this function prepares data for training or use in predictions
prepModelData <- function(df,only.predictors=FALSE){
    # df: raw data
    # if only.predcitors is TRUE then return list(predictors)
    # if only.predictors is FALSE then return list(predictors,response)
    
    # Perform any model specific data preparations
#     load(paste0(DATA.DIR,"/near_zero_vars.RData"))
    
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

