###
#  Model specific common functions
###

prepModelData <- function(df,only.predictors=FALSE){
    # df: raw data
    # returns a list(predictors,response)
    
    # get near zero Vars to eliminate
#     load(paste0(DATA.DIR,"/near_zero_vars.RData"))
    
    # eliminate unwanted variables
    predictors <- df[,setdiff(names(df),c("id","target"))]
   
    if (only.predictors) {

        ans <- list(predictors=predictors)
    } else {
        
        response <- factor(df$target)
        
        ans <- list(predictors=predictors,response=response)
    }

    return(ans)
}

