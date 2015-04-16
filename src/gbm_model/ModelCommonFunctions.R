###
#  Model specific common functions
###

prepModelData <- function(df,only.predictors=FALSE){
    # df: raw data
    # returns a list(predictors,response)
    
    # get near zero Vars to eliminate
#     load(paste0(DATA.DIR,"/near_zero_vars.RData"))
    
    predictors <- df[,setdiff(names(df),c("id","target"))]
   
    if (only.predictors) {
        # eliminate unwanted variables
        return(list(predictors=predictors))
    } else {
        # eliminate unwanted variables
        
        response <- factor(df$target)
        
        return(list(predictors=predictors,response=response))
    }

}

