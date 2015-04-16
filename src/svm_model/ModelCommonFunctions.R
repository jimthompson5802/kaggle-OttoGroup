###
#  Model specific common functions
###

prepModelData <- function(df){
    # df: raw data
    # returns a list(predictors,response)
    
    # get near zero Vars to eliminate
    load(paste0(DATA.DIR,"/near_zero_vars.RData"))
    
    # eliminate unwanted variables
    predictors <- df[,setdiff(names(df),c(nz.vars,"id","target"))]
    response <- factor(df$target)
    
    return(list(predictors=predictors,response=response))
}

