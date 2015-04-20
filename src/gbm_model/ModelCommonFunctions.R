###
#  Model specific common functions
###

# function to calculate feature difference
calcDifference <- function(pair,top.pairs,df) {
    value <- df[,top.pairs[1,pair]] - df[,top.pairs[2,pair]]
    
    return(value)
}


prepModelData <- function(df,only.predictors=FALSE){
    # df: raw data
    # returns a list(predictors,response)
    
    # get near zero Vars to eliminate
#     load(paste0(DATA.DIR,"/near_zero_vars.RData"))
    # get top important variables
    load(paste0(WORK.DIR,"/top_20_variables.RData"))
    gbm.top.20 <- setdiff(gbm.top.20,c("non.zero.count"))
    
    # determine pairwise combination of the top variables
    top.pairs <- combn(gbm.top.20,2)
    
    predictors <- df[,setdiff(names(df),c("id","target"))]
    
    #calculate the number of non-zero count features
    non.zero.count <- apply(predictors,1,function(x){sum(x>0)})

    predictors <- cbind(predictors,non.zero.count)
    
    #compute the pairwise differences
    ll <- lapply(1:ncol(top.pairs),calcDifference,top.pairs,predictors)
    
    # generate feature names
    names(ll) <- apply(top.pairs,2,function(x){paste("diff",x[1],x[2],sep="_")})
    
    new.features <- do.call(cbind,ll)
    
    #augment raw features with new ones
    predictors <- cbind(predictors,new.features)
    
   
    if (only.predictors) {
        # eliminate unwanted variables
        return(list(predictors=predictors))
    } else {
        # eliminate unwanted variables
        
        response <- factor(df$target)
        
        return(list(predictors=predictors,response=response))
    }

}

