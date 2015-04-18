###
# raw data to factor representation
###

# import global variables and common functions
source("./src/CommonFunctions.R")

# get raw data 
load(paste0(DATA.DIR,"/train_calib_test.Rdata"))

# bin data into factors representation
convertToFactors <- function(df) {
    # split off predictors from id and target attributes
    id <- df$id
    target <- df$target
    predictors <- df[setdiff(names(df),c("id","target"))]
    
    predictors <- as.data.frame(lapply(names(predictors),
                         function(this.col,mydf){cut(mydf[,this.col],
                                                     breaks=c(seq(0,20,5),Inf),right=FALSE)},predictors))
    names(predictors) <- setdiff(names(df),c("id","target"))
    
    # recombine back to full data frame
    return(data.frame(id,predictors,target))
}

train.raw <- convertToFactors(train.raw)
calib.raw <- convertToFactors(calib.raw)
test.raw <- convertToFactors(test.raw)

# save
save(train.raw,calib.raw,test.raw,file=paste0(DATA.DIR,"/factors_train_calib_test.RData"))


