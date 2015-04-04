###
# Common Functions
###

# Global variables
DATA.DIR <- "./data"

# Evaulation Function
logLossEval <- function(pred.probs, true.class) {
    # pred.probs: N-by-9 matrix of predicted probabilities
    # true.class:  true class designation
    
    # create indicator matrix
    ll <- lapply(true.class,function(class){as.integer(colnames(pred.probs)==class)})
    y <- do.call(rbind,ll)
    colnames(y) <- paste0("Class_",1:9)
    
    return(-sum(log(pred.probs)*y))
    
}
