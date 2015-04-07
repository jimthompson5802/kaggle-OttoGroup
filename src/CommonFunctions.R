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
    
    # adjust probabilities to avoid numerical issues with log() function
    new.probs <- apply(pred.probs,c(1,2),function(elem){max(min(elem,(1-1e-15)),1e-15)})
    
    # get row totals
    row.totals <- apply(new.probs,1,sum)
    
    ans <- -sum(log(new.probs/row.totals)*y)/nrow(new.probs)
    
    if (is.nan(ans) | is.na(ans)) {
        stop("Returning NAN or NA from LogLoss Function.")
    }
        
    return(ans) 
    
    
}

# caret custom model performance function for log-loss
caretLogLossSummary <- function(data,lev,model) {
    out <- logLossEval(data[,3:ncol(data)],data[,"obs"])
    if (is.na(out) | is.nan(out)) {
        stop("Error in LogLoss Function")
    }
    names(out) <- "LogLoss"
    out
}

# function to initialize data frame to collect model performance
createModelPerfDF <- function(...) {
    df <- 
    data.frame(date.time=character(0),
                model=character(0),
                user.cpu.time=numeric(0),
                sys.cpu.time=numeric(0),
                elapsed.time=numeric(0),
                num.train.obs=numeric(0),
                score=numeric(0),
                ...,
                features=character(0),
                stringsAsFactors=FALSE)
    return(df)
}

# function to record model performance
recordModelPerf <- function(df,model,time.data,train.df,score,bestTune) {
    
    new.row <- data.frame(date.time=as.character(Sys.time()),
                   model=model,
                   user.cpu.time=summary(time.data)["user"],
                   sys.cpu.time=summary(time.data)["system"],
                   elapsed.time=summary(time.data)["elapsed"],
                   training.size=nrow(train.df),
                   score=score,
                   bestTune,
                   features=paste(names(train.df),collapse=","),
                   stringsAsFactors=FALSE)
    
    return(rbind(df,new.row))
}