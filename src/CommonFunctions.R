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
    data.frame(date.time=character(0),  #date/time of model training run
                model=character(0),     # model type
                user.cpu.time=numeric(0), # user cpu seconds
                sys.cpu.time=numeric(0),  # system cput seconds
                elapsed.time=numeric(0),  # total elapsed time
                num.observations=numeric(0), # number of observations used in training
                num.features=numeric(0),  # number of featurs used in training
                score=numeric(0),  # performance score using hold-out test set
                ...,  # model specific hyper-paramters
                features=character(0),  # features used in training
                stringsAsFactors=FALSE)
    return(df)
}

# function to record model performance
recordModelPerf <- function(df,model,time.data,train.df,score,bestTune,...) {
    # time.data is a proc_time object from system.time() function call
    # bestTune is data frame for optimal model hyper-paramters
    # ... optional other model data to capture
    
    # determine if we have any optional data to capture
    dots <- list(...)
    if (length(dots) > 0) {
        # package other data to record
        dots <- list(...)
        other.data <- data.frame(do.call(cbind,dots), stringsAsFactors=FALSE)
        new.row <- data.frame(date.time=as.character(Sys.time()),
                              model=model,
                              user.cpu.time=summary(time.data)["user"],
                              sys.cpu.time=summary(time.data)["system"],
                              elapsed.time=summary(time.data)["elapsed"],
                              num.observations=nrow(train.df),
                              num.features=ncol(train.df),
                              score=score,
                              bestTune,
                              other.data,
                              features=paste(names(train.df),collapse=","),
                              stringsAsFactors=FALSE) 
    } else {
        # no optional data to capture
        new.row <- data.frame(date.time=as.character(Sys.time()),
                              model=model,
                              user.cpu.time=summary(time.data)["user"],
                              sys.cpu.time=summary(time.data)["system"],
                              elapsed.time=summary(time.data)["elapsed"],
                              num.observations=nrow(train.df),
                              num.features=ncol(train.df),
                              score=score,
                              bestTune,
                              features=paste(names(train.df),collapse=","),
                              stringsAsFactors=FALSE) 
    }
    
    
    return(rbind(df,new.row))
}