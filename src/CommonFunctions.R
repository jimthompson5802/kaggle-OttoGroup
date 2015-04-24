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
                improved=character(0), # indicator if score is an improvement
                bestTune=character(0), # optimal hyper-parameters
                tune.grid=character(0), # hyper-parameter tuning grid specified
                model.parms=character(0), # model specific parameters
                comment=character(0), # general comments
                features=character(0),  # features used in training
                stringsAsFactors=FALSE)
    return(df)
}

# function to record model performance
recordModelPerf <- function(df,model=NULL,time.data=NULL,train.df=NULL,score=NA,
                            improved="No", bestTune="", tune.grid="",
                            model.parms="", comment="") {
    # time.data is a proc_time object from system.time() function call
    # bestTune is data frame for optimal model hyper-paramters
    # 
   
    new.row <- data.frame(date.time=as.character(Sys.time()),
                          model=model,
                          user.cpu.time=summary(time.data)["user"],
                          sys.cpu.time=summary(time.data)["system"],
                          elapsed.time=summary(time.data)["elapsed"],
                          num.observations=nrow(train.df),
                          num.features=ncol(train.df),
                          score=score,
                          improved=improved,
                          bestTune=bestTune,
                          tune.grid=tune.grid,
                          model.parms=model.parms,
                          comment=comment,
                          features=paste(names(train.df),collapse=","),
                          stringsAsFactors=FALSE) 
    
    
    return(rbind(df,new.row))
}

#function to flatten one or more rows of data.frame into a string representation
flattenDF <- function(df) {
    #convert data frame to list
    x <- as.list(df)
    
    # convert named list to a string
    return(paste(names(x),as.character(x),sep="=",collapse=","))
}


# function to generate new features - pairwise differences for selcted features
calcPairwiseDiff <- function(selected.features,predictors) {
    # selected.features:  character vector of features to calculate pairwise difference
    # predictors: matrix of predictor attributes
    
    # generate pairwise combinations of selected features
    feature.pairs <- combn(selected.features,2)
    
    #compute pairwise differences
    ll <- lapply(1:ncol(feature.pairs), 
                 function(pair,feature.pairs,predictors) {
                     predictors[,feature.pairs[1,pair]] - predictors[, feature.pairs[2,pair]]},
                 feature.pairs,predictors)
    
    # generate feature names
    names(ll) <- apply(feature.pairs,2,function(x){paste("diff",x[1],x[2],sep=".")})
    
    # return pairwise differences
    return(do.call(cbind,ll))
}

# function to generate new features - pairwise sume for selcted features
calcPairwiseSum <- function(selected.features,predictors) {
    # selected.features:  character vector of features to calculate pairwise difference
    # predictors: matrix of predictor attributes
    
    # generate pairwise combinations of selected features
    feature.pairs <- combn(selected.features,2)
    
    #compute pairwise sum
    ll <- lapply(1:ncol(feature.pairs), 
                 function(pair,feature.pairs,predictors) {
                     predictors[,feature.pairs[1,pair]] + predictors[, feature.pairs[2,pair]]},
                 feature.pairs,predictors)
    
    # generate feature names
    names(ll) <- apply(feature.pairs,2,function(x){paste("sum",x[1],x[2],sep=".")})
    
    # return pairwise differences
    return(do.call(cbind,ll))
}

# partition data set and make predictions in parallel and rejoin predictions
predictInParallel <- function(mdl.fit,input.data,number.workers,only.predictors=FALSE) {
    # mdl.fit: model to use for prediction
    # input.data:  input data set with predictors and response if required
    # number.workers: number of parallel worker tasks to create
    # only.predictors:  Boolean indicating if predictors and response are returned
    
    require(foreach)
    require(doMC)
    
    
    # determine partition size
    partition.size <- ceiling(nrow(input.data) / number.workers)
    
    ll <- NULL
    for(i in seq(1,nrow(input.data),partition.size)) {
        ll[[length(ll)+1]] <- i:min((i+partition.size-1),nrow(input.data))
    }
    
    makePrediction <- function(idx,mdl.fit,input.data,only.predictors) {
        # prepare selected data partition for prediction
        this.data <- prepModelData(input.data[idx,],only.predictors)
        
        id <- input.data[idx,"id"]
        
        pred.probs <- predict(mdl.fit,newdata = this.data$predictors,type = "prob")
        
        return(cbind(id,pred.probs))
    }
    
    # set up for parallel processing
    registerDoMC(cores = number.workers)
    
    prediction <- foreach(idx=ll, .combine=rbind, .packages="randomForest", .verbose=FALSE) %dopar%
                            makePrediction(idx,mdl.fit,input.data,only.predictors)
    
    return(prediction)
}