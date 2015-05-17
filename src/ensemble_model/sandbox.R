makeGradientFunction <- function(target,rf2.probs, gbm2.probs,gbm4.probs) {
    
    # adjust probabilities to avoid numerical issues with log() function
    rf2.probs <- apply(rf2.probs,c(1,2),function(elem){max(min(elem,(1-1e-15)),1e-15)})
    rf2.probs <- rf2.probs/apply(rf2.probs,1,sum)
    
    gbm2.probs <- apply(gbm2.probs,c(1,2),function(elem){max(min(elem,(1-1e-15)),1e-15)})
    gbm2.probs <- gbm2.probs/apply(rf2.probs,1,sum)
    
    gbm4.probs <- apply(gbm4.probs,c(1,2),function(elem){max(min(elem,(1-1e-15)),1e-15)})
    gbm4.probs <- gbm4.probs/apply(rf2.probs,1,sum)
    
    function(w) {
        gr <- rep(NA,3)
        
        # create indicator matrix
        ll <- lapply(target,function(class){as.integer(colnames(rf2.probs)==class)})
        y <- do.call(rbind,ll)
        colnames(y) <- paste0("Class_",1:9)
        
        denom <- w[1]*rf2.probs + w[2]*gbm2.probs + w[3]*gbm4.probs
        
        pred.probs <- rf2.probs/denom
        gr[1] <- -sum(pred.probs*y)/nrow(pred.probs)
        
        pred.probs <- gbm2.probs/denom
        gr[2] <- -sum(pred.probs*y)/nrow(pred.probs)
        
        pred.probs <- gbm4.probs/denom
        gr[3] <- -sum(pred.probs*y)/nrow(pred.probs)
        
        return(gr)
    }
}