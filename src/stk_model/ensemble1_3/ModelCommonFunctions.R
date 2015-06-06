###
#  Common Functions for the ensembling of Level 2 models
###

rf1_2Predictions <- function(df) {
    library(randomForest)
    
    # get model specific functions
    source("./src/stk_model/rf1_2/ModelCommonFunctions.R")
    
    # retrive Level 2 model
    load("./src/stk_model/rf1_2/model_rf_2015-06-06_16_41_23.RData")
    
    #save id vector
    id <- df$id
    
    # prep the data for submission
    submission <- prepModelData(df,only.predictors=TRUE)
    
    # get level 1 predictions
    rf1_1.pred.probs <- rf1_1Predictions(df)
    names(rf1_1.pred.probs) <- paste0("rf1_1.",names(rf1_1.pred.probs))
    
    gbm1_1.pred.probs <- gbm1_1Predictions(df)
    names(gbm1_1.pred.probs) <- paste0("gbm1_1.",names(gbm1_1.pred.probs))
    
    # combine level 1 predictions w
    submission$predictors <- cbind(rf1_1.pred.probs,gbm1_1.pred.probs,submission$predictors)
    
    # predict class probabilities
    pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")
    
    pred.probs
    
}

gbm1_2Predictions <- function(df) {
    library(gbm)
    
    # get model specific functions
    source("./src/stk_model/gbm1_2/ModelCommonFunctions.R")
    
    # retrive Level 2 model
    load("./src/stk_model/gbm1_2/model_gbm_one_vs_all_2015-06-06_16_58_43.RData")
    
    #save id vector
    id <- df$id
    
    # prep the data for submission
    submission <- prepModelData(df,only.predictors=TRUE)
    
    # get level 1 predictions
    rf1_1.pred.probs <- rf1_1Predictions(df)
    names(rf1_1.pred.probs) <- paste0("rf1_1.",names(rf1_1.pred.probs))
    
    gbm1_1.pred.probs <- gbm1_1Predictions(df)
    names(gbm1_1.pred.probs) <- paste0("gbm1_1.",names(gbm1_1.pred.probs))
    
    # combine level 1 predictions w
    submission$predictors <- cbind(rf1_1.pred.probs,gbm1_1.pred.probs,submission$predictors)
    
    # predict class probabilities
    ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,submission$predictors)
    names(ll) <- PRODUCT.CLASSES
    
    pred.probs <- do.call(cbind,ll)
    
    data.frame(pred.probs)
}