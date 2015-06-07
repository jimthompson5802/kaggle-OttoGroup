###
# Level 1 models
###
# rf1 model from Level 1
rf1_1Predictions <- function(train.df) {
    library(caret)
    library(randomForest)
    
    model.file <- "./src/stk_model/rf1_1/model_rf_2015-06-06_20_24_58.RData"
    cat("using rf1_1:",model.file,"\n")
    flush.console()
    
    # get model specific functions
    source("./src/stk_model/rf1_1/ModelCommonFunctions.R")
    
    # get level 1 model
    load(model.file)
    
    train.data <- prepModelData(train.df)
    
    pred.probs <- predict(mdl.fit,newdata = train.data$predictors,type = "prob")
}

# gbm one vs all from Level 1
gbm1_1Predictions <- function(train.df) {
    library(caret)
    library(gbm)
    
    model.file <- "./src/stk_model/gbm1_1/model_gbm_one_vs_all_2015-06-06_20_41_10.RData"
    cat("using gbm1_1:",model.file,"\n")
    flush.console()
    
    # get model specific functions
    source("./src/stk_model/gbm1_1/ModelCommonFunctions.R")
    
    # get level 1 model
    load(model.file)

    train.data <- prepModelData(train.df)
    
    # for each classes predict probability of for that class
    ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,train.data$predictors)
    names(ll) <- PRODUCT.CLASSES
    
    pred.probs <- data.frame(do.call(cbind,ll))
}

###
#  Level 2 models
###

rf1_2Predictions <- function(df) {
    library(randomForest)
    
    model.file <- "./src/stk_model/rf1_2/model_rf_2015-06-06_20_50_09.RData"
    cat("using rf1_2:",model.file,"\n")
    flush.console()
    
    # get model specific functions
    source("./src/stk_model/rf1_2/ModelCommonFunctions.R")
    
    # retrive Level 2 model
    load(model.file)
    
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
    
    model.file <- "./src/stk_model/gbm1_2/model_gbm_one_vs_all_2015-06-07_05_52_42.RData"
    cat("using gbm1_2:",model.file,"\n")
    flush.console()
    
    # get model specific functions
    source("./src/stk_model/gbm1_2/ModelCommonFunctions.R")
    
    # retrive Level 2 model
    load(model.file)
    
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