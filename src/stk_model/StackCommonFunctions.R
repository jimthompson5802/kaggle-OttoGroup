###
# Level 1 models
###

# run all Level 1 models and aggregate the predictions to create feature set
runAllLevel1Models <- function(train.df) {
    
    # get predictions from Level 1 models
    rf1_1.pred.probs <- rf1_1Predictions(train.df)
    names(rf1_1.pred.probs) <- paste0("rf1_1.",names(rf1_1.pred.probs))
    
    # gbm one vs all model from Level 1
    gbm1_1.pred.probs <- gbm1_1Predictions(train.df)
    names(gbm1_1.pred.probs) <- paste0("gbm1_1.",names(gbm1_1.pred.probs))
    
    # gbm one vs all model from Level 1 - tuned n.minobsinnode
    gbm2_1.pred.probs <- gbm2_1Predictions(train.df)
    names(gbm2_1.pred.probs) <- paste0("gbm2_1.",names(gbm1_1.pred.probs))
    
    # logitboost Level 1 - 
    lgst2_1.pred.probs <- lgst2_1Predictions(train.df)
    names(lgst2_1.pred.probs) <- paste0("lgst2_1.",names(lgst2_1.pred.probs))
    
    new.features <- cbind(rf1_1.pred.probs,
                          gbm1_1.pred.probs,
                          gbm2_1.pred.probs,
                          lgst2_1.pred.probs)
    
    return(new.features)
}


# rf1 model from Level 1
rf1_1Predictions <- function(train.df) {
    library(caret)
    library(randomForest)
    
    model.file <- "./src/stk_model/rf1_1/model_rf_2015-06-07_17_00_56.RData"
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
    
    model.file <- "./src/stk_model/gbm1_1/model_gbm_one_vs_all_2015-06-07_17_31_49.RData"
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

gbm2_1Predictions <- function(train.df) {
    library(caret)
    library(gbm)
    
    model.file <- "./src/stk_model/gbm2_1/model_gbm_one_vs_all_2015-06-07_18_01_10.RData"
    cat("using gbm2_1:",model.file,"\n")
    flush.console()
    
    # get model specific functions
    source("./src/stk_model/gbm2_1/ModelCommonFunctions.R")
    
    # get level 1 model
    load(model.file)
    
    train.data <- prepModelData(train.df)
    
    # for each classes predict probability of for that class
    ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,train.data$predictors)
    names(ll) <- PRODUCT.CLASSES
    
    pred.probs <- data.frame(do.call(cbind,ll))
}

lgst2_1Predictions <- function(train.df) {
    library(caret)
    library(caTools)
    
    model.file <- "./src/stk_model/lgst2_1/model_LogitBoost_2015-06-07_23_03_18.RData"
    cat("using lgst2_1:",model.file,"\n")
    flush.console()
    
    # get model specific functions
    source("./src/stk_model/lgst2_1/ModelCommonFunctions.R")
    
    # get level 1 model
    load(model.file)
    
    train.data <- prepModelData(train.df)
    
    pred.probs <- predict(mdl.fit,newdata = train.data$predictors,type = "prob")
}

###
#  Level 2 models
###

rf1_2Predictions <- function(df) {
    library(randomForest)
    
    model.file <- "./src/stk_model/rf1_2/model_rf_2015-06-07_21_28_23.RData"
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
    
    # get predictions from Level 1 models
    level1.features <- runAllLevel1Models(df)
    
    # combine level 1 predictions with current training data
    submission$predictors <- cbind(level1.features,submission$predictors)
    
    # predict class probabilities
    pred.probs <- predict(mdl.fit,newdata = submission$predictors,type = "prob")
    
    pred.probs
    
}

gbm1_2Predictions <- function(df) {
    library(gbm)
    
    model.file <- "./src/stk_model/gbm1_2/model_gbm_one_vs_all_2015-06-07_18_54_33.RData"
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
    
    # get predictions from Level 1 models
    level1.features <- runAllLevel1Models(df)
    
    # combine level 1 predictions with current training data
    submission$predictors <- cbind(level1.features,submission$predictors)
    
    # predict class probabilities
    ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,submission$predictors)
    names(ll) <- PRODUCT.CLASSES
    
    pred.probs <- do.call(cbind,ll)
    
    data.frame(pred.probs)
}