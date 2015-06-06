# get predictions from the Level 1 models
# rf1 model from Level 1
rf1_1Predictions <- function(train.df) {
    library(caret)
    library(randomForest)
    # get level 1 model
    load("./src/stk_model/rf1_1/model_rf_2015-06-06_13_42_31.RData")
    source("./src/stk_model/rf1_1/ModelCommonFunctions.R")
    
    train.data <- prepModelData(train.df)
    
    pred.probs <- predict(mdl.fit,newdata = train.data$predictors,type = "prob")
}

# gbm one vs all from Level 1
gbm1_1Predictions <- function(train.df) {
    library(caret)
    library(gbm)
    # get level 1 model
    load("./src/stk_model/gbm1_1/model_gbm_one_vs_all_2015-06-06_14_13_28.RData")
    source("./src/stk_model/gbm1_1/ModelCommonFunctions.R")
    
    train.data <- prepModelData(train.df)
    
    # for each classes predict probability of for that class
    ll <- lapply(PRODUCT.CLASSES,predictForOneClass,gbm.mdls,train.data$predictors)
    names(ll) <- PRODUCT.CLASSES
    
    pred.probs <- data.frame(do.call(cbind,ll))
}