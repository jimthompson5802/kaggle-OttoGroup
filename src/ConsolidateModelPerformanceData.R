###
# Consolidate model performance data
###

source("./src/CommonFunctions.R")


model.dirs <- list.dirs("./src",recursive=FALSE)
model.dirs <- setdiff(model.dirs,c("skeleton_model","./src/ada_model",
                                   "./src/ensemble_model",
                                   "./src/c50_model"))

#initialize consolidation data frame
consolidatedModelPerf.df <- data.frame()

for (dir in model.dirs){
    load(paste(dir,"modelPerf.RData",sep="/"))
    df <- data.frame(dir=rep(dir,nrow(modelPerf.df)),modelPerf.df,stringsAsFactors=FALSE)
    consolidatedModelPerf.df <- rbind(consolidatedModelPerf.df,df)
}

write.table(consolidatedModelPerf.df,
            file="./ConsolidatedModelPerformance.tsv",
            sep="\t",row.names=FALSE)