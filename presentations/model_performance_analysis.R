###
# Analysis of Model Performance data
###

library(plyr)
library(ggplot2)

model.dirs <- list.dirs("./src",recursive=FALSE)
model.dirs <- setdiff(model.dirs,c("skeleton_model","./src/ada_model",
                                   "./src/ensemble_model", "./src/py_model",
                                   "./src/c50_model","./src/sandbox"))

#initialize consolidation data frame
modPerf.df <- data.frame()

for (dir in model.dirs){
    load(paste(dir,"modelPerf.RData",sep="/"))
    df <- data.frame(dir=rep(dir,nrow(modelPerf.df)),modelPerf.df,stringsAsFactors=FALSE)
    modPerf.df <- rbind(modPerf.df,df)
}

# standardize the model names
modPerf.df$model <- ifelse(modPerf.df$model %in% c("svmPoly","svmLinear","svmRadial"),"svm",modPerf.df$model)
modPerf.df$model <- ifelse(modPerf.df$model %in% c("rf","parallel_rf"),"rf",modPerf.df$model)

# calculate summary stats
summary.df <- ddply(modPerf.df,.(model),summarize,
                    count=length(model),
                    total.cpu.minutes=sum(user.cpu.time)/60,
                    total.elapsed.minutes=sum(elapsed.time)/60)

p1 <- ggplot(summary.df) +
        geom_bar(aes(x=reorder(model,-count),y=count), stat="identity",
                 fill="yellow") + 
        annotate("text",x=reorder(summary.df$model,-summary.df$count), y=summary.df$count-1,
                 label=paste(round(summary.df$total.elapsed.minutes,0),"min")) +
    xlab("Model Algorithm") +
    ylab("Number of Training Runs") + 
    ggtitle("Summary of Model Training Runs")

png("./presentations/model_runs_analysis.png",width=8,height=6, units="in",res=300)
print(p1)
dev.off()
