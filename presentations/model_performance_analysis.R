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


# create list of models and their best performance
# eliminate the invalid scores
df3 <- subset(modPerf.df, score > 0.45 & model!="caretEnsemble")

# sort by model and score
df3 <- df3[order(df3$dir,df3$score),]

# get the three best scores for each model
df3 <- ddply(df3,.(dir),head,3)

# parse out the model identifier
ll <- strsplit(df3$dir,split="/")
df3$model.id <- sapply(ll,function(x){x[[3]]},simplify = TRUE)
ll <- strsplit(df3$model.id,split="_")
df3$model.id <- sapply(ll,function(x){x[[1]]},simplify=TRUE)

df3 <- subset(df3,select=c("model.id","num.observations","num.features","score"))
df3 <- df3[order(df3$score),]
df3$model.id <- paste(df3$model.id,1:nrow(df3),sep="_")

p2 <- ggplot(df3) +
    geom_bar(aes(x=reorder(model.id,score),y=score), stat="identity",
             fill="blue") + 
    scale_y_continuous(limits=c(0,2), breaks = seq(0, 2, 0.2)) +
    xlab("Model") +
    ylab("(Better) MLL Score (Worse)") + 
    ggtitle("Model Performance on test.raw Data Set") +
    theme(axis.text.x=element_text(angle=90))

png("./presentations/model_score_analysis.png",width=8,height=6, units="in",res=300)
print(p2)
dev.off()

