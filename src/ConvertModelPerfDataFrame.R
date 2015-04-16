##
# code to convert to new model Performance data frame
#

WORK.DIR <- "./src/gbm2_model"

# convert current bestTune data to string
bestTune <- apply(modPerf.df[,9:35],1,function(x){as.list(x)})
bestTune <- sapply(bestTune,function(x){paste(names(x),as.character(x),sep="=",collapse=",")},
                   simplify=TRUE)

# model.parms <- apply(modPerf.df["ntree"],1,function(x){as.list(x)})
# model.parms <- sapply(model.parms,function(x){paste(names(x),as.character(x),sep="=",collapse=",")},
#                    simplify=TRUE)


n <- length(bestTune)
modelPerf.df <- data.frame(modPerf.df[,1:8],
                           improved=rep("",n),
                           bestTune=bestTune,
                           tune.grid=rep("",n),
                           model.parms=rep("",n),
                           comment=rep("",n),
                           features=modPerf.df[,"features"],
                           stringsAsFactors=FALSE)

save(modelPerf.df,file=paste0(WORK.DIR,"/modelPerf.RData"))
                           