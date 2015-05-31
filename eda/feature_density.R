###
# generate density plots for each feature and class
###

library(tidyr)
library(ggplot2)
library(plyr)

source("./src/CommonFunctions.R")

# get training data to analyze
load(paste0(DATA.DIR,"/train_calib_test.RData"))

# remove calib and test to avoid any data leakage
rm(calib.raw,test.raw)


predictor.vars <- setdiff(names(train.raw), c("id","target"))

train.raw$non.zero.count <- apply(train.raw[,predictor.vars], 1, function(row){sum(row>0)})
train.raw$count.total <- apply(train.raw[,predictor.vars],1,sum)

predictor.vars <- c(predictor.vars,"non.zero.count","count.total")

# convert to long format 
df <- gather(train.raw, feature, value, one_of(predictor.vars))

df$feature.id <- do.call(rbind,strsplit(as.character(df$feature),"_"))[,2]
df$class.id <- do.call(rbind,strsplit(df$target,"_"))[,2]
df$log.value <- log(df$value+1)


png("./eda/feature_density.png",height=17, width=11, units="in",res=300)
ggplot(df) + 
    geom_density(aes(x=value, colour=target)) +
    facet_wrap(~feature,ncol=8,scales="free") 
dev.off()

png("./eda/feature_density_log.png",height=17, width=11, units="in",res=300)
ggplot(df) + 
    geom_density(aes(x=log.value, colour=target)) +
    facet_wrap(~feature,ncol=8,scales="free") 
dev.off()


makeOneVsAllDensity <- function(cls,df) {
    df2 <- df
    df2$class <- ifelse(df2$target==cls,cls,paste0("Not_",cls))
    
    cat("working on",cls,"\n")
    flush.console()
    
    png(paste0("./eda/feature_density_",cls,".png"),height=17, width=11, units="in",res=300)
    p1 <- ggplot(df2) +
        geom_density(aes(x=value, colour=class)) +
        facet_wrap(~feature,ncol=8,scales="free")
    print(p1)
    dev.off()
    
    png(paste0("./eda/feature_density_log_",cls,".png"),height=17, width=11, units="in",res=300)
    p2 <- ggplot(df2) +
        geom_density(aes(x=log.value, colour=class)) +
        facet_wrap(~feature,ncol=8,scales="free")
    print(p2)
    dev.off()
    
}

a_ply(paste0("Class_",1:9),1,makeOneVsAllDensity,df,.progress="none")

