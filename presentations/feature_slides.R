###
# feature slides for presentation
###

library(ggplot2)
library(tidyr)
library(caret)
library(dplyr)
library(grid)

DATA.DIR <- "./data"

source("./src/CommonFunctions.R")

load(paste0(DATA.DIR,"/train_calib_test.RData"))

predictor.vars <- setdiff(names(train.raw), c("id","target"))

train.raw$non.zero.count <- apply(train.raw[,predictor.vars], 1, function(row){sum(row>0)})
train.raw$count.total <- apply(train.raw[,predictor.vars],1,sum)

predictor.vars <- c(predictor.vars,"non.zero.count","count.total")


# convert to long format 
df <- gather(train.raw, feature, value, one_of(predictor.vars))

df$feature.id <- do.call(rbind,strsplit(as.character(df$feature),"_"))[,2]
df$class.id <- do.call(rbind,strsplit(df$target,"_"))[,2]

png("./presentations/feature_boxplot.png",height=17, width=11, units="in",res=300)
qplot(class.id,value,data=df,geom="boxplot") + 
    facet_wrap(~feature,ncol=8,scales="free_y") +
    xlab("Class")
dev.off()


png("./presentations/feature_boxplot_1.png",height=6, width=8, units="in",res=300)
df2 <- subset(df,feature %in% paste0("feat_",c(1,11,16,25,52,70,87,88)))
qplot(class.id,value,data=df2,geom="boxplot") + 
    facet_wrap(~feature,ncol=4,scales="free_y") +
    xlab("Class")
dev.off()


png("./presentations/feature_density_1.png",height=6, width=8, units="in",res=300)
ggplot(df2) + 
    geom_density(aes(x=value, colour=target)) +
    facet_wrap(~feature,ncol=4,scales="free") 
dev.off()


png("./presentations/feature_synth_1.png",height=6, width=8, units="in",res=300)
df3 <- subset(df,feature %in% c("non.zero.count","count.total"))

p1 <- qplot(class.id,value,data=df3,geom="boxplot") + 
    facet_wrap(~feature,ncol=2,scales="free_y") +
    xlab("Class")

p2 <- ggplot(df3) + 
    geom_density(aes(x=value, colour=target)) +
    facet_wrap(~feature,ncol=4,scales="free") 

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(p1, vp=viewport(layout.pos.row=1, layout.pos.col = 1))
print(p2, vp=viewport(layout.pos.row=2, layout.pos.col = 1))
dev.off()



