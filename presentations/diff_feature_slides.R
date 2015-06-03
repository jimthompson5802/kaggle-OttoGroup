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
load(paste0(DATA.DIR,"/diff_train_calib_test.RData"))
load("./eda/selected_features_for_each_class.RData")


# predictor.vars <- setdiff(names(train.raw), c("id","target"))
# 
# train.raw$non.zero.count <- apply(train.raw[,predictor.vars], 1, function(row){sum(row>0)})
# train.raw$count.total <- apply(train.raw[,predictor.vars],1,sum)
# 
# predictor.vars <- c(predictor.vars,"non.zero.count","count.total")

predictor.vars <- c("diff_17.21","diff_17.33","diff_2.43","diff_2.88",
                    "diff_4.34","diff_25.32")

df <- subset(d.train.raw,select=c("id","target",predictor.vars))


# convert to long format 
df <- gather(df, feature, value, one_of(predictor.vars))

df$feature.id <- do.call(rbind,strsplit(as.character(df$feature),"_"))[,2]
df$class.id <- do.call(rbind,strsplit(df$target,"_"))[,2]

png("./presentations/diff_feature_boxplot.png",height=6, width=8, units="in",res=300)
qplot(class.id,value,data=df,geom="boxplot") + 
    facet_wrap(~feature,ncol=3,scales="free_y") +
    xlab("Class")
dev.off()


png("./presentations/diff_feature_density_1.png",height=6, width=8, units="in",res=300)
ggplot(df) + 
    geom_density(aes(x=value, colour=target)) +
    facet_wrap(~feature,ncol=4,scales="free") 
dev.off()



