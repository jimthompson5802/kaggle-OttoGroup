###
# generate density plots for each feature and class
###

library(tidyr)
library(ggplot2)

source("./src/CommonFunctions.R")

# get training data to analyze
load(paste0(DATA.DIR,"/train_calib_test.RData"))

# remove calib and test to avoid any data leakage
rm(calib.raw,test.raw)


predictor.vars <- setdiff(names(train.raw), c("id","target"))

# convert to long format 
df <- gather(train.raw, feature, value, feat_1:feat_93)

df$feature.id <- do.call(rbind,strsplit(as.character(df$feature),"_"))[,2]
df$class.id <- do.call(rbind,strsplit(df$target,"_"))[,2]

png("./eda/feature_density.png",height=17, width=11, units="in",res=100)
ggplot(df) + 
    geom_density(aes(x=value, colour=target)) +
    facet_wrap(~feature,ncol=8,scales="free_y") 
dev.off()

